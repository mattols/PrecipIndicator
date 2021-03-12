#
# Precipitation indicator functions
# 2021-03-11
#
# # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# LOAD DATA
# nepal_wtu = raster::shapefile(dem_path)
# demHARWTU = raster::raster(dem_path)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
### LOAD FUNCTIONS
## FUNCTION 1
readHAR <- function(har_path,start_year=NULL,monthly=FALSE,ncv=3,layer_name="prcp_"){
  # Function to transform HAR DATA
  # (default name to 'prcp_year')
  # projects to 'WGS84'
  #
  ls_har = list.files(har_path,full.names=T)
  #
  nums = as.numeric(gsub(".*prcp_(.+).nc","\\1",ls_har))
  if(length(nums)==0){stop("File syntax incorrect. Change 'nums' parameter.")}
  sorted_idx = match(sort(nums),nums)
  # get rid of all years before a given year?
  if(!is.null(start_year)){
    # issue with different projection for 2001-2003
    st = which(start_year==sort(nums))
  }else{
    st=1
  }
  ## LOOP
  for (i in st:length(sorted_idx)){
    print(paste("...converting nc file year",i,"of",length(ls_har)), quote=F)
    # read .nc file for year X
    nc_path = ls_har[sorted_idx[i]]
    ncin = ncdf4::nc_open(nc_path)
    ncvar = ncin$var[[ncv]] # 1 & 2 are Lon and Lat
    # NEW
    rlon = raster::raster(nc_path, varname = ncin$var[[1]]$name)
    rlat = raster::raster(nc_path, varname = ncin$var[[2]]$name)
    # Convert to points and match the lat and lons
    plat <- rasterToPoints(rlat)
    plon <- rasterToPoints(rlon)
    lonlat <- cbind(plon[,3], plat[,3])
    # Specify the lonlat as spatial points with projection as long/lat
    lonlat <- SpatialPoints(lonlat, proj4string = CRS("+proj=longlat +datum=WGS84"))
    # My best guess at the proj4 string from the online info at: 
    # Appendix A https://esdynamics.geo.uni-tuebingen.de/wiki/files/modelling/pdf/har_user_guide_V1.2.pdf
    mycrs <- sp::CRS("+proj=lcc +lat_1=30 +lat_2=35 +lat_0=30 +lon_0=87 +units=m +datum=WGS84 +no_defs")
    plonlat <- sp::spTransform(lonlat, CRSobj = mycrs)
    # read data as raster and re-project
    if(monthly){pr = raster::stack(nc_path, varname = ncvar$name)
    }else{pr = raster::raster(nc_path, varname = ncvar$name)}
    # Fix the projection and extent
    projection(pr) <- mycrs
    extent(pr) <- extent(plonlat)
    r <- projectRaster(pr, crs=CRS("+proj=longlat +datum=WGS84"))
    #
    # then save each raster (r) to the stack final stack
    if(i==st){
      har_stk = r
    }else{
      r2 = raster::crop(r, extent(har_stk))
      # har_stk = crop(har_stk, r2)
      har_stk = raster::stack(har_stk,r2)
    }
  }
  ## END
  # re-name stack 
  x = sort(nums)[st:length(nums)]
  if(monthly){x = paste0(rep(x,each=12),"_m",rep(1:12,12))}
  names(har_stk) = paste0(layer_name,x)
  return(har_stk)
}

####################################################################
## FUNCTION 2
crop_convert <- function(har_stk, demHARWTU, month=FALSE, mask=TRUE){
  # convert to volume and crop
  #
  if(month){dt=30.42;hr=730}else{dt=365;hr=8760}
  # convert from mm/hr to km^3/yr and km^3/mo
  # har_stk <- har_stk * (1e-6) * (10**2) * dt * hr
  har_stk <- har_stk * (1e-6) * (10**2) * hr
  #
  if(mask){Pvar = raster::crop(har_stk,demHARWTU) * demHARWTU}else{Pvar = raster::crop(har_stk,demHARWTU)}
  names(Pvar) = names(har_stk)
  return(Pvar)
}

####################################################################
## FUNCTION 3
calculate_P <- function(har_years, har_months, nepal_shp, demHARWTU, create_mapdf=TRUE, save_to=NULL){
  # all equations for precip indicator
  # output: PYV, PYM, PT
  #
  print("...calculating variables",quote=F)
  # get rid of unwanted fields
  nepal_shp <- nepal_shp[, -c(3:4,6)]
  #
  # Call Function 2
  # crop & convert with Function 2
  Py = crop_convert(har_years, demHARWTU, month=FALSE)
  Pm = crop_convert(har_months, demHARWTU, month=TRUE)
  PyBAS = crop_convert(har_years, demHARWTU, month=FALSE, mask=FALSE)
  #
  ## PYV - inter-annual variability in precipitation
  Py_max = calc(Py, fun = max);Py_min = calc(Py, fun = min)
  PYV = 1 - ( (Py_max - Py_min)/Py_max )
  nepal_shp@data <- data.frame(nepal_shp@data, PYV = extract(PYV, nepal_shp, fun=mean, na.rm=TRUE))
  # 
  ## PMV - intra-annual variability in precipitation
  # ave monthly precip sum
  month_idx = unlist(lapply(1:length(names(Pm)), function(x) strsplit(names(Pm),"_m")[[x]][2]))
  Pm_sum = stackApply(Pm, indices = month_idx, fun = sum)
  Pm_max = calc(Pm_sum, fun = max);Pm_min = calc(Pm_sum, fun = min)
  PMV <- 1 - ( (Pm_max - Pm_min)/Pm_max )
  nepal_shp@data <- data.frame(nepal_shp@data, PMV = extract(PMV, nepal_shp, fun=mean, na.rm=TRUE))
  #
  ## Calculate PT
  # average annual WTU precipitation sum divided by basin precip sum
  PWTU = calc(Py, fun = sum)
  PBAS = calc(PyBAS, fun = sum)
  nepal_shp@data <- data.frame(nepal_shp@data, PWTU = extract(PWTU, nepal_shp, fun=sum, na.rm=TRUE),
                               PBAS = extract(PBAS, nepal_shp, fun=sum, na.rm=TRUE))
  nepal_shp@data <- data.frame(nepal_shp@data, PT = nepal_shp@data$PWTU/nepal_shp@data$PBAS)
  #
  # FINAL P
  nepal_shp@data <- data.frame(nepal_shp@data, P = 0.5 * (nepal_shp@data$PYV + nepal_shp@data$PMV) * nepal_shp@data$PT)
  if(create_mapdf){
    mapa <- nepal_shp
    mapa@data$id <- rownames(mapa@data)
    mapa.df     <- ggplot2::fortify(mapa)
    mapa.df     <<- plyr::join(mapa.df,mapa@data, by="id")
    cat("\nNew variable created for plotting: \n mapa.df \n")
  }
  if(!is.null(save_to)){
    rgdal::writeOGR(obj=nepal_shp, dsn=save_to, layer="precip_indicator", driver="ESRI Shapefile")
    cat("\nSaved shapefile: \n   precip_indicator \ndestination:",paste0("'",save_to,"precip_indicator.shp","'"))
  }
  return(nepal_shp)
}
