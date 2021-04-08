# 
# Glacier runoff functions 
# 2021-03-22
#   
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(sp)
library(ncdf4)

##############################################################################
# Function 1
readPyGEM <- function(ncpath, var_nc = "glac_runoff_monthly", dtemporal = c('year','month','all', 'keepall'), 
                      time_range = 2004:2018, ROI=NULL){
  # any variable
  #   dtemporal: read and aggregate data by monthly or yearly averages (or full time period)
  #   time_range: select time period of interest
  #   ROI: specify region of interest (shapefile)
  # open nc file
  ncid = nc_open(ncpath)
  # spatial data
  rgiid = ncvar_get(ncid, "RGIId"); time = ncvar_get(ncid, "time")
  lon = ncvar_get(ncid, "CenLon"); lat = ncvar_get(ncid, "CenLat")
  # find date origin (https://nsidc.org/data/HMA_GL_RCP/versions/1/print/)
  date = as.Date(time, origin = "1999-10-01")
  # make data.frame from variable
  myvar = ncvar_get(ncid, var_nc)
  myvar_t = as.data.frame(t(myvar))
  # close nc file
  nc_close(ncid)
  # filter for range of years
  if(!is.null(time_range)){
    keepers = which(min(time_range) <= format(date,"%Y") & max(time_range) >= format(date,"%Y"))
    date = date[keepers]
    myvar = myvar[keepers,]
    myvar_t = myvar_t[,keepers]
  }
  # create index for dtemporal (for aggregating)
  if (dtemporal=='year'){dtime = format(date,"%Y")}
  if (dtemporal=='month'){dtime = format(date,"%m")}
  if (dtemporal=='all'){dtime = rep(1,length(date))}
  if (dtemporal=='keepall'){dtime = 1:length(date)}
  dtidx = as.numeric(dtime)-(as.numeric(dtime[1]) - 1)
  #
  # aggregate data by dt (month or year)
  if (dtemporal=='year'){
    # if year, sum all months in each year
    aggdata <-aggregate(as.data.frame(myvar), by=list(dtidx), 
                        FUN=sum, na.rm=TRUE);dt = 365
    
  }else{ # average of all months
    aggdata <-aggregate(as.data.frame(myvar), by=list(dtidx), 
                        FUN=mean, na.rm=TRUE);dt = 30.42
  }
  # conversion m^3/month (month) m^3/year (year) -> km^3/time
  # two ways to do this:
  # agg_conv = aggdata * 1e-9 * dt # if original data is monthly mean
  agg_conv = aggdata * 1e-9        # if original data is monthly sum
  agg_mat <- t(agg_conv[,2:ncol(agg_conv)])
  colnames(agg_mat) <- paste0("dt_",unique(dtime))
  df_dt = data.frame(RGIId = rgiid, lon, lat, agg_mat)
  # 
  # Make spatial object from data.frame
  PyGEM_pts = df_dt
  coordinates(PyGEM_pts) <- ~lon+lat
  PyGEM_pts$x = coordinates(PyGEM_pts)[,1]
  PyGEM_pts$y = coordinates(PyGEM_pts)[,2]
  proj4string(PyGEM_pts) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  # 
  # crop to shapefile
  if (!is.null(ROI)){
    PyGEM_pts = PyGEM_pts[ROI,]
  }
  return(PyGEM_pts)
}

##############################################################################
# Function 2
calculate_R <- function(Ryear, Rmonth, nepal_shp, create_mapdf=TRUE, save_to=NULL){
  #
  ## RYV - inter-annual variability in glacier runoff
  # uses max/min of basin-wide average runoff
  basinAnnAve <- over(nepal_shp, Ryear[,grep("dt",names(Ryear))], fn = mean)
  Ry_max = apply(basinAnnAve, 1, max); Ry_min = apply(basinAnnAve, 1, min)  
  nepal_shp@data <- data.frame(nepal_shp@data, RYV = 1 - ( (Ry_max - Ry_min)/Ry_max ))
  # 
  ## RMV - intra-annual variability in glacier runoff
  # ave monthly glacier runoff sum
  basinMoSum <- over(nepal_shp, Rmonth[,grep("dt",names(Rmonth))], fn = sum)
  Rm_max = apply(basinMoSum, 1, max); Rm_min = apply(basinMoSum, 1, min)  
  nepal_shp@data <- data.frame(nepal_shp@data, RMV = 1 - ( (Rm_max - Rm_min)/Rm_max ))
  #
  ## Calculate RT
  # average annual WTU glacier runoff sum divided by average annual WTU precipitation sum
  RWTU = apply(over(nepal_shp, Ryear[,grep("dt",names(Ryear))], fn = sum), 1, sum)
  PWTU = apply(over(nepal_shp, Ryear[,grep("dt",names(Ryear))], fn = sum), 1, sum)
  nepal_shp@data <- data.frame(nepal_shp@data, RT = RWTU/PWTU)
  #
  # FINAL R
  nepal_shp@data <- data.frame(nepal_shp@data, R = 0.5 * (nepal_shp@data$RYV + nepal_shp@data$RMV) * nepal_shp@data$RT)
  if(create_mapdf){
    mapa <- nepal_shp
    mapa@data$id <- rownames(mapa@data)
    mapa.df.R     <- ggplot2::fortify(mapa)
    mapa.df.R     <<- plyr::join(mapa.df.R,mapa@data, by="id")
    cat("\nNew variable created for plotting: \n mapa.df.R \n")
  }
  if(!is.null(save_to)){
    rgdal::writeOGR(obj=nepal_shp, dsn=save_to, layer="runoff_indicator", driver="ESRI Shapefile")
    cat("\nSaved shapefile: \n   runoff_indicator \ndestination:",paste0("'",save_to,"runoff_indicator.shp","'"))
  }
  return(nepal_shp)
}
