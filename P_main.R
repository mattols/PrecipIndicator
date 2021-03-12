#
# Precipitation indicator for Nepal
# based on: https://doi.org/10.1038/s41586-019-1822-y
# last update: 2021-03-11
#
# # # # # # # #

# define: "PATH_TO" locations below

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# DEFINE DATAPATHS
Hy_path     = "PATH_TO/HAR Precip Yearly"
Hm_path     = "PATH_TO/HAR Precip Monthly"
nepal_path  = "PATH_TO/nepal_shp.shp"
dem_path    = "PATH_TO/demHARWTU_new.tif"
#   path to folders containing yearly (Hy) and monthly (Hm) HAR data
#   shapefile of main Nepal watersheds
#   elevation data masked to areas above 3500 (WTU)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# LOAD FUNCTIONS & DATA
source('PATH_TO/P_functions.R', echo=TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# READ HAR 
har_years <- readHAR(Hy_path,start_year=2004)
har_months <- readHAR(Hm_path,start_year=2004,monthly=TRUE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# CALCULATE P
P <- calculate_P(har_years, har_months, nepal_shp, demHARWTU, create_mapdf = TRUE)

# # # # # # # # # # # # # # # # # # # #
# PLOT
source('PATH_TO/PrecipIndicator/P_plots.R', echo=TRUE)
# open P_plots.R to edit/make others

# # # # # # # # # # # # # # # # # # # #
# SAVE
save_path = "PATH_TO_FOLDER/"
P <- calculate_P(har_years, har_months, nepal_shp, demHARWTU, create_mapdf = TRUE,
                 save_to = save_path)
# save .shp for later use
