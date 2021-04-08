# 
# Glacier runoff indicator 
# 2021-03-22
#
# Data: PyGEM
# Description: https://nsidc.org/data/HMA_GL_RCP/versions/1/print/
# Access: https://nsidc.org/data/HMA_GL_RCP/versions/1
#   
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# load Nepal watershed shp
nepal_shp = raster::shapefile("~/School/Kripa_mentoring/Final_data/nepal_wtu.shp")

# nc path for PyGEM data
ncpath = "~/School/Kripa_mentoring/data/R15--all--ERA-Interim_c2_ba1_100sets_1980_2017-v2.nc"

# load functions
source('~/src/PrecipIndicator/R_functions.R', echo=FALSE)

# yearly averaged data
Ryear = readPyGEM(ncpath,dtemporal='year',ROI=nepal_shp)
head(Ryear)
# monthly averaged
Rmonth = readPyGEM(ncpath,dtemporal='month',ROI=nepal_shp)

# calculate R - runoff indicator
# calculate P first!
R <- calculate_R(Ryear,Rmonth,nepal_shp, P)
# you need to think about how to calculate RT
#   right now the value is 1 for each watershed, see below

# 1.) Plot R
library(ggplot2)
ggplot(mapa.df.R, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=R),colour = "black") +
  ggtitle("Nepal Glacier Runoff Indicator 2004-2018") +
  theme(plot.title = element_text(size = 6, face = "bold")) +
  ylab("Latitude") + xlab("") + theme_classic() +
  scale_fill_continuous(name="R indicator") +
  coord_fixed()
ggplot(mapa.df.R, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=RT),colour = "black") +
  ggtitle("!! Error in RT calculation !!") +
  theme(plot.title = element_text(size = 6, face = "bold")) +
  ylab("Latitude") + xlab("") + theme_classic() +
  scale_fill_continuous(name="P indicator") +
  coord_fixed()

# 2.) RT ERROR
# As you can see there is an error in RT.
# This is because:
#   by default, all glaciers are in the WTU
#   which means that the WTU runoff sum and basin-wide runoff sum are the same
# example:
plot(demHARWTU,col='blue',main="Glacier runoff locations within the Nepal WTUs",legend=F)
plot(Ryear,add=T,pch=20,cex=0.5)
plot(nepal_shp,add=T)
legend("topright",c("Glacier runoff (PyGEM)","WTU Area"),fill=c('black',"blue"))
