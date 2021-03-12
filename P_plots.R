#
# Precipitation indicator plots
# 2021-03-11
#
# # # # # # # #

library(ggplot2)
library(plyr)
library(gridExtra)

# 1.) Quick visualization of study region
plot(demHARWTU,col='cyan',main="Nepal Water Tower Units (WTU) area above 3500m", xlab = "Longitude", ylab = "Latitude", legend=F)
plot(nepal_wtu,add=T,col=ggplot2::alpha('red',0.3))
text(nepal_wtu, nepal_wtu$SUB_NAME,cex=0.6)
legend("topright",c("Nepal Hydrobasins","WTU Area"),fill=ggplot2::alpha(c('red',"cyan"),0.3))

# 2.) Plot P
ggplot(mapa.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=P),colour = "black") +
  ggtitle("Nepal Hydrobasin Precipitation Indicator 2004-2018") +
  theme(plot.title = element_text(size = 6, face = "bold")) +
  ylab("Latitude") + xlab("") + theme_classic() +
  scale_fill_continuous(name="P indicator") +
  coord_fixed()

# 3.) Observe values for PYV, PMV, and PT
p1 = ggplot(mapa.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=PYV),colour = "black") +
  ggtitle("1 - inter-annual variability in precip") +
  theme(plot.title = element_text(size = 6, face = "bold")) +
  ylab("Latitude") + xlab("") + theme_classic() +
  scale_fill_continuous(name="PYV") +
  coord_fixed()
p2 = ggplot(mapa.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=PMV),colour = "black") +
  ggtitle("1 - intra-annual variability in precip") +
  theme(plot.title = element_text(size = 6, face = "bold")) +
  ylab("Latitude") + xlab("Longitude") + theme_classic() +
  scale_fill_continuous(name="PYV") +
  coord_fixed()
p3 = ggplot(mapa.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=PT),colour = "black") +
  ggtitle("Precipitation basin contribution") +
  theme(plot.title = element_text(size = 6, face = "bold")) +
  ylab("Latitude") + xlab("Longitude") + theme_classic() +
  scale_fill_continuous(name="PT") +
  coord_fixed()
# plot all 3
grid.arrange(p1,p2,p3,ncol=1)
