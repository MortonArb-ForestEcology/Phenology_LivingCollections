library(raster); library(rgdal)
eos <- raster("~/Desktop/eMODIS_East_EOSTAvg0114_v1/eostavg0114")
eos
plot(eos)

il.co <- readOGR("~/Desktop/Research/Chicago_Urban_Forest_Origins/data/IL_BNDY_County/IL_BNDY_County_Py.shp")
summary(il.co)
plot(il.co)

test <- readOGR("~/Desktop/IL_BNDY_County/IL_BNDY_County_Py.shp")
summary(test)

# chicagoland <- il.co[il.co$COUNTY_NAM %in% c("COOK", "LAKE", "KANE", "KENDALL", "DUPAGE", "WILL", "MCHENRY"),]

# chicagoland <- spTransform(chicagoland, CRS("+proj=laea + ellps=WGS84 +units=m +x_0=45 +y_0=0"))
# summary(chicagoland)

eos.chicago <- crop(eos, chicagoland)