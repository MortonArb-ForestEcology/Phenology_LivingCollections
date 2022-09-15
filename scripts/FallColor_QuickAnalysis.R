# Script to look at conditions going into the fall
# # - how does our weather compare to past years?
# # - when have we seen start/"peak" color in the past?
# # - maybe get/compare USA-NPN data for IL/IN/WI to look at the wave coming our way



# Looking at Arboretum data
# 1. Get all available Arboretum Data
# 2. Calculate 50% metric --> 50% leaves with color OR 50% canopy with leaves OR 25% & 25%
# 


# Trying to look at MODIS data from google earth engine
###### NOTE: This is very slow because of the fine spatial resolution! we're dealign with half a billion pixels

# https://cran.r-project.org/web/packages/rgee/vignettes/rgee01.html
# https://r-spatial.github.io/rgee/articles/rgee03.html
### # http://www.css.cornell.edu/faculty/dgr2/_static/files/R_html/ex_rgee.html 
### # ^ This one is really good!
library(rgee); library(raster); library(rgdal); library(terra)
# ee_check()
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
# ee_get_earthengine_path()

# Setting up Chicago as 
chicago <- ee$FeatureCollection('users/crollinson/Chicago')$first();
# Map$centerObject(chicago)
chi <- ee$Feature(chicago)
chibuff10 <- chi$buffer(distance=10e3)
chimsk <- ee$Image$constant(1)$clip(chibuff10$geometry())$mask()

# Map$addLayer(chimsk)

# Map$addLayer(chibuff10) # Works!

# addTime <- function(image){image$addBands(image$metadata("system:time_start")).divide(1000*60*60*24*365)}

modpheno <- ee$ImageCollection("MODIS/006/MCD12Q2")

# print(modpheno$select("MidGreendown_1"))

Greendown1 <- modpheno$select('MidGreendown_1');
Greendown1Vis <- list(
  min=11400,
  max=11868,
  palette=c('0f17ff', 'b11406', 'f1ff23')
  )

Greendown1Vis.yday <- list(
  min=1,
  max=365,
  palette=c('0f17ff', 'b11406', 'f1ff23')
)

# Map$addLayer(Greendown1$first(), Greendown1Vis, "Start of Greendown")
# modNames <- modpheno$bandNames()

Greendown1b <- Greendown1$toBands()
ee_print(Greendown1b)

Map$centerObject(chibuff10)
Map$addLayer(Greendown1b$select("2001_01_01_MidGreendown_1"), Greendown1Vis, "Start of Greendown")


greenchi.r <- ee_as_raster(Greendown1b, region=chibuff10$geometry(), via="drive")
class(greenchi.r)
names(greenchi.r)
greenchi.r[[1]]
summary(greenchi.r)
greenchi.r <- brick(greenchi.r)
raster::plot(greenchi.r)

library(terra)
# greenchi.terra <- rast(greenchi.r)
greenchi.r[greenchi.r==0] <- NA
# greenchi.terra[greenchi.terra<180] <- NA
# greenchi.terra[greenchi.terra>365] <- NA
summary(greenchi.r)
plot(greenchi.terra)

yrs <- 2001:2019
for(i in 1:dim(greenchi.r)[3]){
  noffset <- difftime(paste0(yrs[i], "-01-01"), "1970-01-01")
  greenchi.r[[i]] <- greenchi.r[[i]] - as.numeric(noffset)
}
summary(greenchi.r)

greenchi.r[greenchi.r<180] <- NA
greenchi.r[greenchi.r>365] <- NA
summary(greenchi.r)
plot(greenchi.r)

ts.green1 <- apply(as.array(greenchi.r), 3, mean, na.rm=T)

dat1 <- data.frame(year=2001:2019, greendown1 = ts.green1)
lm1 <- lm(greendown1 ~ year, data=dat1)
summary(lm1)
# phenochi.ee <- Greendown1b$mask(chimsk)
# ee_print(phenochi.ee)
# phenochi.ee <- Greendown1$map(function(img){
#   tmp <- img$mask(chimsk)
#   return(tmp)
# })

# phenochi.ee2 <- phenochi.ee$select("MidGreendown1")$toBands
# ee_print(phenochi.ee2)
# test <- Greendown1$first()
# phenochi <- test$mask(chimsk)

# 2001 is first year
(2001-1970)*365
yrs <- as.list(2001:2019)
yrs2 <- lapply(yrs, FUN=function(x){(x-1970)*365})

ee_crs <- phenochi.ee$first()$projection()$getInfo()$crs

pheno.geometry <- phenochi.ee$first()$geometry(proj = ee_crs)$bounds()


# Trying to convert the phenology data to a raster like I'm used to working with 
# phenochi.IMG <- phenochi.ee$toBands()
# class(phenochi.IMG)
# ee_print(phenochi.IMG)
# 
# library(raster); library(terra)
phenochi.rast <- ee_as_raster(phenochi.ee$first(), maxPixels = 5e9) # This is super slow

# Getting regional averages works easily with the extract function
phenochi <- ee_extract(x=modpheno$select(c("MidGreendown_1", "MidGreendown_2")), y=chibuff10, fun=ee$Reducer$mean())
# phenochi2 <- ee_extract(x=modpheno$select("MidGreendown_2"), y=chibuff10, FUN=ee$Reducer$mean())

dat.chipheno <- data.frame(MidGreendown1.raw=as.numeric(phenochi[1,grep("MidGreendown_1", names(phenochi))]),
                           MidGreendown2.raw=as.numeric(phenochi[1,grep("MidGreendown_2", names(phenochi))]))

dat.chipheno$MidGreendown1.date <- as.Date(dat.chipheno$MidGreendown1.raw, origin="1970-01-01")
dat.chipheno$MidGreendown2.date <- as.Date(dat.chipheno$MidGreendown2.raw, origin="1970-01-01")
dat.chipheno$year <- lubridate::year(dat.chipheno$MidGreendown1.date)
dat.chipheno$MidGreendown1.yday <- lubridate::yday(dat.chipheno$MidGreendown1.date)
dat.chipheno$MidGreendown2.yday <- lubridate::yday(dat.chipheno$MidGreendown2.date)


library(ggplot2)
ggplot(data=dat.chipheno) +
  geom_line(aes(x=year, y=MidGreendown1.yday, color="start")) +
  geom_line(aes(x=year, y=MidGreendown2.yday, color="end")) +
  scale_color_manual(values=c("start"="orange2", "end"="red2"))

lm1 <- lm(MidGreendown1.yday~year, data=dat.chipheno)
lm2 <- lm(MidGreendown2.yday~year, data=dat.chipheno)
summary(lm1)
summary(lm2)

meanDown <- phenochi.ee$reduce(ee$Reducer$mean());
meanDown.rast <- ee_as_raster(meanDown)
# test <- ee_extract(meanDown)
print("Mean Senescence", meanDown)
Map$addLayer(meanDown$select('MidGreendown_1_mean'), Greendown1Vis, 'Mean Start of Greendown')

phenodate.ee <- phenochi.ee$map(function(img){
  phenod <- img$select("MidGreendown1")$Date()
  return(phenod)
})


# Tryign with spatial data
phenochi.sf <- ee_extract(x=modpheno$select(c("MidGreendown_1", "MidGreendown_2")), y=chibuff10, fun=ee$Reducer$mean(),  sf=T)
