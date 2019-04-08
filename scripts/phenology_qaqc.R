# -------------------------------------------------------------
# Checking weekly phenology observation data
# -------------------------------------------------------------


# -------------------------------------------------------------
# Set file paths, load libraries etc.
# -------------------------------------------------------------
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages
library(lubridate)

# Source my cleaning function
source("clean_google_form.R")

dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)

path.dat <- file.path(dir.base, "Observing Lists/2018_Quercus")
maps.out <- file.path(path.dat)
path.gis <- "/Volumes/GIS/Collections" # Path on a Mac
# path.gis <- "Y:/Collections" # Path on a PC
# -------------------------------------------------------------


# -------------------------------------------------------------
# Access & format the observations
# -------------------------------------------------------------
pheno.lc <- gs_title("Phenology_Observations_GoogleForm")
pheno.lc

# get the data from a particular sheet
quercus <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Quercus", dat.yr=lubridate::year(Sys.Date()))
summary(quercus)

acer <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Acer", dat.yr=lubridate::year(Sys.Date()))
summary(acer)

dat.all <- rbind(quercus, acer)
summary(dat.all)

#----------------------------
# For QAQC, get rid of trees that have been removed
#----------------------------
# Querying the googlesheet for missing trees up front to make it easier
sheet.gone <- gs_title("Removed Trees - Phenology_LivingCollections")
sheet.gone # Prints all the metadata

# Get the particular sheet & coerce it into a data frame rather than something special
df.gone <- data.frame(gs_read(sheet.gone, ws="Removed Trees"))
summary(df.gone)

dat.all <- dat.all[!dat.all$PlantNumber %in% df.gone$PlantNumber,]
#----------------------------


#----------------------------
# QAQC sanity checks: Check for the following
#----------------------------
# Observation dates prior to the year we're observing
# Oberservation dates after todays date
# Missing plant numbers
#----------------------------
dat.all[is.na(dat.all$PlantNumber),]
summary(dat.all[dat.all$Species=="Quercus macrocarpa" & dat.all$Observer=="Dorrell",])

yr.wrong <- dat.all[lubridate::year(dat.all$Date.Observed)<lubridate::year(Sys.Date()),"Date.Observed"]
# dat.all[dat.all$Date.Observed==yr.wrong,"Date.Observed"] <- as.Date(paste(2018, month(yr.wrong), day(yr.wrong), sep="-"))

dat.all <- droplevels(dat.all) # Get rid of unused levels
summary(dat.all)
#----------------------------
#----------------------------

#----------------------------
# Check to make sure observers are entering their data right away
#----------------------------
# Check for our list of removed trees 
# Checking to make sure everybody has made observations in the past week
obs.check <- aggregate(dat.all$Date.Observed, by=list(dat.all$Observer), FUN=max)
names(obs.check) <- c("Observer", "Observation.Last")
obs.check
obs.check[obs.check$Observation.Last < Sys.Date()-8,] # Return anybody that's more than 8 days old

# Checking to make sure all trees have observations for the past week
acc.check <- aggregate(dat.all$Date.Observed, by=dat.all[,c("PlantNumber", "Species", "Observer")], FUN=max)
names(acc.check)[which(names(acc.check)=="x")] <- "Observation.Last"
summary(acc.check)
acc.check[acc.check$Observation.Last < Sys.Date()-8,] # Return any tree that hasn't been observed for more than 8 days

#----------------------------

# -------------------------------------------------------------

# -------------------------------------------------------------
# Save Maps & Animations
# -------------------------------------------------------------
# Read in data about all of the trees in the oak collection
oaks.all <- read.csv("/Volumes/GoogleDrive/My Drive/Morton_Data_Misc/2018-03-19_161744393-BRAHMSOnlineData.csv")
summary(oaks.all)


# ---------------
# Read in & formate Arb GIS layers
# ---------------
#Collection Boundaries
collections <- readOGR(file.path(path.gis, "Collections_outlines/coll_bndry_master_plan.shp"))
summary(collections)

# Morton Grid system
# I *think* this is a 100-foot grid (30.48 m)
morton.grid <- readOGR("/Volumes/GIS/Collections/grid_system/adjustedgrid.shp")
class(morton.grid)
# plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray20")

# Water, streams
water <- readOGR("/Volumes/GIS/Collections/hydrology/water.shp")
pools <- readOGR("/Volumes/GIS/Collections/hydrology/wat_pools_seasonal.shp")
streams.perm <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_permanent.shp")
streams.int <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_seasonal.shp")

# Roads, Trails
roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_2011-2020_ctrln.shp")
paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
parking <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/parking_lots.shp")
# summary(roads)
# summary(paths)

# Woodland boundarys
woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
woods <- woods[2,] # We only want to worry about the main block; row 1 = King's Grove, row 2= main tract; row 3 = weird patch

# The start point was found by trial and error
grid.labs.x <- data.frame(grid.x=seq(323102, by=30.5, length.out=length(89:107)), grid.y=571230, x.lab=89:107)
grid.labs.y <- data.frame(grid.x=323102-30.5, grid.y=seq(571227+30.5, by=30.5, length.out=length(3:15)), y.lab=LETTERS[seq(from=3, to=15)])

# summary(grid.labs.x)
labs.x <- SpatialPointsDataFrame(coords = grid.labs.x[,c("grid.x", "grid.y")], grid.labs.x, proj4string=CRS(projection(roads)))
labs.y <- SpatialPointsDataFrame(coords = grid.labs.y[,c("grid.x", "grid.y")], grid.labs.y, proj4string=CRS(projection(roads)))

# Transforming our datalayers to lat/lon to mesh with the tree data
woods <- spTransform(woods, CRS("+proj=longlat"))
roads <- spTransform(roads, CRS("+proj=longlat"))
paths <- spTransform(paths, CRS("+proj=longlat"))
morton.grid <- spTransform(morton.grid, CRS("+proj=longlat"))
labs.x <- spTransform(labs.x, CRS("+proj=longlat"))
labs.y <- spTransform(labs.y, CRS("+proj=longlat"))

labs.x <- data.frame(labs.x)
labs.y <- data.frame(labs.y)
names(labs.x)[4:5] <- c("long", "lat")
names(labs.y)[4:5] <- c("long", "lat")

extent.map <- c(range(labs.x$long, na.rm=T)+c(0.0002,-0.0002), range(labs.y$lat, na.rm=T)+c(0, 0.00005))
grid.crop <- crop(morton.grid, extent.map)
# ---------------
# -------------------------------------------------------------
