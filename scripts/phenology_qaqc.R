# -------------------------------------------------------------
# Checking weekly phenology observation data
# -------------------------------------------------------------


# -------------------------------------------------------------
# Set file paths, load libraries etc.
# -------------------------------------------------------------
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages


dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)


path.dat <- file.path(dir.base, "Observing Lists/2018_Quercus")
maps.out <- file.path(path.dat)
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
# -------------------------------------------------------------

# -------------------------------------------------------------
# Access & format the observations
# -------------------------------------------------------------
pheno.lc <- gs_title("Phenology_Observations_GoogleForm_2018")
pheno.lc

# get the data from a particular sheet
dat.raw <- data.frame(gs_read(pheno.lc, ws="Raw Observations"))

# Renaming some columns
names(dat.raw)[grep("OPTIONAL", names(dat.raw))] <- "Notes"
names(dat.raw)[grep("species", names(dat.raw))] <- "Species"

# Coming up with handy groups for our columns
cols.meta <- c("Timestamp", "Email.Address", "Observer", "Date.Observed", "Species", "PlantNumber", "Notes")
pheno.leaf <- names(dat.raw)[grep("leaf", tolower(names(dat.raw)))]
pheno.flower <- names(dat.raw)[grep("flower", tolower(names(dat.raw)))]
pheno.fruit <- names(dat.raw)[grep("fruit", tolower(names(dat.raw)))]


# Setting things to factors
for(i in 1:ncol(dat.raw)){
  if(class(dat.raw[,i])=="character") dat.raw[,i] <- as.factor(dat.raw[,i])
}
summary(dat.raw)
cols.id <- grep("accession", names(dat.raw))

dat.clean <- dat.raw[,c(cols.meta[!cols.meta=="PlantNumber"], pheno.leaf, pheno.flower, pheno.fruit)]
dat.clean$PlantNumber <- as.factor(apply(dat.raw[,cols.id], 1, FUN=function(x) {x[which(!is.na(x))][1]})) # Get the PlantNumber
dat.clean$Timestamp <- strptime(dat.clean$Timestamp, format="%m/%d/%Y %H:%M:%S")
dat.clean$Date.Observed <- as.Date(dat.clean$Date.Observed, format="%m/%d/%Y")
dat.clean <- dat.clean[,c(cols.meta, pheno.leaf, pheno.flower, pheno.fruit)] # Just re-organizing to how I like to see things
summary(dat.clean)

# Get rid of observations that have TEST in them or are before our last phenology training
rows.remove <- c(which(is.na(dat.clean$Species)), grep("TEST", toupper(dat.clean$NOTES)), grep("TEST", toupper(dat.clean$Observer)), which(dat.clean$Date.Observed < as.Date("2018-03-09")))
dat.clean <- dat.clean[!(1:nrow(dat.clean) %in% rows.remove),] # 

dat.clean <- droplevels(dat.clean) # Get rid of unused levels
summary(dat.clean)

#
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
collections <- readOGR("/Volumes/GIS/Collections/Collections_outlines/coll_bndry_master_plan.shp")
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
