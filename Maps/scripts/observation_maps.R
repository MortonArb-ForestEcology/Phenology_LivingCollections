# ----------------------------------------
# Script to map phenology status and individual observer maps for the Oak Collections 
# Christy Rollinson, crollinson@mortonarb.org
# 3 April, 2017

# Goal:
# 1. Make maps to help phenology observation volunteers find their trees
# 2. Make maps showing the phenological status of the oak collection (goal: update weekly)
# ----------------------------------------



# ----------------------------------------
# Load libraries, base datasets, assign file paths, etc.
# ----------------------------------------
# ---------------------
# Loading useful libraries
# ---------------------
library(raster); library(rgdal); library(rgeow) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages
# ---------------------

# ---------------------
# Setting File paths
# ---------------------
dir.base <- "~/Desktop/Research/Phenology_LivingCollections/Maps/"
setwd(dir.base)

maps.out <- "figures"
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
# ---------------------

# ---------------------
# Loading GIS layers
# # Note: Must be connected to GIS server for this to work
# ---------------------
# Trees! 2016 is the most recent I could find
# Note: there's a LOT of info I don't want
db.cols <- c("sci_name", "sort_scina", "sci_nm3", "type", "trade_nm", "plant_id", "habitat", "coll_id", "coll_nm", "collsuba", "grid_loc", "x_coord", "y_coord")
trees <- readOGR("/Volumes/GIS/Collections/PLANTDB/HDB_2015-05-12.shp")
summary(trees[,db.cols])
names(trees)
plot(trees)


#Collection Boundaries
collections <- readOGR("/Volumes/GIS/Collections/Collections_outlines/coll_bndry_master_plan.shp")
summary(collections)

# Morton Grid system
morton.grid <- readOGR("/Volumes/GIS/Collections/grid_system/adjustedgrid.shp")
class(morton.grid)
# plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray20")

# Water, streams
water <- readOGR("/Volumes/GIS/Collections/hydrology/water.shp")
pools <- readOGR("/Volumes/GIS/Collections/hydrology/wat_pools_seasonal.shp")
streams.perm <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_permanent.shp")
streams.int <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_seasonal.shp")

# Roads, Trails
roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_MASTER_ctrln.shp")
paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
# summary(roads)
# summary(paths)

# Woodland boundarys
woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
woods <- woods[2,] # We only want to worry about the main block; row 1 = King's Grove, row 2= main tract; row 3 = weird patch
# ---------------------



# ---------------------
# Loading Tree Data & Metadata
# ---------------------
pheno.trees <- read.csv("data/Living Collections - Oak Collection Observers - 2017-04-03.csv")
# ---------------------

# ----------------------------------------

# ----------------------------------------
# Subsetting and graphing
# ----------------------------------------
unique(trees$coll_nm)
quercus <- trees[!is.na(trees$coll_nm) & trees$coll_nm=="Quercus",db.cols]
summary(quercus); 
dim(quercus)

# Some quick exploratory graphing of the oak collection
plot(quercus, pch=19, cex=0.5)
plot(woods, add=T, col="green4")
# plot(collections[!is.na(collections$collection) & collections$collection=="Quercus",], add=T, col="indianred")
plot(roads, add=T, lwd=3, col="gray50")
plot(paths, add=T, lwd=2, col="brown", lty="dashed")
plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray20")
plot(quercus, add=T, pch=19, cex=0.5)
plot(quercus[quercus$plant_id %in% pheno.trees$Accession,], add=T, pch=19, col="blue", cex=1.5)

# ----------------------------------------
