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
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(lubridate); library(stringr)
library(ggplot2); library(grid) # graphing packages
# ---------------------

# ---------------------
# Setting File paths
# ---------------------
dir.base <- "~/Desktop/Research/Phenology_LivingCollections/"
setwd(dir.base)

maps.out <- "figures/maps_status"
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
# ---------------------



# ---------------------
# Loading Tree Data & Metadata
# ---------------------
pheno.data <- read.csv("data/observations/Observations_2017_LC_Oaks_TreeShrub_2017-04-04.csv", na.strings="")
pheno.data <- pheno.data[!is.na(pheno.data$genus),]
pheno.data$date_observed <- as.Date(pheno.data$date_observed)
pheno.data$date_entered <- as.Date(pheno.data$date_entered)
summary(pheno.data)

# -----------------
# Data Cleaning
# -----------------
# Somehow I screwed up and entered leaf_falling_intensity as a number
# vars.percent <- c("leaf_intensity", "leaf_increasing_size_intensity", "leaf_color_intensity", "leaf_falling_intensity", "flower_open_intensity", "fruit_ripe_intensity", "fruit_drop_intensity")
# for(i in vars.percent){
#   
# }

# Note: Leaf should not be listed as colored if it is still ont he tree from the fall
pheno.data[,"leaf_color_observed"] <- as.factor(ifelse(pheno.data$leaf_color_observed=="Y" & month(pheno.data$date_observed)<=4, "N", paste(pheno.data$leaf_color_observed)))
pheno.data$leaf_color_intensity <- ifelse(pheno.data$leaf_color_observed=="N", 0, pheno.data$leaf_color_intensity)

summary(pheno.data)
# -----------------
# ---------------------


# ---------------------
# Loading GIS layers
# # Note: Must be connected to GIS server for this to work
# ---------------------
{
  # ------------
  # Trees! 2016 is the most recent I could find
  # Note: there's a LOT of info I don't want
  # Note: We're just going to create a local subset of the oaks so we don't have to load everything all the time
  # ------------
  {
    # db.cols <- c("sci_name", "sort_scina", "sci_nm3", "type", "trade_nm", "plant_id", "habitat", "coll_id", "coll_nm", "collsuba", "grid_loc", "x_coord", "y_coord")
    # trees <- readOGR("/Volumes/GIS/Collections/PLANTDB/HDB_2015-05-12.shp")
    # summary(trees[,db.cols])
    # names(trees)
    # # plot(trees)
    # 
    # # Subsetting just the oak collection
    # unique(trees$coll_nm)
    # quercus <- trees[!is.na(trees$coll_nm) & trees$coll_nm=="Quercus",db.cols]
    # summary(quercus); 
    # dim(quercus)
    # 
    # writeOGR(quercus, "data/Collection_Quercus", "Collection_Quercus", driver="ESRI Shapefile")
  }
  
  quercus <- readOGR("data/spatial/Collection_Quercus/quercus.shp")
  summary(quercus)
  # ------------
  
  
  
  #Collection Boundaries
  collections <- readOGR("/Volumes/GIS/Collections/Collections_outlines/coll_bndry_master_plan.shp")
  summary(collections)
  
  # Morton Grid system
  # I *think* this is a 100-foot grid (30.48 m)
  morton.grid <- readOGR("/Volumes/GIS/Collections/grid_system/adjustedgrid.shp")
  class(morton.grid)
  # plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray20")
  
  # The start point for the oak collection was found by trial and error
  grid.oaks.x <- data.frame(grid.x=seq(323102, by=30.5, length.out=length(89:107)), grid.y=571230, x.lab=89:107)
  grid.oaks.y <- data.frame(grid.x=323102-30.5, grid.y=seq(571227+30.5, by=30.5, length.out=length(3:15)), y.lab=LETTERS[seq(from=3, to=15)])
  
  
  # Water, streams
  water <- readOGR("/Volumes/GIS/Collections/hydrology/water.shp")
  pools <- readOGR("/Volumes/GIS/Collections/hydrology/wat_pools_seasonal.shp")
  streams.perm <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_permanent.shp")
  streams.int <- readOGR("/Volumes/GIS/Collections/hydrology/wat_streams_seasonal.shp")
  
  # Roads, Trails
  roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_MASTER_ctrln.shp")
  paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
  parking <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/parking_lots.shp")
  # summary(roads)
  # summary(paths)
  
  # Woodland boundarys
  woods <- readOGR("/Volumes/GIS/Collections/Natural Resources Management/2008 vegetative cover type/Woodland.shp")
  woods <- woods[2,] # We only want to worry about the main block; row 1 = King's Grove, row 2= main tract; row 3 = weird patch
}
# ---------------------

# ----------------------------------------



# ----------------------------------------
# Subsetting and graphing
# ----------------------------------------

# Finding just the most recent observation for each tree
obs.last <- aggregate(pheno.data$date_observed, by=list(pheno.data$id), FUN=max)
summary(as.factor(obs.last$x))

# subsetting to just the most recent window with lots of observations
pheno.new <- pheno.data[pheno.data$date_observed>=as.Date("2017-03-27"),]
summary(pheno.new)
pheno.new[pheno.new$leaf_color_observed=="Y",]

dim(pheno.new)

# No observations of breaking bud right now
summary(pheno.data[pheno.data$leaf_breaking_bud_observed=="Y",])


png(file.path(maps.out, paste0("Map_Phenology_OakCollection_Status_", Sys.Date(), ".png")), height=8, width=11, unit="in", res=220)
plot(quercus, pch=19, cex=0.5, main=paste0("The Morton Arboretum\nPhenology Status of the Oak Collection\nWeek of ", min(pheno.new$date_observed)))
plot(woods, add=T, col="green4")
plot(parking, add=T, col="tan")
# plot(collections[!is.na(collections$collection) & collections$collection=="Quercus",], add=T, col="indianred")
plot(roads, add=T, lwd=3, col="tan4")
plot(paths, add=T, lwd=2, col="brown", lty="dashed")
plot(morton.grid, add=T, lty="dashed", lwd=0.3, col="black")
plot(quercus, add=T, pch=19, cex=0.5)
plot(quercus[quercus$plant_id %in% pheno.new[pheno.new$leaf_breaking_bud_observed=="N","id"],], add=T, pch=19, col="gray50", cex=1.5)
plot(quercus[quercus$plant_id %in% pheno.new[pheno.new$leaf_breaking_bud_observed=="Y","id"],], add=T, pch=19, col="darkolivegreen1", cex=1.5)
legend(x=grid.oaks.x$grid.x[1], y=grid.oaks.y$grid.y[1], xjust=0, yjust=0, legend=c("Roads", "Trails"), col=c("tan4", "brown"), lty=c("solid", "dashed"), lwd=c(3,2), cex=1.5)
legend(x=grid.oaks.x$grid.x[1], y=grid.oaks.y$grid.y[4]-30.5/2, xjust=0, yjust=0, legend=c("Dormant", "Bud Burst", "Unmonitored"), col=c("gray50", "darkolivegreen1", "black"), pch=19, cex=1.5)
# text(x=grid.oaks.x$grid.x, y=grid.oaks.x$grid.y, labels=grid.oaks.x$x.lab, font=2)
# text(x=grid.oaks.y$grid.x, y=grid.oaks.y$grid.y, labels=grid.oaks.y$y.lab, font=2)
dev.off()
# ----------------------------------------
