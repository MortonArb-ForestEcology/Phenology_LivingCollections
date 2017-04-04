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
roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_MASTER_ctrln.shp")
paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
parking <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/parking_lots.shp")
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

# The start poitn was found by trial and error
grid.labs.x <- data.frame(grid.x=seq(323102, by=30.5, length.out=length(89:107)), grid.y=571230, x.lab=89:107)
grid.labs.y <- data.frame(grid.x=323102-30.5, grid.y=seq(571227+30.5, by=30.5, length.out=length(3:15)), y.lab=LETTERS[seq(from=3, to=15)])

# Some quick exploratory graphing of the oak collection
plot(quercus, pch=19, cex=0.5)
plot(woods, add=T, col="green4")
# plot(collections[!is.na(collections$collection) & collections$collection=="Quercus",], add=T, col="indianred")
plot(roads, add=T, lwd=3, col="gray50")
plot(paths, add=T, lwd=2, col="brown", lty="dashed")
plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray20")
plot(quercus, add=T, pch=19, cex=0.5)
plot(quercus[quercus$plant_id %in% pheno.trees$Accession,], add=T, pch=19, col="blue", cex=1.5)
text(x=grid.labs.x$grid.x, y=grid.labs.x$grid.y, labels=grid.labs.x$x.lab, font=2)
text(x=grid.labs.y$grid.x, y=grid.labs.y$grid.y, labels=grid.labs.y$y.lab, font=2)
# text(x=323102, y=571230, labels=c("89"), font=2)
# text(x=323102-30.5, y=571227+30.5, labels=c("C"), font=2)
# text(x=323102-30.5, y=571227+30.5*2, labels=c("D"), font=2)
# text(x=323163 + 30.5*8, y=571230, labels=c("98"), font=2)
323163-30.5*2

# Map for carol
png(file.path(maps.out, paste0("Map_Phenology_OakCollection_Nemec_", Sys.Date(), ".png")), height=8, width=11, unit="in", res=220)
plot(quercus, pch=19, cex=0.5)
plot(woods, add=T, col="green4")
plot(parking, add=T, col="tan")
# plot(collections[!is.na(collections$collection) & collections$collection=="Quercus",], add=T, col="indianred")
plot(roads, add=T, lwd=3, col="gray50")
plot(paths, add=T, lwd=2, col="brown", lty="dashed")
plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray20")
plot(quercus, add=T, pch=19, cex=0.5)
plot(quercus[quercus$plant_id %in% pheno.trees[pheno.trees$Primary.Observer=="Carol Nemec","Accession"],], add=T, pch=19, col="blue", cex=1.5)
text(x=grid.labs.x$grid.x, y=grid.labs.x$grid.y, labels=grid.labs.x$x.lab, font=2)
text(x=grid.labs.y$grid.x, y=grid.labs.y$grid.y, labels=grid.labs.y$y.lab, font=2)
dev.off()

png(file.path(maps.out, paste0("Map_Phenology_OakCollection_Dorrell_", Sys.Date(), ".png")), height=8, width=11, unit="in", res=220)
plot(quercus, pch=19, cex=0.5)
plot(woods, add=T, col="green4")
plot(parking, add=T, col="tan")
# plot(collections[!is.na(collections$collection) & collections$collection=="Quercus",], add=T, col="indianred")
plot(roads, add=T, lwd=3, col="gray50")
plot(paths, add=T, lwd=2, col="brown", lty="dashed")
plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray20")
plot(quercus, add=T, pch=19, cex=0.5)
plot(quercus[quercus$plant_id %in% pheno.trees[pheno.trees$Primary.Observer=="Joan Dorrell","Accession"],], add=T, pch=19, col="red", cex=1.5)
text(x=grid.labs.x$grid.x, y=grid.labs.x$grid.y, labels=grid.labs.x$x.lab, font=2)
text(x=grid.labs.y$grid.x, y=grid.labs.y$grid.y, labels=grid.labs.y$y.lab, font=2)
dev.off()

png(file.path(maps.out, paste0("Map_Phenology_OakCollection_Bigsby_", Sys.Date(), ".png")), height=8, width=11, unit="in", res=220)
plot(quercus, pch=19, cex=0.5)
plot(woods, add=T, col="green4")
# plot(collections[!is.na(collections$collection) & collections$collection=="Quercus",], add=T, col="indianred")
plot(parking, add=T, col="tan")
plot(roads, add=T, lwd=3, col="gray50")
plot(paths, add=T, lwd=2, col="brown", lty="dashed")
plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray20")
plot(quercus, add=T, pch=19, cex=0.5)
plot(quercus[quercus$plant_id %in% pheno.trees[pheno.trees$Primary.Observer=="Brock Bigsby","Accession"],], add=T, pch=19, col="goldenrod3", cex=1.5)
text(x=grid.labs.x$grid.x, y=grid.labs.x$grid.y, labels=grid.labs.x$x.lab, font=2)
text(x=grid.labs.y$grid.x, y=grid.labs.y$grid.y, labels=grid.labs.y$y.lab, font=2)
dev.off()


png(file.path(maps.out, paste0("Map_Phenology_OakCollection_byObserver_", Sys.Date(), ".png")), height=8, width=11, unit="in", res=220)
plot(quercus, pch=19, cex=0.5)
plot(woods, add=T, col="green4")
# plot(collections[!is.na(collections$collection) & collections$collection=="Quercus",], add=T, col="indianred")
plot(roads, add=T, lwd=3, col="gray50")
plot(parking, add=T, col="tan")
plot(paths, add=T, lwd=2, col="brown", lty="dashed")
plot(morton.grid, add=T, lty="dashed", lwd=0.5, col="gray70")
plot(quercus, add=T, pch=19, cex=0.5)
plot(quercus[quercus$plant_id %in% pheno.trees[pheno.trees$Primary.Observer=="Carol Nemec","Accession"],], add=T, pch=19, col="blue", cex=1.5)
plot(quercus[quercus$plant_id %in% pheno.trees[pheno.trees$Primary.Observer=="Joan Dorrell","Accession"],], add=T, pch=19, col="red", cex=1.5)
plot(quercus[quercus$plant_id %in% pheno.trees[pheno.trees$Primary.Observer=="Brock Bigsby","Accession"],], add=T, pch=19, col="goldenrod3", cex=1.5)
text(x=grid.labs.x$grid.x, y=grid.labs.x$grid.y, labels=grid.labs.x$x.lab, font=2)
text(x=grid.labs.y$grid.x, y=grid.labs.y$grid.y, labels=grid.labs.y$y.lab, font=2)
legend(x=grid.labs.x$grid.x[1], y=grid.labs.y$grid.y[1], xjust=0, yjust=0, legend=c("Carol Nemec", "Joan Dorrell", "Brock Bigsby"), col=c("blue", "red", "goldenrod3"), pch=19, cex=1.5)
dev.off()

# ----------------------------------------
