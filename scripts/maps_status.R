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
rm(list=ls())

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
pheno.data <- read.csv("data/observations/Observations_2017_LC_Oaks_TreeShrub_2017-05-11.csv", na.strings="")
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
{
  # Finding just the most recent observation for each tree
  obs.last <- aggregate(pheno.data$date_observed, by=list(pheno.data$id), FUN=max)
  names(obs.last) <- c("id", "last.obs")
  summary(as.factor(obs.last$last.obs))
  summary(obs.last)
  
  # subsetting to observations within the last week of most recent round of obs
  pheno.new <- pheno.data[pheno.data$date_observed>=max(obs.last$last.obs)-7,]
  summary(pheno.new)
  summary(pheno.new$id)
  
  dim(pheno.new)
  # Getting only the most recent obs for each individual
  for(i in unique(pheno.new$id)){
    pheno.new <- pheno.new[pheno.new$id!=i | (pheno.new$id==i & pheno.new$date_observed==obs.last[obs.last$id==i, "last.obs"]),]
  }
  summary(pheno.new)
  dim(pheno.new)
  
  pheno.new[pheno.new$leaf_color_observed=="Y",]
  
  dim(pheno.new)
  
  # No observations of breaking bud right now
  summary(pheno.new)
  pheno.new[pheno.new$leaf_observed=="N",]
  # pheno.new[pheno.new$leaf_increasing_size_observed=="N",]
  
  
  # Making a snapshot update map
  png(file.path(maps.out, paste0("Map_Phenology_OakCollection_Status_", Sys.Date(), ".png")), height=8, width=11, unit="in", res=220)
  plot(quercus, pch=19, cex=0.5, main=paste0("The Morton Arboretum\nPhenology Status of the Oak Collection\nWeek of ", min(pheno.new$date_observed)))
  plot(woods, add=T, col="gray80")
  plot(parking, add=T, col="tan")
  # plot(collections[!is.na(collections$collection) & collections$collection=="Quercus",], add=T, col="indianred")
  plot(roads, add=T, lwd=3, col="tan4")
  plot(paths, add=T, lwd=2, col="brown", lty="dashed")
  plot(morton.grid, add=T, lty="dashed", lwd=0.3, col="black")
  plot(quercus, add=T, pch=19, cex=0.5)
  plot(quercus[quercus$plant_id %in% pheno.new[pheno.new$leaf_breaking_bud_observed=="N","id"],], add=T, pch=19, col="gray50", cex=1.5)
  plot(quercus[quercus$plant_id %in% pheno.new[pheno.new$leaf_breaking_bud_observed=="Y","id"],], add=T, pch=19, col="darkolivegreen3", cex=1.5)
  plot(quercus[quercus$plant_id %in% pheno.new[pheno.new$leaf_observed=="Y","id"],], add=T, pch=19, col="green4", cex=1.5)
  legend(x=grid.oaks.x$grid.x[1], y=grid.oaks.y$grid.y[1], xjust=0, yjust=0, legend=c("Roads", "Trails"), col=c("tan4", "brown"), lty=c("solid", "dashed"), lwd=c(3,2), cex=1.5)
  legend(x=grid.oaks.x$grid.x[1], y=grid.oaks.y$grid.y[4]-30.5/2, xjust=0, yjust=0, legend=c("Dormant", "Bud Burst", "Leaf Out", "Unmonitored"), col=c("gray50", "darkolivegreen3", "green4", "black"), pch=19, cex=1.5)
  # text(x=grid.oaks.x$grid.x, y=grid.oaks.x$grid.y, labels=grid.oaks.x$x.lab, font=2)
  # text(x=grid.oaks.y$grid.x, y=grid.oaks.y$grid.y, labels=grid.oaks.y$y.lab, font=2)
  dev.off()
}
# ----------------------------------------

# ----------------------------------------
# Making prettier maps & animating in changes using ggplot
# ----------------------------------------
{
  library(ggplot2); library(animation)
  
  # Making quercus a data frame
  quercus.df <- data.frame(quercus)
  names(quercus.df)[which(names(quercus.df) %in% c("plant_id", "coords.x1", "coords.x2"))] <- c("id", "x", "y")
  summary(quercus.df)
  
  # Some trees aren't listed in the quercus dataset, so pulling from the total plant database
  db.cols <- c("sci_name", "sort_scina", "sci_nm3", "type", "trade_nm", "plant_id", "habitat", "coll_id", "coll_nm", "collsuba", "grid_loc", "x_coord", "y_coord")
  trees <- readOGR("/Volumes/GIS/Collections/PLANTDB/HDB_2015-05-12.shp")
  summary(trees[,db.cols])
  
  # # We have 10 oaks that aren't in the collectiosn database... why is that???
  trees.obs <- read.csv("data/observations/Living Collections - Oak Collection Observers - 2017-04-03.csv")
  trees.obs[!trees.obs$Accession %in% trees$plant_id,]
  nrow(trees.obs[!trees.obs$Accession %in% trees$plant_id,])
  summary(trees.obs)

  # Seeing what we have observations for that aren't in our observing list
  summary(pheno.data[!pheno.data$id %in% trees.obs,])
  summary(droplevels(pheno.data[!pheno.data$id %in% trees.obs,"id"]))
  
  # Subsetting the massive trees database to just those with phenology observations  
  trees.pheno <- trees[trees$plant_id %in% unique(pheno.data$id),db.cols]
  trees.pheno <- data.frame(trees.pheno)
  names(trees.pheno)[which(names(trees.pheno) %in% c("plant_id", "coords.x1", "coords.x2"))] <- c("id", "x", "y")
  summary(trees.pheno)
  dim(trees.pheno); length(unique(pheno.data$id))
  
  
  # We have lots of trees not in the grid file
  trees.nogrid <- pheno.data[!pheno.data$id %in% trees.pheno$plant_id,]
  summary(trees.nogrid)
  summary(droplevels(trees.nogrid$id))
  length(unique(droplevels(trees.nogrid$id)))
  

  # Merging quercus coordinates into the phenology observations
  pheno.data <- merge(pheno.data, trees.pheno[,c("id", "x", "y")], all.x=T)
  summary(pheno.data)
  # summary(pheno.data[is.na(pheno.data$x),])

  
  # Setting up a dataframe to color-code phenology
  pheno.data[,"leaf_breaking_bud_intensity"] <- as.factor(ifelse(pheno.data$leaf_breaking_bud_intensity=="2", "3-10", paste(pheno.data$leaf_breaking_bud_intensity)))
  pheno.data$leaf_breaking_bud_intensity <- factor(pheno.data$leaf_breaking_bud_intensity, levels=c("0", "3-10", "11-100", "101-1000", "1001-10000", ">10000"))
  summary(pheno.data$leaf_breaking_bud_intensity)
  
  summary(pheno.data$leaf_intensity)
  pheno.data$leaf_intensity <- factor(pheno.data$leaf_intensity, levels=c("0", "<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%"))
  levels(pheno.data$leaf_intensity)
  summary(pheno.data$leaf_intensity)
  
  summary(pheno.data$id)
  
  # filtering out individuals with less than 5 observations
  pheno.remove <- aggregate(pheno.data$x, by=list(pheno.data$id), FUN=length)
  pheno.remove <- pheno.remove[pheno.remove$x < 5,]
    
  pheno.data <- pheno.data[!pheno.data$id %in% pheno.remove$Group.1, ]
  summary(pheno.data$id)
  
  dates.obs <- unique(pheno.data$date_observed)
  dates.obs <- dates.obs[order(dates.obs)]
  saveGIF(
    for(i in seq_along(dates.obs)){
      date.now <- dates.obs[i]
      
      dat.now <- pheno.data[pheno.data$date_observed<=date.now,]
      summary(dat.now)
      
      # Finding just the most recent observation for each tree
      obs.last <- aggregate(dat.now$date_observed, by=list(dat.now$id), FUN=max)
      names(obs.last) <- c("id", "last.obs")
      summary(as.factor(obs.last$last.obs))
      summary(obs.last)
      
      for(j in unique(obs.last$id)){
        dat.now <- dat.now[dat.now$id!=j | (dat.now$id==j & dat.now$date_observed==obs.last[obs.last$id==j, "last.obs"]),]
      }
      summary(dat.now)
    
      pheno.status <- dat.now[,c("id", "x", "y")]
      pheno.status$Phenophase <- ifelse(dat.now$leaf_observed=="Y", "Leaf Out", 
                                        ifelse(dat.now$leaf_breaking_bud_observed=="Y", "Bud Burst", "Dormant"))
      pheno.status$Phenophase <- factor(pheno.status$Phenophase, levels=c("Dormant", "Bud Burst", "Leaf Out"))
      pheno.status$Intensity  <- ifelse(pheno.status$Phenophase=="Leaf Out", dat.now$leaf_intensity, 
                                        ifelse(pheno.status=="Bud Burst", dat.now$leaf_breaking_bud_intensity, NA))
      pheno.status$Intensity  <- factor(pheno.status$Intensity, c("0", "3-10", "11-100", "101-1000", "1001-10000", ">10000", "<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%"))
      summary(pheno.status)
      
      # print(i)
      print(
        ggplot() +
          coord_equal() +
          coord_cartesian(xlim=range(coordinates(quercus)[,1]), ylim=range(coordinates(quercus)[,2])) +
          geom_polygon(data=woods, aes(x=long, y=lat, group=group), fill="darkgreen", alpha=0.5) +
          geom_path(data=roads, aes(x=long, y=lat, group=group), size=8, color="gray80") +
          geom_path(data=paths, aes(x=long, y=lat, group=group), size=5, linetype="dashed", color="brown") +
          # geom_path(data=morton.grid, aes(x=long, y=lat, group=group), linetype="dashed", alpha=0.8) +
          geom_point(data=quercus.df, aes(x=x, y=y), color="black", size=8) +
          geom_point(data=pheno.status, aes(x=x, y=y, color=Phenophase), size=20) +
          scale_color_manual(values=c("gray50", "darkolivegreen3", "green4")) +
          ggtitle(paste0("The Morton Arboretum\nPhenology Status of the Oak Collection\nWeek of ", date.now)) +
          labs(x="x (meters)", y="y (meters)") +
          theme(panel.grid=element_blank(),
                plot.background=element_blank(),
                panel.background=element_blank()) +
          theme(plot.title=element_text(face="bold", hjust=0.5, size=40)) +
          theme(legend.position="bottom",
                legend.title=element_text(size=36, face="bold"),
                legend.key=element_rect(fill=NA),
                legend.text=element_text(size=36))
      )
    }, # End Date Loop
    movie.name=file.path(maps.out, "Phenology_Oaks_2017.gif"), interval=1, nmax=1000, autobrowse=F, autoplay=F, ani.height=1600, ani.width=2400)
}
