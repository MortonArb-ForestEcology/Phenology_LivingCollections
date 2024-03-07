
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages


dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)


path.dat <- file.path(dir.base, "Observing Lists/Ulmus")
maps.out <- file.path(path.dat)
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
dir.create(path.dat, recursive = T, showWarnings = F)

# Species in the NPN database
npn <- c("americana", "crassifolia", "pumila", "rubra", "alata")
length(npn)

# Species requested from Kim Shearer:
kim <- c("crassifolia", "parvifolia", "thomasii", "minor", "parvifolia", "minor x parvifolia")
# U.crassifolia- which is a fall-flowering species and could be used for cultivar development.
# U.parvifolia- which is another fall-flowering species from texas.
# U.thomasii-  is of interest to the curator at the Ames Iowa ARS station. They would like to know about the phenological timing of seed out and possibly have some seed collected for seed storage experiments and to hold some at their repository. 
# U. minor x pravifolia-

# Querying the googlesheet for missing trees up front to make it easier
sheet.gone <- gs_title("Removed Trees - Phenology_LivingCollections")
sheet.gone # Prints all the metadata

# Get the particular sheet & coerce it into a data frame rather than something special
df.gone <- data.frame(gs_read(sheet.gone, ws="Removed Trees"))
summary(df.gone)

# ----------------------------
# Narrowing down the phenology observering lists
# ----------------------------
ulmus <- read.csv("../data/collections/Ulmus_2019-10-14_212755276-BRAHMSOnlineData.csv")
ulmus <- ulmus[grep("Ulmus", ulmus$Taxon),] # Get rid of all non-ulmuss
ulmus$Taxon2 <- ulmus$Taxon
ulmus$Taxon2 <- gsub("'", "", ulmus$Taxon2) # Get rid of '' for names
# ulmus$ProvenanceType <- as.factor(toupper(ulmus$ProvenanceType))
summary(ulmus)
dim(ulmus)

# Removing for double entries
for(PLANT in unique(ulmus$PlantNumber)){
  ulmus <- ulmus[c(which(ulmus$PlantNumber!=PLANT), which(ulmus$PlantNumber==PLANT)[1]), ]
}
summary(ulmus)

unique(ulmus$Taxon)
length(unique(ulmus$Taxon)) # 61 unique entries

# Looping through to add genus and species to the ulmus form
taxa.split <- strsplit(paste(ulmus$Taxon), split=" ")
for(i in 1:nrow(ulmus)){
  ulmus[i,"genus"] <- taxa.split[[i]][1]
  ulmus[i,"species"] <- taxa.split[[i]][2]
}
summary(as.factor(ulmus$species))
length(unique(ulmus$species)) # 34 unique species designations
dim(ulmus)

# Checking for how many species overlap with NPN
length(which(unique(ulmus$species) %in% npn)) # 4 out of 5 -- interestingly, we're missing rubra!


# Do a quick plot of where things are just to make sure there are no clear outliers
ggplot(data=ulmus) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=species)) +
  guides(color=F)

# Setting up a vector of anything we know we want to exclude based off of name or location
# trees.exclude <- ulmus[ulmus$BgLatitude==max(ulmus$BgLatitude),"PlantNumber"] 
trees.exclude <- c()

# If we just do NPN & Kim, that's 44 trees out of 157 potential
# Provenance Codes: G = garden source; U = Unknown (for us, often natural recruit); W = wild source accession; Z = propagule from wild source
# ulmus$ProvenanceType %in% c("U", "W", "Z")
# ulmus2 <- ulmus[(ulmus$species %in% npn | ulmus$species %in% kim ) & !is.na(ulmus$species) & !ulmus$PlantNumber %in% trees.exclude,]
ulmus2 <- ulmus # We're just going to do them all!  I'm duping the df just to make things easier
summary(ulmus2)
dim(ulmus2)
dim(ulmus)


unique(ulmus2$Taxon)

unique(ulmus[!ulmus$PlantNumber %in% ulmus2$PlantNumber, "Taxon"])
unique(ulmus[!ulmus$PlantNumber %in% ulmus2$PlantNumber, "species"])
unique(ulmus2$Taxon)
unique(ulmus2$species)
summary(as.factor(ulmus2$species))
# ----------------------------


# ----------------------------
# Doing some clustering and mapping
# ----------------------------
# Compute a euclidean distance matrix
ulmus.dist <- dist(ulmus2[,c("BgLongitude", "BgLatitude")])
summary(ulmus.dist)

# Go through and add in the closest group so that groups are between 20 & 30 trees
max.trees <- 25
min.trees <- 15
n.groups <- round(nrow(ulmus2)/mean(max.trees, min.trees))

# ---------
# Option 1: Group groups based on distance
# ---------
# k-means clustering
# nrow(ulmus2)/18
# set.seed(30141103) # This looked good, but lets try again
set.seed(201901231207) # This looks like a winner!

ulmus.kmeans <- kmeans(ulmus2[,c("BgLongitude", "BgLatitude")], centers=n.groups*3) # Starting with our desired end point x ~5 so we have small clusters that get added
# ulmus.kmeans <- kmeans(ulmus2[,c("BgLongitude", "BgLatitude")], centers=12) # Starting with an average of paired trees
summary(summary(as.factor(ulmus.kmeans$cluster))) # Looking at average number of members when pairing
ulmus.kmeans$centers
ulmus2$group.kmean <- as.factor(ulmus.kmeans$cluster)

# Next step is doing clustering trying to balance number + distance
dist.kmeans <- as.matrix(dist(ulmus.kmeans$centers))
summary(dist.kmeans)



# Finding a way to pair clusters that balances number and distance
ulmus.groups <- data.frame(clust1 = as.factor(1:nrow(ulmus.kmeans$centers)),
                          ulmus.kmeans$centers)
for(i in 1:nrow(ulmus.groups)){
  ulmus.groups[i, "n.clust1"] <- length(which(ulmus.kmeans$cluster==i))
}
summary(ulmus.groups)

# Sort our groupings from most to least
ulmus.groups <- ulmus.groups[order(ulmus.groups$n.clust1, ulmus.groups$BgLongitude, ulmus.groups$BgLatitude, decreasing=T),]
head(ulmus.groups)

# Initialize with cluster 1
clust.new = 1
ulmus.groups[1, "clust2"] <- clust.new

# Loop through and combine groups
pb <- txtProgressBar(min=0, max=nrow(ulmus.groups), style=3)
for(i in 1:nrow(ulmus.groups)){
  
  setTxtProgressBar(pb, i)
  if(i > 1 & !is.na(ulmus.groups[i, "clust2"])) next # Skip over already assigned groups
  
  # find how many are in this group
  group1 <- ulmus.groups[i, "clust1"]
  ulmus.groups[i,"clust2"] <- clust.new
  
  n.group <- sum(ulmus.groups$n.clust1[which(ulmus.groups$clust2==clust.new)])
  
  # Find the unassigned rows
  groups.open <- ulmus.groups[is.na(ulmus.groups$clust2), "clust1"]
  
  # When we have less than our minimum numebr of trees in the group, add the next closest group
  while(n.group<min.trees & length(groups.open)>0){
    groups.open <- ulmus.groups[is.na(ulmus.groups$clust2), "clust1"] # Update this in our loop
    
    # Find the closest trees(s) to the existing groups
    dist.closest <- min(dist.kmeans[groups.open,group1], na.rm=T)
    
    # If the group is further than the average distance among current groups, just skip over it
    grps.now <- which(ulmus.groups$clust2==clust.new)
    if(length(grps.now)>1 & dist.closest>sd(dist.kmeans[grps.now,grps.now])*1.5) break
    # Make sure we're not adding an oddball group:
    # dist.grp <- 
    # if(dist.closet >=)
    
    group.closest <- groups.open[which(dist.kmeans[groups.open,group1]==dist.closest)]
    
    # If: 
    #  1. adding all groups of closest trees keeps us under the maximum, add them all
    # #  2a. if adding the group with the most trees keeps us under 15, add that one
    # #  2b. add the first one in the list
    if(sum(n.group, ulmus.groups[ulmus.groups$clust1 %in% group.closest, "n.clust1"]) <= max.trees) {
      ulmus.groups[ulmus.groups$clust1 %in% group.closest, "clust2"] <- clust.new
    } else if(length(group.closest)>1) {
      ulmus.groups[ulmus.groups$clust1 %in% group.closest[1], "clust2"] <- clust.new
    } 
    n.group <- sum(ulmus.groups$n.clust1[which(ulmus.groups$clust2==clust.new)])
    # group1 <- ulmus.groups$clust1[which(ulmus.groups$clust2==clust.new)] # Update group1 with the new cluster
  } # end while
  
  # If we have a less than full group, add the component groups to the closest group
  if(n.group < min.trees & i==nrow(ulmus.groups)){
    # Find the groups we're lumping
    clust.redo <- ulmus.groups[ulmus.groups$clust2==clust.new, "clust1"]
    
    for(j in clust.redo){
      groups.open <- ulmus.groups[ulmus.groups$clust1!=j & ulmus.groups$clust2!=clust.new, "clust1"]
      # if this is our last group, add it to the closest cluster
      group.closest <- groups.open[which(dist.kmeans[groups.open, j]==min(dist.kmeans[groups.open,j], na.rm=T))]
      ulmus.groups[ulmus.groups$clust1==j, "clust2"] <- ulmus.groups[ulmus.groups$clust1==group.closest, "clust2"]
    }
    
    
  }
  
  clust.new=clust.new+1
} # End loop
ulmus.groups$clust2 <- as.factor(ulmus.groups$clust2)
summary(ulmus.groups)

# This looks pretty good, but needs just a bit of re-tweaking
# Manually moving the tiny south cluster from group 9 to group 10
# ulmus.groups[ulmus.groups$clust2==9,]

ulmus.group2 <- aggregate(ulmus.groups$n.clust1, by=list(ulmus.groups$clust2), sum)
names(ulmus.group2) <- c("group1", "n")
summary(ulmus.group2)
ulmus.group2
# clust.kmeans <- hclust(ulmus.dist)
# plot(clust.kmeans)

# Assigning groups from our re-clustering
for(i in 1:nrow(ulmus2)){
  group1 <- ulmus2$group.kmean[i]
  ulmus2[i, "group1"] <- ulmus.groups[ulmus.groups$clust1==paste(group1),"clust2"]
}
ulmus2$group1 <- as.factor(ulmus2$group1)
summary(ulmus2$group1)

# Redoing group numbers so they get assigned in order of priority based on our priority species:
# Kim gets first dibs followed by npn
ulmus2$priority <- as.factor(ifelse(ulmus2$species %in% kim, "KIM", ifelse(ulmus2$species %in% npn, "NPN", NA)))
summary(ulmus2)

ulmus.prior <- aggregate(ulmus2$priority, by=list(ulmus2$group1), FUN=function(x){length(which(!is.na(x)))})
names(ulmus.prior) <- c("group1", "n.trees")

# re-assign numbers to align with priority
ulmus.prior[order(ulmus.prior$n.trees, decreasing=T),"Priority"] <- 1:nrow(ulmus.prior)

# Merge priority assignments back into master list
ulmus2 <- merge(ulmus2, ulmus.prior[,c("group1", "Priority")], all.x=T)
summary(ulmus2)
# ---------

# ulmus.clust <- hclust(ulmus.dist) # Perform hierarchical clustering with the distance matrix
# ulmus2$group <- as.factor(cutree(ulmus.clust, k=18))
# ---------

# Assiging 
# ulmus2$group <- as.factor(ulmus.kmeans$cluster)
summary(ulmus2)

ulmus.groups$group.kmean = ulmus.groups$clust1

# Remove plants that no longer exist
ulmus[ulmus$PlantNumber %in% df.gone$PlantNumber, c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA
ulmus2[ulmus2$PlantNumber %in% df.gone$PlantNumber, c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA
ulmus2 <- ulmus2[!is.na(ulmus2$BgLatitude),]
summary(ulmus2)
dim(ulmus2)

groups.n <- aggregate(ulmus2$BgLatitude, by=list(ulmus2$Priority), FUN=length)
names(groups.n) <- c("Priority", "n.trees")


# library(ggplot2)
png(file.path(path.dat, "CollectionAssignments_Ulmus.png"), height=8, width=12, units="in", res=320)
ggplot(data=ulmus2) +
  ggtitle("Phenology Monitoring Lists:\nUlmus Collection") +
  labs(x="Longitude", y="Latitude") +
  coord_equal() +
  facet_wrap(~Priority) +
  geom_point(data=ulmus[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.1, color="black", alpha=0.2) +
  geom_point(data=ulmus2[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.25, color="black") +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(Priority))) + 
  # geom_text(data=ulmus.groups, x=quantile(ulmus2$BgLongitude, 0.05), y=quantile(ulmus2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  geom_text(data=groups.n, x=quantile(ulmus$BgLongitude, 0.95, na.rm=T), y=max(ulmus$BgLatitude, 0.995, na.rm=T), aes(label=paste0("n = ", n.trees)), fontface="bold") +
  guides(color=F) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        axis.title = element_blank(),
        axis.text = element_blank())
dev.off()

png(file.path(path.dat, "Ulmus_all.png"), height=6, width=9, units="in", res=320)
ggplot(data=ulmus2) +
  # ggtitle("Phenology Monitoring Lists 2018:\nUlmus Collection") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=ulmus[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=2.5, color="gray50") +
  geom_point(aes(x=BgLongitude, y=BgLatitude), color="darkgreen", size=5) + 
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()

nrow(ulmus2[ulmus2$species %in% npn,])

png(file.path(path.dat, "Ulmus_NPN.png"), height=8, width=12, units="in", res=320)
ggplot(data=ulmus2) +
  # ggtitle("Phenology Monitoring Lists 2018:\nUlmus Collection") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=ulmus[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=1, color="black", alpha=0.2) +
  geom_point(aes(x=BgLongitude, y=BgLatitude), size=2, color="black") +
  geom_point(data=ulmus2[ulmus2$species %in% npn,], aes(x=BgLongitude, y=BgLatitude, color="NPN Species"), size=3) + 
  scale_color_manual(values="blue2") +
  guides(color=F) +
  # geom_text(data=ulmus.groups, x=quantile(ulmus2$BgLongitude, 0.05), y=quantile(ulmus2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()

png(file.path(path.dat, "Ulmus_Shearer.png"), height=8, width=12, units="in", res=320)
ggplot(data=ulmus2) +
  # ggtitle("Phenology Monitoring Lists 2018:\nUlmus Collection") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=ulmus[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=1, color="black", alpha=0.2) +
  geom_point(aes(x=BgLongitude, y=BgLatitude), size=2, color="black") +
  geom_point(data=ulmus2[ulmus2$species %in% kim,], aes(x=BgLongitude, y=BgLatitude, color="NPN Species"), size=3) + 
  scale_color_manual(values="green3") +
  guides(color=F) +
  # geom_text(data=ulmus.groups, x=quantile(ulmus2$BgLongitude, 0.05), y=quantile(ulmus2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()


summary(ulmus2)

ulmus.list <- ulmus2[,c("Priority", "PlantNumber", "Taxon", "Vernacular", "BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")]
ulmus.list <- ulmus.list[order(ulmus.list$Priority, ulmus.list$Taxon, ulmus.list$PlantNumber),]
names(ulmus.list)[1] <- "Obs.List" # Renaming the first column to be consistent across lists
summary(ulmus.list)
head(ulmus.list)
write.csv(ulmus.list[!is.na(ulmus.list$BgLatitude),], file.path(path.dat, "ObservingLists_Ulmus.csv"), row.names=F)
# ----------------------------


# ----------------------------
# Making lists for each assignment
# ----------------------------
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages


# dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)


# path.dat <- file.path(dir.base, "Observing Lists/2018_Ulmus")
# maps.out <- file.path(path.dat)
# path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search

# ---------------
# Read in data about the ulmus collection
ulmuss.all <- read.csv("../data/collections/Ulmus_2019-10-14_212755276-BRAHMSOnlineData.csv")
summary(ulmuss.all)

ulmus.list <- read.csv(file.path(path.dat, "ObservingLists_Ulmus.csv"))
ulmus.list$Obs.List <- as.factor(ulmus.list$Obs.List)
summary(ulmus.list); 
dim(ulmus.list)


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
# grid.labs.x <- data.frame(grid.x=seq(323102, by=30.5, length.out=length(89:107)), grid.y=571230, x.lab=89:107)
# grid.labs.y <- data.frame(grid.x=323102-30.5, grid.y=seq(571227+30.5, by=30.5, length.out=length(3:15)), y.lab=LETTERS[seq(from=3, to=15)])

# summary(grid.labs.x)
# labs.x <- SpatialPointsDataFrame(coords = grid.labs.x[,c("grid.x", "grid.y")], grid.labs.x, proj4string=CRS(projection(roads)))
# labs.y <- SpatialPointsDataFrame(coords = grid.labs.y[,c("grid.x", "grid.y")], grid.labs.y, proj4string=CRS(projection(roads)))

# Transforming our datalayers to lat/lon to mesh with the tree data
woods <- spTransform(woods, CRS("+proj=longlat"))
roads <- spTransform(roads, CRS("+proj=longlat"))
paths <- spTransform(paths, CRS("+proj=longlat"))
morton.grid <- spTransform(morton.grid, CRS("+proj=longlat"))
# labs.x <- spTransform(labs.x, CRS("+proj=longlat"))
# labs.y <- spTransform(labs.y, CRS("+proj=longlat"))

# labs.x <- data.frame(labs.x)
# labs.y <- data.frame(labs.y)
# names(labs.x)[4:5] <- c("long", "lat")
# names(labs.y)[4:5] <- c("long", "lat")

extent.map <- c(range(ulmus.list$BgLongitude, na.rm=T)+c(-0.0005,+0.0005), range(ulmus.list$BgLatitude, na.rm=T)+c(-0.0005, 0.0005))
grid.crop <- crop(morton.grid, extent.map)

# ---------------



# cols.save <- c("observer", "Taxon", "")

for(ID in unique(ulmus.list$Obs.List) ){
  dat.tmp <- ulmus.list[ulmus.list$Obs.List==ID & !is.na(ulmus.list$BgLatitude), !names(ulmus.list)=="observer"]
  
  png(file.path(path.dat, paste0("ObservingList_Ulmus_", stringr::str_pad(ID, 2, side="left", "0"), ".png")), height=8, width=10, units="in", res=120)
  print(
    ggplot() +
      coord_equal(xlim=extent.map[1:2], ylim=extent.map[3:4]) +
      geom_polygon(data=woods, aes(x=long, y=lat, group=group), fill="darkgreen", alpha=0.5) +
      geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=5, color="gray80") +
      geom_path(data=paths, aes(x=long, y=lat, group=group), size=3, linetype="solid", color="brown") +
      geom_path(data=grid.crop, aes(x=long, y=lat, group=group), size=0.25, linetype="dotted", color="gray30") +
      geom_point(data=ulmuss.all, aes(x=BgLongitude, y=BgLatitude), color="black", size=1.5) +
      geom_point(data=ulmus.list, aes(x=BgLongitude, y=BgLatitude), color="gray50", size=3) +
      geom_point(data=dat.tmp, aes(x=BgLongitude, y=BgLatitude), color="darkgreen", size=5) +
      # geom_text(data=labs.x[2:nrow(labs.x),], aes(x=long, y=lat+0.0002, label=x.lab), color="black", fontface="bold") +
      # geom_text(data=labs.y[2:nrow(labs.y),], aes(x=long+0.0005, y=lat, label=y.lab), color="black", fontface="bold") +
      # scale_color_manual(values=c("gray50", "darkolivegreen3", "green4")) +
      ggtitle(paste0("The Morton Arboretum\nUlmus Phenology: ", stringr::str_pad(ID, 2, side="left", "0"))) +
      # labs(x="x (meters)", y="y (meters)") +
      theme(panel.grid=element_blank(),
            plot.background=element_blank(),
            panel.background=element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank()) +
      theme(plot.title=element_text(face="bold", hjust=0.5, size=24)) +
      theme(legend.position="bottom",
            legend.title=element_text(size=36, face="bold"),
            legend.key=element_rect(fill=NA),
            legend.text=element_text(size=36))
  )
  dev.off()
  
  write.csv(dat.tmp, file.path(path.dat, paste0("ObservingList_Ulmus_", stringr::str_pad(ID, 2, side="left", "0"), ".csv")), row.names=F)
}


# ----------------------------
