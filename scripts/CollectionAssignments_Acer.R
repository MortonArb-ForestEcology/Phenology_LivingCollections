
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages


dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)


path.dat <- file.path(dir.base, "Observing Lists/Acer")
maps.out <- file.path(path.dat)
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
dir.create(path.dat, recursive = T, showWarnings = F)

# Species in the NPN database
npn <- c("macrophyllum", "grandidentatum", "nigrum", "negundo", "spicatum", "platanoides", "rubrum", "glabrum", "saccharinum", "pensylvanicum", "saccharum", "pseudoplatanus", "circinatum")
length(npn)

# spp.keep <- c("palmatum", "miyabei", "freemanii", "campestre")
# Querying the googlesheet for missing trees up front to make it easier
sheet.gone <- gs_title("Removed Trees - Phenology_LivingCollections")
sheet.gone # Prints all the metadata

# Get the particular sheet & coerce it into a data frame rather than something special
df.gone <- data.frame(gs_read(sheet.gone, ws="Removed Trees"))
summary(df.gone)

# ----------------------------
# Narrowing down the phenology observering lists
# ----------------------------
acer <- read.csv("../data/collections/Acer_2019-03-12_190650301-BRAHMSOnlineData.csv")
acer <- acer[grep("Acer", acer$Taxon),] # Get rid of all non-acers
acer$Taxon2 <- acer$Taxon
acer$Taxon2 <- gsub("'", "", acer$Taxon2) # Get rid of '' for names
acer$ProvenanceType <- as.factor(toupper(acer$ProvenanceType))
summary(acer)
dim(acer)

# Removing for double entries
for(PLANT in unique(acer$PlantNumber)){
  acer <- acer[c(which(acer$PlantNumber!=PLANT), which(acer$PlantNumber==PLANT)[1]), ]
}
summary(acer)

unique(acer$Taxon)
length(unique(acer$Taxon)) # 89 unique entries

# Looping through to add genus and species to the acer form
taxa.split <- strsplit(paste(acer$Taxon), split=" ")
for(i in 1:nrow(acer)){
  acer[i,"genus"] <- taxa.split[[i]][1]
  acer[i,"species"] <- taxa.split[[i]][2]
}
summary(as.factor(acer$species))
dim(acer)

# Checking for how many species overlap with NPN
length(which(unique(acer$species) %in% npn))

# Getting rid of a tree that's really far away from everything else 
# I think I/Christy may monitor this one myself for forecasting based on Matt Lobdell's suggestion
trees.exclude <- acer[acer$BgLatitude==max(acer$BgLatitude),"PlantNumber"] 

# Provenance Codes: G = garden source; U = Unknown (for us, often natural recruit); W = wild source accession; Z = propagule from wild source
acer2 <- acer[(acer$species %in% npn | acer$ProvenanceType %in% c("U", "W", "Z")) & !is.na(acer$species) & !acer$PlantNumber %in% trees.exclude,]
summary(acer2)
dim(acer2)
dim(acer)


unique(acer2$Taxon)

unique(acer[!acer$PlantNumber %in% acer2$PlantNumber, "Taxon"])
unique(acer[!acer$PlantNumber %in% acer2$PlantNumber, "species"])
unique(acer2$Taxon)
unique(acer2$species)
summary(as.factor(acer2$species))
# ----------------------------


# ----------------------------
# Doing some clustering and mapping
# ----------------------------
# Compute a euclidean distance matrix
acer.dist <- dist(acer2[,c("BgLongitude", "BgLatitude")])
summary(acer.dist)

# Go through and add in the closest group so that groups are between 20 & 30 trees
max.trees <- 25
min.trees <- 15
n.groups <- round(nrow(acer2)/mean(max.trees, min.trees))

# ---------
# Option 1: Group groups based on distance
# ---------
# k-means clustering
# nrow(acer2)/18
# set.seed(30141038)
# set.seed(30141103)
set.seed(201901) # Good, but we'll cut group

acer.kmeans <- kmeans(acer2[,c("BgLongitude", "BgLatitude")], centers=n.groups*3) # Starting with our desired end point x ~5 so we have small clusters that get added
# acer.kmeans <- kmeans(acer2[,c("BgLongitude", "BgLatitude")], centers=12) # Starting with an average of paired trees
summary(summary(as.factor(acer.kmeans$cluster))) # Looking at average number of members when pairing
acer.kmeans$centers
acer2$group.kmean <- as.factor(acer.kmeans$cluster)

# Next step is doing clustering trying to balance number + distance
dist.kmeans <- as.matrix(dist(acer.kmeans$centers))
summary(dist.kmeans)



# Finding a way to pair clusters that balances number and distance
acer.groups <- data.frame(clust1 = as.factor(1:nrow(acer.kmeans$centers)),
                         acer.kmeans$centers)
for(i in 1:nrow(acer.groups)){
  acer.groups[i, "n.clust1"] <- length(which(acer.kmeans$cluster==i))
}
summary(acer.groups)

# Sort our groupings from most to least
acer.groups <- acer.groups[order(acer.groups$n.clust1, acer.groups$BgLongitude, acer.groups$BgLatitude, decreasing=T),]
head(acer.groups)

# Initialize with cluster 1
clust.new = 1
acer.groups[1, "clust2"] <- clust.new

# Loop through and combine groups
set.seed(201901)
pb <- txtProgressBar(min=0, max=nrow(acer.groups), style=3)
for(i in 1:nrow(acer.groups)){
  
  setTxtProgressBar(pb, i)
  if(i > 1 & !is.na(acer.groups[i, "clust2"])) next # Skip over already assigned groups
  
  # find how many are in this group
  group1 <- acer.groups[i, "clust1"]
  acer.groups[i,"clust2"] <- clust.new
  
  n.group <- sum(acer.groups$n.clust1[which(acer.groups$clust2==clust.new)])
  
  # Find the unassigned rows
  groups.open <- acer.groups[is.na(acer.groups$clust2), "clust1"]
  
  # When we have less than our minimum numebr of trees in the group, add the next closest group
  while(n.group<min.trees & length(groups.open)>0){
    groups.open <- acer.groups[is.na(acer.groups$clust2), "clust1"] # Update this in our loop
    
    # Find the closest trees(s) to the existing groups
    dist.closest <- min(dist.kmeans[groups.open,group1], na.rm=T)
    
    # If the group is further than the average distance among current groups, just skip over it
    grps.now <- which(acer.groups$clust2==clust.new)
    if(length(grps.now)>1 & dist.closest>sd(dist.kmeans[grps.now,grps.now])*1.5) break
    # Make sure we're not adding an oddball group:
    # dist.grp <- 
    # if(dist.closet >=)
    
    group.closest <- groups.open[which(dist.kmeans[groups.open,group1]==dist.closest)]
    
    # If: 
    #  1. adding all groups of closest trees keeps us under the maximum, add them all
    # #  2a. if adding the group with the most trees keeps us under 15, add that one
    # #  2b. add the first one in the list
    if(sum(n.group, acer.groups[acer.groups$clust1 %in% group.closest, "n.clust1"]) <= max.trees) {
      acer.groups[acer.groups$clust1 %in% group.closest, "clust2"] <- clust.new
    } else if(length(group.closest)>1) {
      acer.groups[acer.groups$clust1 %in% group.closest[1], "clust2"] <- clust.new
    } 
    n.group <- sum(acer.groups$n.clust1[which(acer.groups$clust2==clust.new)])
    # group1 <- acer.groups$clust1[which(acer.groups$clust2==clust.new)] # Update group1 with the new cluster
  } # end while
  
  # If we have a less than full group, add the component groups to the closest group
  if(n.group < min.trees & i==nrow(acer.groups)){
    # Find the groups we're lumping
    clust.redo <- acer.groups[acer.groups$clust2==clust.new, "clust1"]
    
    for(j in clust.redo){
      groups.open <- acer.groups[acer.groups$clust1!=j & acer.groups$clust2!=clust.new, "clust1"]
      # if this is our last group, add it to the closest cluster
      group.closest <- groups.open[which(dist.kmeans[groups.open, j]==min(dist.kmeans[groups.open,j], na.rm=T))]
      acer.groups[acer.groups$clust1==j, "clust2"] <- acer.groups[acer.groups$clust1==group.closest, "clust2"]
    }
    
    
  }
  
  clust.new=clust.new+1
} # End loop
acer.groups$clust2 <- as.factor(acer.groups$clust2)
summary(acer.groups)

# This looks pretty good, but needs just a bit of re-tweaking
# Manually moving the tiny south cluster from group 9 to group 10
# acer.groups[acer.groups$clust2==9,]

acer.group2 <- aggregate(acer.groups$n.clust1, by=list(acer.groups$clust2), sum)
names(acer.group2) <- c("group1", "n")
summary(acer.group2)
acer.group2
# clust.kmeans <- hclust(acer.dist)
# plot(clust.kmeans)

# Assigning groups from our re-clustering
for(i in 1:nrow(acer2)){
  group1 <- acer2$group.kmean[i]
  acer2[i, "group1"] <- acer.groups[acer.groups$clust1==paste(group1),"clust2"]
}
acer2$group1 <- as.factor(acer2$group1)
summary(acer2$group1)

groups.n <- aggregate(acer2$BgLatitude, by=list(acer2$group1), FUN=length)
names(groups.n) <- c("group1", "n.trees")

# Getting rid of group 10 because we can't make it cooperate
acer2 <- acer2[acer2$group1!=10,]

acer2$priority <- as.factor(ifelse(acer2$species %in% npn, "NPN", NA))
summary(acer2)

acer.prior <- aggregate(acer2$priority, by=list(acer2$group1), FUN=function(x){length(which(!is.na(x)))})
names(acer.prior) <- c("group1", "n.trees")

# re-assign numbers to align with priority
acer.prior[order(acer.prior$n.trees, decreasing=T),"Priority"] <- 1:nrow(acer.prior)

# Merge priority assignments back into master list
acer2 <- merge(acer2, acer.prior[,c("group1", "Priority")], all.x=T)
summary(acer2)

# ---------

# acer.clust <- hclust(acer.dist) # Perform hierarchical clustering with the distance matrix
# acer2$group <- as.factor(cutree(acer.clust, k=18))
# ---------

# Assiging 
# acer2$group <- as.factor(acer.kmeans$cluster)
summary(acer2)

acer.groups$group.kmean = acer.groups$clust1

# Remove plants that no longer exist
acer[acer$PlantNumber %in% df.gone$PlantNumber, c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA
acer2[acer2$PlantNumber %in% df.gone$PlantNumber, c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA
acer2 <- acer2[!is.na(acer2$BgLatitude),]

summary(acer2)
dim(acer2)


groups.n <- aggregate(acer2$BgLatitude, by=list(acer2$Priority), FUN=length)
names(groups.n) <- c("Priority", "n.trees")


library(ggplot2)
png(file.path(path.dat, "CollectionAssignments_Acer.png"), height=8, width=12, units="in", res=320)
ggplot(data=acer2[,]) +
  ggtitle("Phenology Monitoring Lists:\nAcer Collection") +
  labs(x="Longitude", y="Latitude") +
  coord_equal() +
  facet_wrap(~Priority) +
  geom_point(data=acer[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.1, color="black", alpha=0.2) +
  geom_point(data=acer2[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.25, color="black") +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(Priority))) + 
  # geom_text(data=acer.groups, x=quantile(acer2$BgLongitude, 0.05), y=quantile(acer2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  geom_text(data=groups.n, x=quantile(acer$BgLongitude, 0.95, na.rm=T), y=max(acer$BgLatitude, 0.995, na.rm=T), aes(label=paste0("n = ", n.trees)), fontface="bold") +
  guides(color=F) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        axis.title = element_blank(),
        axis.text = element_blank())
dev.off()

png(file.path(path.dat, "Acer_all.png"), height=6, width=9, units="in", res=320)
ggplot(data=acer2) +
  # ggtitle("Phenology Monitoring Lists 2018:\nAcer Collection") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=acer[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=2.5, color="gray50") +
  geom_point(aes(x=BgLongitude, y=BgLatitude), color="darkgreen", size=5) + 
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()


png(file.path(path.dat, "Acer_NPN.png"), height=8, width=12, units="in", res=320)
ggplot(data=acer2) +
  # ggtitle("Phenology Monitoring Lists 2018:\nAcer Collection") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=acer[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=1, color="black", alpha=0.2) +
  geom_point(aes(x=BgLongitude, y=BgLatitude), size=2, color="black") +
  geom_point(data=acer2[acer2$species %in% npn,], aes(x=BgLongitude, y=BgLatitude, color="NPN Species"), size=3) + 
  scale_color_manual(values="green4") +
  guides(color=F) +
  # geom_text(data=acer.groups, x=quantile(acer2$BgLongitude, 0.05), y=quantile(acer2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()


summary(acer2)

acer.list <- acer2[,c("Priority", "PlantNumber", "Taxon", "Vernacular", "BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")]
acer.list <- acer.list[order(acer.list$Priority, acer.list$Taxon, acer.list$PlantNumber),]
summary(acer.list)
head(acer.list)
write.csv(acer.list[!is.na(acer.list$BgLatitude),], file.path(path.dat, "ObservingLists_Acer.csv"), row.names=F)
# ----------------------------


# ----------------------------
# Making lists for each assignment
# ----------------------------
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages


# dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)


# path.dat <- file.path(dir.base, "Observing Lists/2018_Acer")
# maps.out <- file.path(path.dat)
# path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search

# ---------------
# Read in data about the acer collection
acers.all <- read.csv("../data/collections/Acer_2019-03-12_190650301-BRAHMSOnlineData.csv")
summary(acers.all)

acer.list <- read.csv(file.path(path.dat, "ObservingLists_Acer.csv"))
acer.list$Priority <- as.factor(acer.list$Priority)
summary(acer.list); 
dim(acer.list)


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

extent.map <- c(range(acer.list$BgLongitude, na.rm=T)+c(-0.0005,+0.0005), range(acer.list$BgLatitude, na.rm=T)+c(-0.0005, 0.0005))
grid.crop <- crop(morton.grid, extent.map)

# ---------------



# cols.save <- c("observer", "Taxon", "")

for(ID in unique(acer.list$Priority) ){
  dat.tmp <- acer.list[acer.list$Priority==ID & !is.na(acer.list$BgLatitude), !names(acer.list)=="observer"]
  
  png(file.path(path.dat, paste0("ObservingList_Acer_", stringr::str_pad(ID, 2, side="left", "0"), ".png")), height=8, width=10, units="in", res=120)
  print(
    ggplot() +
      coord_equal(xlim=extent.map[1:2], ylim=extent.map[3:4]) +
      geom_polygon(data=woods, aes(x=long, y=lat, group=group), fill="darkgreen", alpha=0.5) +
      geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=5, color="gray80") +
      geom_path(data=paths, aes(x=long, y=lat, group=group), size=3, linetype="solid", color="brown") +
      geom_path(data=grid.crop, aes(x=long, y=lat, group=group), size=0.25, linetype="dotted", color="gray30") +
      geom_point(data=acers.all, aes(x=BgLongitude, y=BgLatitude), color="black", size=1.5) +
      geom_point(data=acer.list, aes(x=BgLongitude, y=BgLatitude), color="gray50", size=3) +
      geom_point(data=dat.tmp, aes(x=BgLongitude, y=BgLatitude), color="darkgreen", size=5) +
      # geom_text(data=labs.x[2:nrow(labs.x),], aes(x=long, y=lat+0.0002, label=x.lab), color="black", fontface="bold") +
      # geom_text(data=labs.y[2:nrow(labs.y),], aes(x=long+0.0005, y=lat, label=y.lab), color="black", fontface="bold") +
      # scale_color_manual(values=c("gray50", "darkolivegreen3", "green4")) +
      ggtitle(paste0("The Morton Arboretum\nAcer Phenology: ", stringr::str_pad(ID, 2, side="left", "0"))) +
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
  
  write.csv(dat.tmp, file.path(path.dat, paste0("ObservingList_Acer_", stringr::str_pad(ID, 2, side="left", "0"), ".csv")), row.names=F)
}


# ----------------------------
