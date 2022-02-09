
library(raster); library(rgdal); library(rgeos); library(dplyr); library(factoextra); # spatial analysis packages
library(ggplot2); library(grid);library(gridExtra); # graphing packages

dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
#setwd(dir.base)

path.dat <- file.path(dir.base, "Observing Lists")
path.out <- "Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Observing Lists"
.maps.out <- file.path(path.dat)
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
dir.create(path.dat, recursive = T, showWarnings = F)

# Species in the NPN database
npn <- c("americana", "cordata", "tomentosa")
length(npn)
spp.keep <- c("amurensis", "dasystyla", "euchlora", "europaea", "flavescens", "japonica", "mandshurica","mongolica", "oliveri", "orbicularis","paucicostata","platyphyllos", "zamoyskiana")
 


 #################I don't think I need this because there are no missing Tilia, and I have alreay subset out the Tilia we will be observing
 # Querying the googlesheet for missing trees up front to make it easier
#sheet.gone <- gs_title("Removed Trees - Phenology_LivingCollections")
#sheet.gone # Prints all the metadata

# Get the particular sheet & coerce it into a data frame rather than something special
#df.gone <- data.frame(gs_read(sheet.gone, ws="Removed Trees"))
#summary(df.gone)

# ----------------------------
# Narrowing down the phenology observering lists
# ----------------------------
tilia <- read.csv("../data/ObservingLists/Tilia_v_1.csv")
#acer <- acer[grep("Acer", acer$Taxon),] # Get rid of all non-acers
#acer$Taxon2 <- acer$Taxon
#acer$Taxon2 <- gsub("'", "", acer$Taxon2) # Get rid of '' for names
#acer$ProvenanceType <- as.factor(toupper(acer$ProvenanceType))
#summary(acer)
#dim(acer)

###This didn't work
#tilia<- read.csv("/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Observing Lists/Tilia/Tilia_1.csv")

#tilia<- read_sheet("/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Observing Lists/Tilia/Tilia_all")

head(tilia)

# Removing for double entries

for(PLANT in unique(tilia$PlantNumber)){
  tilia <- tilia[c(which(tilia$PlantNumber!=PLANT), which(tilia$PlantNumber==PLANT)[1]), ]
}
summary(tilia)

unique(tilia$Taxon)
length(unique(tilia$Taxon)) # 29 unique entries

# Looping through to add genus and species to the tilia form
taxa.split <- strsplit(paste(tilia$Taxon), split=" ")
for(i in 1:nrow(tilia)){
  tilia[i,"genus"] <- taxa.split[[i]][1]
  tilia[i,"species"] <- taxa.split[[i]][2]
}
summary(as.factor(tilia$species))
dim(tilia)

# Checking for how many species overlap with NPN
length(which(unique(tilia$species) %in% npn))

# Getting rid of trees that are may be difficult to ovserve  

trees.exclude1 <- tilia[tilia$BgLongitude<=-88.068438001,] 

trees.exclude2 <- tilia[tilia$BgLatitude>=41.818238,]

trees.exclude <- rbind(trees.exclude1,trees.exclude2)

#Creating a new list composed of all trees except those listed in tree.exclude
tilia <- anti_join(tilia, trees.exclude)


# Not sure if I need this right now
#Provenance Codes: G = garden source; U = Unknown (for us, often natural recruit); W = wild source accession; Z = propagule from wild source
#tilia2 <- tilia[(tilia$species %in% npn) & !is.na(tilia$species) & !tilia$PlantNumber %in% trees.exclude,]
#summary(tilia2)
#dim(tilia2)
#dim(tilia)

#writing to see if this works

#write.csv(tilia,"/Users/breidy/Documents/tilacheck2.csv", row.names = FALSE)

#unique(tilia2$Taxon)

#unique(tilia[!tilia$PlantNumber %in% tilia2$PlantNumber, "Taxon"])
#unique(tilia[!tilia$PlantNumber %in% tilia2$PlantNumber, "species"])
#unique(tilia2$Taxon)
#unique(tilia2$species)
#summary(as.factor(tilia2$species))
# ----------------------------


# ----------------------------
# Doing some clustering and mapping
# ----------------------------


# Compute a euclidean distance matrix
tilia.dist <- dist(tilia[,c("BgLongitude", "BgLatitude")])
summary(tilia.dist)

#Visualizing distance
fviz_dist(tilia.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#getting the kmean for 4 cetners with 50 starts
tilia.kmeans4 <- kmeans(tilia[,c("BgLongitude", "BgLatitude")], centers=4, nstart=50)

#viewing the structure of the kmeans
str(tilia.kmeans)

fviz_cluster(tilia.kmeans4, data = tilia.dist)

fviz_cluster(tilia.kmeans4, data =tilia.dist, choose.vars = NULL, stand = TRUE,
axes = c(1, 2), geom = c("text"), repel = FALSE,
show.clust.cent =FALSE, ellipse = FALSE, ellipse.type = "convex",
ellipse.level = 0.95, ellipse.alpha = 0.2, shape = NULL,
pointsize = 1.5, labelsize = 12, main = "Cluster plot",)

#compairing different clusters with differnt center sizes
tilia.kmeans3 <- kmeans(tilia[,c("BgLongitude", "BgLatitude")], centers=3, nstart=50)
tilia.kmeans5 <- kmeans(tilia[,c("BgLongitude", "BgLatitude")], centers=5, nstart=50)
tilia.kmeans6 <- kmeans(tilia[,c("BgLongitude", "BgLatitude")], centers=6, nstart=50)

# plots to compare
p1 <- fviz_cluster(tilia.kmeans3, geom = "point", data = tilia.dist) + ggtitle("k = 3")
p2 <- fviz_cluster(tilia.kmeans4, geom = "point",  data = tilia.dist) + ggtitle("k = 4")
p3 <- fviz_cluster(tilia.kmeans5, geom = "point",  data = tilia.dist) + ggtitle("k = 4")
p4 <- fviz_cluster(tilia.kmeans6, geom = "point",  data = tilia.dist) + ggtitle("k = 6")

### DID NOT WORK
grid.arrange(p1, p2, p3, p4, nrow = 2)


dist.kmeans <- as.matrix(dist(tilia.kmeans$centers))
summary(dist.kmeans)



#----------------- Working above this line----------------------#




# Go through and add in the closest group so that groups are between 20 & 30 trees
max.trees <- 17
min.trees <-10 
n.groups <- round(nrow(tilia)/mean(max.trees + min.trees))
n.groups

# ---------
# Option 1: Group groups based on distance
# ---------
# k-means clustering
# nrow(tilia2)/18
# set.seed(30141038)
# set.seed(30141103)
#set.seed(201901) # Good, but we'll cut group

tilia.kmeans <- kmeans(tilia[,c("BgLongitude", "BgLatitude")], centers=n.groups*2, nstart=25) # Starting with our desired end point x 4 so we have small clusters that get added
 tilia.kmeans <- kmeans(tilia2[,c("BgLongitude", "BgLatitude")], centers=12) # Starting with an average of paired trees
summary(summary(as.factor(tilia.kmeans$cluster))) # Looking at average number of members when pairing
tilia.kmeans$centers
tilia$group.kmean <- as.factor(tilia.kmeans$cluster)

# Next step is doing clustering trying to balance number + distance
dist.kmeans <- as.matrix(dist(tilia.kmeans$centers))
summary(dist.kmeans)



# Finding a way to pair clusters that balances number and distance
tilia.groups <- data.frame(clust1 = as.factor(1:nrow(tilia.kmeans$centers)),
                          tilia.kmeans$centers)
for(i in 1:nrow(tilia.groups)){
  tilia.groups[i, "n.clust1"] <- length(which(tilia.kmeans$cluster==i))
}
summary(tilia.groups)

# Sort our groupings from most to least
tilia.groups <- tilia.groups[order(tilia.groups$n.clust1, tilia.groups$BgLongitude, tilia.groups$BgLatitude, decreasing=T),]
head(tilia.groups)

# Initialize with cluster 1
clust.new = 1
tilia.groups[1, "clust2"] <- clust.new

# Loop through and combine groups
set.seed(201901)
pb <- txtProgressBar(min=0, max=nrow(tilia.groups), style=3)
for(i in 1:nrow(tilia.groups)){
  
  setTxtProgressBar(pb, i)
  if(i > 1 & !is.na(tilia.groups[i, "clust2"])) next # Skip over already assigned groups
  
  # find how many are in this group
  group1 <- tilia.groups[i, "clust1"]
  tilia.groups[i,"clust2"] <- clust.new
  
  n.group <- sum(tilia.groups$n.clust1[which(tilia.groups$clust2==clust.new)])
  
  # Find the unassigned rows
  groups.open <- tilia.groups[is.na(tilia.groups$clust2), "clust1"]
  
  # When we have less than our minimum numebr of trees in the group, add the next closest group
  while(n.group<min.trees & length(groups.open)>0){
    groups.open <- tilia.groups[is.na(tilia.groups$clust2), "clust1"] # Update this in our loop
    
    # Find the closest trees(s) to the existing groups
    dist.closest <- min(dist.kmeans[groups.open,group1], na.rm=T)
    
    # If the group is further than the average distance among current groups, just skip over it
    grps.now <- which(tilia.groups$clust2==clust.new)
    if(length(grps.now)>1 & dist.closest>sd(dist.kmeans[grps.now,grps.now])*1.5) break
    # Make sure we're not adding an oddball group:
    # dist.grp <- 
    # if(dist.closet >=)
    
    group.closest <- groups.open[which(dist.kmeans[groups.open,group1]==dist.closest)]
    
    # If: 
    #  1. adding all groups of closest trees keeps us under the maximum, add them all
    # #  2a. if adding the group with the most trees keeps us under 15, add that one
    # #  2b. add the first one in the list
    if(sum(n.group, tilia.groups[tilia.groups$clust1 %in% group.closest, "n.clust1"]) <= max.trees) {
      tilia.groups[tilia.groups$clust1 %in% group.closest, "clust2"] <- clust.new
    } else if(length(group.closest)>1) {
      tilia.groups[tilia.groups$clust1 %in% group.closest[1], "clust2"] <- clust.new
    } 
    n.group <- sum(tilia.groups$n.clust1[which(tilia.groups$clust2==clust.new)])
    # group1 <- tilia.groups$clust1[which(tilia.groups$clust2==clust.new)] # Update group1 with the new cluster
  } # end while
  
  # If we have a less than full group, add the component groups to the closest group
  if(n.group < min.trees & i==nrow(tilia.groups)){
    # Find the groups we're lumping
    clust.redo <- tilia.groups[tilia.groups$clust2==clust.new, "clust1"]
    
    for(j in clust.redo){
      groups.open <- tilia.groups[tilia.groups$clust1!=j & tilia.groups$clust2!=clust.new, "clust1"]
      # if this is our last group, add it to the closest cluster
      group.closest <- groups.open[which(dist.kmeans[groups.open, j]==min(dist.kmeans[groups.open,j], na.rm=T))]
      tilia.groups[tilia.groups$clust1==j, "clust2"] <- tilia.groups[tilia.groups$clust1==group.closest, "clust2"]
    }
    
    
  }
  
  clust.new=clust.new+1
} # End loop
tilia.groups$clust2 <- as.factor(tilia.groups$clust2)
summary(tilia.groups)

# This looks pretty good, but needs just a bit of re-tweaking
# Manually moving the tiny south cluster from group 9 to group 10
# tilia.groups[tilia.groups$clust2==9,]

tilia.group2 <- aggregate(tilia.groups$n.clust1, by=list(tilia.groups$clust2), sum)
names(tilia.group2) <- c("group1", "n")
summary(tilia.group2)
tilia.group2
# clust.kmeans <- hclust(tilia.dist)
# plot(clust.kmeans)

# Assigning groups from our re-clustering
for(i in 1:nrow(tilia2)){
  group1 <- tilia2$group.kmean[i]
  tilia2[i, "group1"] <- tilia.groups[tilia.groups$clust1==paste(group1),"clust2"]
}
tilia2$group1 <- as.factor(tilia2$group1)
summary(tilia2$group1)

groups.n <- aggregate(tilia2$BgLatitude, by=list(tilia2$group1), FUN=length)
names(groups.n) <- c("group1", "n.trees")

# Getting rid of group 10 because we can't make it cooperate
tilia2 <- tilia2[tilia2$group1!=10,]

tilia2$priority <- as.factor(ifelse(tilia2$species %in% npn, "NPN", NA))
summary(tilia2)

tilia.prior <- aggregate(tilia2$priority, by=list(tilia2$group1), FUN=function(x){length(which(!is.na(x)))})
names(tilia.prior) <- c("group1", "n.trees")

# re-assign numbers to align with priority
tilia.prior[order(tilia.prior$n.trees, decreasing=T),"Priority"] <- 1:nrow(tilia.prior)

# Merge priority assignments back into master list
tilia2 <- merge(tilia2, tilia.prior[,c("group1", "Priority")], all.x=T)
summary(tilia2)

# ---------

# tilia.clust <- hclust(tilia.dist) # Perform hierarchical clustering with the distance matrix
# tilia2$group <- as.factor(cutree(tilia.clust, k=18))
# ---------

# Assiging 
# tilia2$group <- as.factor(tilia.kmeans$cluster)
summary(tilia2)

tilia.groups$group.kmean = tilia.groups$clust1

# Remove plants that no longer exist
tilia[tilia$PlantNumber %in% df.gone$PlantNumber, c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA
tilia2[tilia2$PlantNumber %in% df.gone$PlantNumber, c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA
tilia2 <- tilia2[!is.na(tilia2$BgLatitude),]

summary(tilia2)
dim(tilia2)


groups.n <- aggregate(tilia2$BgLatitude, by=list(tilia2$Priority), FUN=length)
names(groups.n) <- c("Priority", "n.trees")


library(ggplot2)
png(file.path(path.dat, "CollectionAssignments_tilia.png"), height=8, width=12, units="in", res=320)
ggplot(data=tilia2[,]) +
  ggtitle("Phenology Monitoring Lists:\ntilia Collection") +
  labs(x="Longitude", y="Latitude") +
  coord_equal() +
  facet_wrap(~Priority) +
  geom_point(data=tilia[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.1, color="black", alpha=0.2) +
  geom_point(data=tilia2[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.25, color="black") +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(Priority))) + 
  # geom_text(data=tilia.groups, x=quantile(tilia2$BgLongitude, 0.05), y=quantile(tilia2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  geom_text(data=groups.n, x=quantile(tilia$BgLongitude, 0.95, na.rm=T), y=max(tilia$BgLatitude, 0.995, na.rm=T), aes(label=paste0("n = ", n.trees)), fontface="bold") +
  guides(color=F) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        axis.title = element_blank(),
        axis.text = element_blank())
dev.off()

png(file.path(path.dat, "tilia_all.png"), height=6, width=9, units="in", res=320)
ggplot(data=tilia2) +
  # ggtitle("Phenology Monitoring Lists 2018:\ntilia Collection") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=tilia[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=2.5, color="gray50") +
  geom_point(aes(x=BgLongitude, y=BgLatitude), color="darkgreen", size=5) + 
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()


png(file.path(path.dat, "tilia_NPN.png"), height=8, width=12, units="in", res=320)
ggplot(data=tilia2) +
  # ggtitle("Phenology Monitoring Lists 2018:\ntilia Collection") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=tilia[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=1, color="black", alpha=0.2) +
  geom_point(aes(x=BgLongitude, y=BgLatitude), size=2, color="black") +
  geom_point(data=tilia2[tilia2$species %in% npn,], aes(x=BgLongitude, y=BgLatitude, color="NPN Species"), size=3) + 
  scale_color_manual(values="green4") +
  guides(color=F) +
  # geom_text(data=tilia.groups, x=quantile(tilia2$BgLongitude, 0.05), y=quantile(tilia2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()


summary(tilia2)

tilia.list <- tilia2[,c("Priority", "PlantNumber", "Taxon", "Vernacular", "BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")]
tilia.list <- tilia.list[order(tilia.list$Priority, tilia.list$Taxon, tilia.list$PlantNumber),]
names(tilia.list)[1] <- "Obs.List"
summary(tilia.list)
head(tilia.list)
write.csv(tilia.list[!is.na(tilia.list$BgLatitude),], file.path(path.dat, "ObservingLists_tilia.csv"), row.names=F)
# ----------------------------


# ----------------------------
# Making lists for each assignment
# ----------------------------
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages


# dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)


# path.dat <- file.path(dir.base, "Observing Lists/2018_tilia")
# maps.out <- file.path(path.dat)
# path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search

# ---------------
# Read in data about the tilia collection
tilias.all <- read.csv("../data/collections/tilia_2019-03-12_190650301-BRAHMSOnlineData.csv")
summary(tilias.all)

tilia.list <- read.csv(file.path(path.dat, "ObservingLists_tilia.csv"))
tilia.list$Obs.List <- as.factor(tilia.list$Obs.List)
summary(tilia.list); 
dim(tilia.list)


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

extent.map <- c(range(tilia.list$BgLongitude, na.rm=T)+c(-0.0005,+0.0005), range(tilia.list$BgLatitude, na.rm=T)+c(-0.0005, 0.0005))
grid.crop <- crop(morton.grid, extent.map)

# ---------------



# cols.save <- c("observer", "Taxon", "")

for(ID in unique(tilia.list$Obs.List) ){
  dat.tmp <- tilia.list[tilia.list$Obs.List==ID & !is.na(tilia.list$BgLatitude), !names(tilia.list)=="observer"]
  
  png(file.path(path.dat, paste0("ObservingList_tilia_", stringr::str_pad(ID, 2, side="left", "0"), ".png")), height=8, width=10, units="in", res=120)
  print(
    ggplot() +
      coord_equal(xlim=extent.map[1:2], ylim=extent.map[3:4]) +
      geom_polygon(data=woods, aes(x=long, y=lat, group=group), fill="darkgreen", alpha=0.5) +
      geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=5, color="gray80") +
      geom_path(data=paths, aes(x=long, y=lat, group=group), size=3, linetype="solid", color="brown") +
      geom_path(data=grid.crop, aes(x=long, y=lat, group=group), size=0.25, linetype="dotted", color="gray30") +
      geom_point(data=tilias.all, aes(x=BgLongitude, y=BgLatitude), color="black", size=1.5) +
      geom_point(data=tilia.list, aes(x=BgLongitude, y=BgLatitude), color="gray50", size=3) +
      geom_point(data=dat.tmp, aes(x=BgLongitude, y=BgLatitude), color="darkgreen", size=5) +
      # geom_text(data=labs.x[2:nrow(labs.x),], aes(x=long, y=lat+0.0002, label=x.lab), color="black", fontface="bold") +
      # geom_text(data=labs.y[2:nrow(labs.y),], aes(x=long+0.0005, y=lat, label=y.lab), color="black", fontface="bold") +
      # scale_color_manual(values=c("gray50", "darkolivegreen3", "green4")) +
      ggtitle(paste0("The Morton Arboretum\ntilia Phenology: ", stringr::str_pad(ID, 2, side="left", "0"))) +
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
  
  write.csv(dat.tmp, file.path(path.dat, paste0("ObservingList_tilia_", stringr::str_pad(ID, 2, side="left", "0"), ".csv")), row.names=F)
}


# ----------------------------
