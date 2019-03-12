
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages


dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)


path.dat <- file.path(dir.base, "Observing Lists/Acer")
maps.out <- file.path(path.dat)
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search

# Species in the NPN database
npn <- c("macrophyllum", "grandidentatum", "nigrum", "negundo", "spicatum", "platanoides", "rubrum", "glabrum", "saccharinum", "pensylvanicum", "saccharum", "pseudoplatanus", "circinatum")
length(npn)

spp.keep <- c("palmatum", "miyabei", "freemanii", "campestre")

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

acer2 <- acer[acer$species %in% npn | acer$ProvenanceType %in% c("U", "W", "Z"),]
summary(acer2)
dim(acer2)
dim(acer)

unique(acer2$Taxon)

unique(acer[!acer$PlantNumber %in% acer2$PlantNumber, "Taxon"])
acer[acer$Taxon=="Acer saccharum 'Green Mountain'", ]

# Funding out what has unknown origin
summary(acer2[acer2$ProvenanceType!="U", ])
dim(acer2[acer2$ProvenanceType!="U", ])

summary(acer2[acer2$ProvenanceType=="W", ])
summary(droplevels(acer2[acer2$ProvenanceType=="W", "Taxon"]))
summary(droplevels(acer2[acer2$ProvenanceType!="U", "Taxon"]))


#
length(npn[paste("Acer", npn) %in% unique(acer2$Taxon)])
npn[paste("Acer", npn) %in% unique(acer2$Taxon)]


# Possibly prioritize US Red List species over Wild Type origin
# spp.filter <- acer2$ProvenanceType!="U" & (acer2$Taxon %in% paste("Acer", npn) | acer2$ProvenanceType=="W")
spp.filter <- acer2$Taxon %in% paste("Acer", npn) | acer2$ProvenanceType!="U"
nrow(acer2[spp.filter,])

summary(acer2[spp.filter,])
summary(droplevels(acer2[spp.filter,"Taxon"]))

acer2 <- acer2[spp.filter,]
summary(acer2)
# ----------------------------


# ----------------------------
# Doing some clustering and mapping
# ----------------------------
set.seed(12040302)
# Compute a euclidean distance matrix
acer.dist <- dist(acer2[,c("BgLongitude", "BgLatitude")])
summary(acer.dist)

# Go through and add in the closest group so that groups are between 20 & 30 trees
n.groups <- 12+1 # Number of observers plus 1-2
max.trees <- round(nrow(acer2)/n.groups*1.5 )
min.trees <- round(nrow(acer2)/n.groups)


# ---------
# Option 1: Group groups based on distance
# ---------
# k-means clustering
# nrow(acer2)/18
set.seed(12040302)

acer.kmeans <- kmeans(acer2[,c("BgLongitude", "BgLatitude")], centers=n.groups*5) # Starting with our desired end point x ~5 so we have small clusters that get added
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

clust.new = 1
acer.groups[1, "clust2"] <- clust.new

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
    
    # Find the closest trees(s)
    group.closest <- groups.open[which(dist.kmeans[groups.open,group1]==min(dist.kmeans[groups.open,group1], na.rm=T))]
    
    # If: 
    #  1. adding all groups of closest trees keeps us under 15, add them all
    # #  2a. if adding the group with the most trees keeps us under 15, add that one
    #  2b. add the first one in the list
    if(sum(n.group, acer.groups[acer.groups$clust1 %in% group.closest, "n.clust1"]) <= max.trees) {
      acer.groups[acer.groups$clust1 %in% group.closest, "clust2"] <- clust.new
    } else {
      acer.groups[acer.groups$clust1 %in% group.closest[1], "clust2"] <- clust.new
    }
    n.group <- sum(acer.groups$n.clust1[which(acer.groups$clust2==clust.new)])
  } # end while
  
  # If we have a less than full group, add the component groups to the closest group
  if(n.group < min.trees*0.75 & length(groups.open)==0){
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
# ---------

# acer.clust <- hclust(acer.dist) # Perform hierarchical clustering with the distance matrix
# acer2$group <- as.factor(cutree(acer.clust, k=18))
# ---------

# Assiging 
# acer2$group <- as.factor(acer.kmeans$cluster)
summary(acer2)

acer.groups$group.kmean = acer.groups$clust1

# According to Carol Nemec, Q. hartwissiana at L-100/94-36 was removed last year (27-95*5)
# Since getting rid of it all together might cause some issues, we'll just give it NAs for 
# its lat & lon and then not write those rows again
acer[acer$PlantNumber=="27-95*5",c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA
acer2[acer2$PlantNumber=="27-95*5",c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA

summary(acer2)
library(ggplot2)
png(file.path(path.dat, "CollectionAssignments_Acer.png"), height=8, width=12, units="in", res=320)
ggplot(data=acer2) +
  ggtitle("Phenology Monitoring Lists:\nAcer Collection") +
  labs(x="Longitude", y="Latitude") +
  coord_equal() +
  facet_wrap(~group1) +
  geom_point(data=acer[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.1, color="black", alpha=0.2) +
  geom_point(data=acer2[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.25, color="black") +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=group1)) + 
  # geom_text(data=acer.groups, x=quantile(acer2$BgLongitude, 0.05), y=quantile(acer2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  geom_text(data=acer.group2, x=quantile(acer2$BgLongitude, 0.05, na.rm=T), y=quantile(acer2$BgLatitude, 0.005, na.rm=T), aes(label=paste0("n = ", n)), fontface="bold") +
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
  geom_point(data=acer2[acer2$Taxon %in% paste("Acer", npn),], aes(x=BgLongitude, y=BgLatitude, color="NPN Species"), size=3) + 
  scale_color_manual(values="green4") +
  guides(color=F) +
  # geom_text(data=acer.groups, x=quantile(acer2$BgLongitude, 0.05), y=quantile(acer2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()


summary(acer2)

acer.list <- acer2[,c("group1", "PlantNumber", "Taxon", "Vernacular", "BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")]
acer.list <- acer.list[order(acer.list$group1, acer.list$Taxon, acer.list$PlantNumber),]
summary(acer.list)
head(acer.list)
write.csv(acer.list[!is.na(acer.list$BgLatitude),], file.path(path.dat, "ObservingLists_Acer.csv"), row.names=F)
# ----------------------------


# ----------------------------
# Creating list assignments by observer
# ----------------------------
# observer.list <- data.frame(list=1:12, Name=c())
# Assigning names based on interests & time commitment# 
# -- Ellen Raimondi & Robin Solomon need the shortest lists
# -- Carol Nemec wanted as much overlap with last year as possible
# -- Barabara Rose would like as many natives as possible
# -- Larry Peterman & Frank Zibrat have the fewest time constraints
# ----------------------------

library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages


dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)


path.dat <- file.path(dir.base, "Observing Lists/2018_Acer")
maps.out <- file.path(path.dat)
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search

# ---------------
# Read in data about the acer collection
acers.all <- read.csv("../data/collections/Acer_2018-03-19_161744393-BRAHMSOnlineData.csv")
summary(acers.all)

acer.list <- read.csv(file.path(path.dat, "ObservingLists_Acer.csv"))
acer.list$group1 <- as.factor(acer.list$group1)
summary(acer.list); 
dim(acer.list)

# Create the basis for the observing list
obs.list <- aggregate(acer.list[,c("BgLatitude", "BgLongitude")], by=list(acer.list$group1), FUN=mean)
obs.list$n.group <- aggregate(acer.list[,c("BgLatitude")], by=list(acer.list$group1), FUN=length)[,2]

# --------
# Find out which one overlaps with Carol's the most
# --------
# Found out one of Carol's old trees is gone, so we're going to remove it and just give her 
# 7 (which is what she was previously assigned with my algorithm)
# list.2017.sheet <- gs_title("Phenology_LivingCollections_ObservingLists")
# list.2017 <- data.frame(gs_read(list.2017.sheet, ws="2017 Observers - Acer Collection"))
# summary(list.2017)
# unique(list.2017$Primary.Observer)
# 
# trees.carol <- acer.list[acer.list$PlantNumber %in% list.2017[list.2017$Primary.Observer == "Carol Nemec", "Accession"], ]
# trees.carol <- summary(trees.carol$group1)
# list.carol <- names(trees.carol[which(trees.carol == max(trees.carol))])
# summary(trees.carol)

obs.list[obs.list$Group.1==7,"observer"] <- "Nemec"


trees.brock <- acer.list[acer.list$PlantNumber %in% list.2017[list.2017$Primary.Observer == "Brock Bigsby", "Accession"], ]
trees.brock <- summary(trees.brock$group1)
list.brock <- names(trees.brock[which(trees.brock == max(trees.brock))])[1]
trees.brock

obs.list[obs.list$Group.1==list.brock,"observer"] <- "Bigsby"

# --------

# --------
# For Robin & Ellen, pick the 2 with the fewest trees that are closest together
# --------
for(i in 1:nrow(obs.list)){
  dist.lat <- acer.list[acer.list$group1==i,"BgLatitude"] - obs.list[i,"BgLatitude"]
  dist.lon <- acer.list[acer.list$group1==i,"BgLongitude"] - obs.list[i,"BgLongitude"]
  dist.trees <- sqrt(dist.lat^2 + dist.lon^2)
  
  obs.list[i,"dist.mean"] <- mean(dist.trees)
  obs.list[i,"dist.sd"  ] <- sd(dist.trees)
}
obs.list

for(OBS in c("Solomon", "Raimondi")){
  dat.temp <- obs.list[is.na(obs.list$observer),]
  list.clump <-dat.temp[dat.temp$n.group == min(dat.temp$n.group) & dat.temp$dist.mean==min(dat.temp$dist.mean),"Group.1"]
  obs.list[obs.list$Group.1 == list.clump,"observer"] <- OBS
}
obs.list
# --------

# --------
# Giving Frank and Larry the lists with the most trees
# --------
for(OBS in c("Zibrat", "Peterman")){
  dat.temp <- obs.list[is.na(obs.list$observer),]
  list.clump <-dat.temp[dat.temp$n.group == max(dat.temp$n.group) ,"Group.1"]
  obs.list[obs.list$Group.1 == list.clump,"observer"] <- OBS
}
obs.list
# --------

# --------
# Find the remaining list with the most key local species
# --------
spp.key <- c("Acer macrocarpa", "Acer alba", "Acer rubra", "Acer palustris", "Acer velutina")
summary(acer.list$Taxon)

dat.temp <- acer.list[acer.list$group1 %in% unique(obs.list[is.na(obs.list$observer), "Group.1"]) & acer.list$Taxon %in% spp.key,]
dim(dat.temp)
summary(dat.temp)

trees.br <- summary(dat.temp$group1)
list.br <- names(trees.br[which(trees.br == max(trees.br))])
obs.list[obs.list$Group.1==list.br,"observer"] <- "Rose"
# --------

# --------
# Assign the remaining peopel in Alphabetical Order
# --------
unassigned <- c("Coffey-Sears", "Dorrell", "Frerichs", "Krummel", "Wilderman")

for(OBS in unassigned){
  grp.obs <- obs.list[is.na(obs.list$observer), "Group.1"][1]
  obs.list[obs.list$Group.1==grp.obs, "observer"] <- OBS
}
obs.list

write.csv(obs.list, file.path(path.dat, "Phenology_LivingCollections_ObservingList_2018.csv"), row.names = F)
# --------


obs.list <- read.csv(file.path(path.dat, "Phenology_LivingCollections_ObservingList_2018.csv"))
summary(obs.list)

# Merge Observer names into acer.list
names(obs.list)[1] <- c("group1")
obs.list$observer <- as.factor(obs.list$observer)
summary(obs.list)

acer.list <- merge(acer.list, obs.list[,c("group1", "observer")], all=T)

summary(acer.list)


png(file.path(path.dat, "Acer_lists_ListID.png"), height=8, width=12, units="in", res=320)
ggplot(data=acer.list[]) +
  ggtitle("Phenology Monitoring Lists 2018:\nAcer Collection") +
  labs(x="Longitude", y="Latitude") +
  facet_wrap(~group1) +
  geom_point(data=acer[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.1, color="black", alpha=0.2) +
  geom_point(data=acer2[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.25, color="black") +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=group1)) + 
  # geom_text(data=acer.groups, x=quantile(acer2$BgLongitude, 0.05), y=quantile(acer2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  guides(color=F) +
  geom_text(data=obs.list, x=quantile(acer2$BgLongitude, 0.05, na.rm=T), y=quantile(acer2$BgLatitude, 0.005, na.rm=T), aes(label=paste0("n = ", n.group)), fontface="bold") +
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()

# png(file.path(path.dat, "Acer_lists_2018_Names.png"), height=8, width=12, units="in", res=320)
# ggplot(data=acer.list[]) +
#   ggtitle("Phenology Monitoring Lists 2018:\nAcer Collection") +
#   labs(x="Longitude", y="Latitude") +
#   facet_wrap(~observer) +
#   geom_point(data=acer[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.1, color="black", alpha=0.2) +
#   geom_point(data=acer2[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.25, color="black") +
#   geom_point(aes(x=BgLongitude, y=BgLatitude, color=observer)) + 
#   # geom_text(data=acer.groups, x=quantile(acer2$BgLongitude, 0.05), y=quantile(acer2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
#   geom_text(data=obs.list, x=quantile(acer2$BgLongitude, 0.05, na.rm=T), y=quantile(acer2$BgLatitude, 0.005, na.rm=T), aes(label=paste0("n = ", n.group)), fontface="bold") +
#   coord_equal() +
#   theme_bw() +
#   theme(plot.title=element_text(hjust=0.5, face="bold"))
# dev.off()
# ---------------



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



# cols.save <- c("observer", "Taxon", "")

for(ID in unique(acer.list$group1) ){
  dat.tmp <- acer.list[acer.list$group1==ID & !is.na(acer.list$BgLatitude), !names(acer.list)=="observer"]
  
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
      geom_text(data=labs.x[2:nrow(labs.x),], aes(x=long, y=lat+0.0002, label=x.lab), color="black", fontface="bold") +
      geom_text(data=labs.y[2:nrow(labs.y),], aes(x=long+0.0005, y=lat, label=y.lab), color="black", fontface="bold") +
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
  
  write.csv(dat.tmp, paste0("ObservingList_Acer_", stringr::str_pad(ID, 2, side="left", "0"), ".csv"), row.names=F)
}


# ----------------------------
