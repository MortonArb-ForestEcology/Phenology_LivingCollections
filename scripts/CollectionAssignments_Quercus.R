
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages


dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)


path.dat <- file.path(dir.base, "Observing Lists/Quercus")
maps.out <- file.path(path.dat)
path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search
dir.create(path.dat, recursive = T, showWarnings = F)

# Querying the googlesheet for missing trees up front to make it easier
sheet.gone <- gs_title("Removed Trees - Phenology_LivingCollections")
sheet.gone # Prints all the metadata

# Get the particular sheet & coerce it into a data frame rather than something special
df.gone <- data.frame(gs_read(sheet.gone, ws="Removed Trees"))
summary(df.gone)


# ----------------------------
# Narrowing down the phenology observering lists
# ----------------------------
quercus <- read.csv("../data/collections/Quercus_2018-03-19_161744393-BRAHMSOnlineData.csv")
quercus$Taxon2 <- quercus$Taxon
quercus$Taxon2 <- gsub("'", "", quercus$Taxon2) # Get rid of '' for names

quercus$ProvenanceType <- as.factor(toupper(quercus$ProvenanceType))
dim(quercus)

# Fixing a weird accession number; change from *1 to *2
quercus[quercus$Accession=="326-99",]
summary(quercus)

# Figuring out which are not full species or are cultivars
taxa.split <- strsplit(quercus$Taxon2, split=" ")
name.length <- unlist(lapply(taxa.split, length))

quercus2 <- quercus[which(name.length==2),]

dim(quercus2)


# Funding out what has unknown origin
summary(quercus2[quercus2$ProvenanceType!="U", ])
dim(quercus2[quercus2$ProvenanceType!="U", ])

summary(quercus2[quercus2$ProvenanceType=="W", ])
summary(droplevels(quercus2[quercus2$ProvenanceType=="W", "Taxon"]))
summary(droplevels(quercus2[quercus2$ProvenanceType!="U", "Taxon"]))

# Species in the NPN database
npn <- c("agrifolia", "alba", "arizonica", "douglasii", "emoryi", "engelmannii", "falcata", "gambelii", "garryana", "geminata", "ilicifolia", "imbricaria", "kelloggii", "laevis", "laurifolia", "lobata", "macrocarpa", "marilandica", "montana", "palustris", "phellos", "rubra", "shumardii", "vacciniifolia", "velutina", "virginiana")
length(npn)

#
length(npn[paste("Quercus", npn) %in% unique(quercus2$Taxon)])
npn[paste("Quercus", npn) %in% unique(quercus2$Taxon)]

length(which(unique(quercus2$Taxon) %in% paste("Quercus", npn)))

# Possibly prioritize US Red List species over Wild Type origin
# spp.filter <- quercus2$ProvenanceType!="U" & (quercus2$Taxon %in% paste("Quercus", npn) | quercus2$ProvenanceType=="W")
spp.filter <- quercus2$Taxon %in% paste("Quercus", npn) | quercus2$ProvenanceType!="U"
nrow(quercus2[spp.filter,])

summary(quercus2[spp.filter,])
summary(droplevels(quercus2[spp.filter,"Taxon"]))

quercus2 <- quercus2[spp.filter,]
summary(quercus2)

# Fixing a weird accession number; change from *1 to *2
quercus2[quercus2$Accession=="326-99",]
# summary(quercus)


# ----------------------------


# ----------------------------
# Doing some clustering and mapping
# ----------------------------
set.seed(12040302)
# Compute a euclidean distance matrix
oak.dist <- dist(quercus2[,c("BgLongitude", "BgLatitude")])
summary(oak.dist)

# Go through and add in the closest group so that groups are between 20 & 30 trees
n.groups <- 12+1 # Number of observers plus 1-2
max.trees <- round(nrow(quercus2)/n.groups*1.5 )
min.trees <- round(nrow(quercus2)/n.groups)


# ---------
# Option 1: Group groups based on distance
# ---------
# k-means clustering
# nrow(quercus2)/18
set.seed(12040302)

oak.kmeans <- kmeans(quercus2[,c("BgLongitude", "BgLatitude")], centers=n.groups*5) # Starting with our desired end point x ~5 so we have small clusters that get added
# oak.kmeans <- kmeans(quercus2[,c("BgLongitude", "BgLatitude")], centers=12) # Starting with an average of paired trees
summary(summary(as.factor(oak.kmeans$cluster))) # Looking at average number of members when pairing
oak.kmeans$centers
quercus2$group.kmean <- as.factor(oak.kmeans$cluster)

# Next step is doing clustering trying to balance number + distance
dist.kmeans <- as.matrix(dist(oak.kmeans$centers))
summary(dist.kmeans)



# Finding a way to pair clusters that balances number and distance
oak.groups <- data.frame(clust1 = as.factor(1:nrow(oak.kmeans$centers)),
                         oak.kmeans$centers)
for(i in 1:nrow(oak.groups)){
  oak.groups[i, "n.clust1"] <- length(which(oak.kmeans$cluster==i))
}
summary(oak.groups)

# Sort our groupings from most to least
oak.groups <- oak.groups[order(oak.groups$n.clust1, oak.groups$BgLongitude, oak.groups$BgLatitude, decreasing=T),]
head(oak.groups)

clust.new = 1
oak.groups[1, "clust2"] <- clust.new

pb <- txtProgressBar(min=0, max=nrow(oak.groups), style=3)
set.seed(12040302)
for(i in 1:nrow(oak.groups)){
  
  setTxtProgressBar(pb, i)
  if(i > 1 & !is.na(oak.groups[i, "clust2"])) next # Skip over already assigned groups

  # find how many are in this group
  group1 <- oak.groups[i, "clust1"]
  oak.groups[i,"clust2"] <- clust.new
  
  n.group <- sum(oak.groups$n.clust1[which(oak.groups$clust2==clust.new)])
  
  # Find the unassigned rows
  groups.open <- oak.groups[is.na(oak.groups$clust2), "clust1"]
  
  # When we have less than our minimum numebr of trees in the group, add the next closest group
  while(n.group<min.trees & length(groups.open)>0){
    groups.open <- oak.groups[is.na(oak.groups$clust2), "clust1"] # Update this in our loop
    
    # Find the closest trees(s)
    group.closest <- groups.open[which(dist.kmeans[groups.open,group1]==min(dist.kmeans[groups.open,group1], na.rm=T))]
    
    # If: 
    #  1. adding all groups of closest trees keeps us under 15, add them all
    # #  2a. if adding the group with the most trees keeps us under 15, add that one
    #  2b. add the first one in the list
    if(sum(n.group, oak.groups[oak.groups$clust1 %in% group.closest, "n.clust1"]) <= max.trees) {
      oak.groups[oak.groups$clust1 %in% group.closest, "clust2"] <- clust.new
    } else {
      oak.groups[oak.groups$clust1 %in% group.closest[1], "clust2"] <- clust.new
    }
    n.group <- sum(oak.groups$n.clust1[which(oak.groups$clust2==clust.new)])
  } # end while
  
  # If we have a less than full group, add the component groups to the closest group
  if(n.group < min.trees*0.75 & length(groups.open)==0){
    # Find the groups we're lumping
    clust.redo <- oak.groups[oak.groups$clust2==clust.new, "clust1"]
    
    for(j in clust.redo){
      groups.open <- oak.groups[oak.groups$clust1!=j & oak.groups$clust2!=clust.new, "clust1"]
      # if this is our last group, add it to the closest cluster
      group.closest <- groups.open[which(dist.kmeans[groups.open, j]==min(dist.kmeans[groups.open,j], na.rm=T))]
      oak.groups[oak.groups$clust1==j, "clust2"] <- oak.groups[oak.groups$clust1==group.closest, "clust2"]
    }

    
  }
  
  clust.new=clust.new+1
} # End loop
oak.groups$clust2 <- as.factor(oak.groups$clust2)
summary(oak.groups)

oak.group2 <- aggregate(oak.groups$n.clust1, by=list(oak.groups$clust2), sum)
names(oak.group2) <- c("group1", "n")
summary(oak.group2)
oak.group2
# clust.kmeans <- hclust(oak.dist)
# plot(clust.kmeans)

# Assigning groups from our re-clustering
for(i in 1:nrow(quercus2)){
  group1 <- quercus2$group.kmean[i]
  quercus2[i, "group1"] <- oak.groups[oak.groups$clust1==paste(group1),"clust2"]
}
quercus2$group1 <- as.factor(quercus2$group1)
summary(quercus2$group1)

# Fixing a weird accession number; change from *1 to *2
quercus2[quercus2$Accession=="326-99",]


# Redoing group numbers so they get assigned in order of priority based on our priority species:
# Kim gets first dibs followed by npn
quercus2$priority <- as.factor(ifelse(quercus2$Taxon %in% paste("Quercus", npn), "NPN", NA))
summary(quercus2)

quercus.prior <- aggregate(quercus2$priority, by=list(quercus2$group1), FUN=function(x){length(which(!is.na(x)))})
names(quercus.prior) <- c("group1", "n.trees")

# re-assign numbers to align with priority
quercus.prior[order(quercus.prior$n.trees, decreasing=T),"Priority"] <- 1:nrow(quercus.prior)

# Merge priority assignments back into master list
quercus2 <- merge(quercus2, quercus.prior[,c("group1", "Priority")], all.x=T)
summary(quercus2)


# ---------
# oak.clust <- hclust(oak.dist) # Perform hierarchical clustering with the distance matrix
# quercus2$group <- as.factor(cutree(oak.clust, k=18))
# ---------

# Assiging 
# quercus2$group <- as.factor(oak.kmeans$cluster)
summary(quercus2)

oak.groups$group.kmean = oak.groups$clust1

# According to Carol Nemec, Q. hartwissiana at L-100/94-36 was removed last year (27-95*5)
# Since getting rid of it all together might cause some issues, we'll just give it NAs for 
# its lat & lon and then not write those rows again
# quercus.remove <- c("134-U*5", "134-U*7", "422-48*1", "386-2010*1", "27-95*5", "526-2000*4", "526-2000*3", "539-96*5", "498-2005*3", "222-2015*1", "326-99*2", "521-54*1", "255-99*1", "466-37*1")
quercus[quercus$PlantNumber %in% df.gone$PlantNumber,c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA
quercus2[quercus2$PlantNumber %in% df.gone$PlantNumber,c("BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")] <- NA
quercus2 <- quercus2[!is.na(quercus2$BgLatitude),]


groups.n <- aggregate(quercus2$BgLatitude, by=list(quercus2$Priority), FUN=length)
names(groups.n) <- c("Priority", "n.trees")

# Fixing a weird accession number; change from *1 to *2
quercus2[quercus2$Accession=="326-99",]

summary(quercus2)
library(ggplot2)
png(file.path(path.dat, "CollectionAssignments_Quercus.png"), height=8, width=12, units="in", res=320)
ggplot(data=quercus2) +
  ggtitle("Phenology Monitoring Lists:\nQuercus Collection") +
  labs(x="Longitude", y="Latitude") +
  coord_equal() +
  facet_wrap(~Priority) +
  geom_point(data=quercus[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.1, color="black", alpha=0.2) +
  geom_point(data=quercus2[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=0.25, color="black") +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(Priority))) + 
  # geom_text(data=oak.groups, x=quantile(quercus2$BgLongitude, 0.05), y=quantile(quercus2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  geom_text(data=groups.n, x=quantile(quercus2$BgLongitude, 0.05, na.rm=T), y=quantile(quercus2$BgLatitude, 0.005, na.rm=T), aes(label=paste0("n = ", n.trees)), fontface="bold") +
  guides(color=F) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        axis.title = element_blank(),
        axis.text = element_blank())
dev.off()

png(file.path(path.dat, "Quercus_all.png"), height=6, width=9, units="in", res=320)
ggplot(data=quercus2) +
  # ggtitle("Phenology Monitoring Lists 2018:\nOak Collection") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=quercus[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=2.5, color="gray50") +
  geom_point(aes(x=BgLongitude, y=BgLatitude), color="darkgreen", size=5) + 
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()


png(file.path(path.dat, "Quercus_NPN.png"), height=8, width=12, units="in", res=320)
ggplot(data=quercus2) +
  # ggtitle("Phenology Monitoring Lists 2018:\nOak Collection") +
  labs(x="Longitude", y="Latitude") +
  geom_point(data=quercus[,c("BgLongitude", "BgLatitude")], aes(x=BgLongitude, y=BgLatitude), size=1, color="black", alpha=0.2) +
  geom_point(aes(x=BgLongitude, y=BgLatitude), size=2, color="black") +
  geom_point(data=quercus2[quercus2$Taxon %in% paste("Quercus", npn),], aes(x=BgLongitude, y=BgLatitude, color="NPN Species"), size=3) + 
  scale_color_manual(values="green4") +
  guides(color=F) +
  # geom_text(data=oak.groups, x=quantile(quercus2$BgLongitude, 0.05), y=quantile(quercus2$BgLatitude, 0.005), aes(label=paste0("n = ", n.clust1)), fontface="bold") +
  coord_equal() +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, face="bold"))
dev.off()


summary(quercus2)

quercus.list <- quercus2[,c("Priority", "PlantNumber", "Taxon", "Vernacular", "BgLatitude", "BgLongitude", "GardenGrid", "GardenSubGrid")]
quercus.list <- quercus.list[order(quercus.list$Priority, quercus.list$Taxon, quercus.list$PlantNumber),]
names(quercus.list)[1] <- "Obs.List" # Renaming the first column to be consistent across lists
summary(quercus.list)
head(quercus.list)
write.csv(quercus.list[!is.na(quercus.list$BgLatitude),], file.path(path.dat, "ObservingLists_Quercus.csv"), row.names=F)
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


# dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)

# 
# path.dat <- file.path(dir.base, "Observing Lists/2018_Quercus")
# maps.out <- file.path(path.dat)
# path.gis <- "/Volumes/GIS/Collections" # Note: could soft-code this in, but repeating it everywhere is making it easier to search

# ---------------
# Read in data about the oak collection
oaks.all <- read.csv("../data/collections/Quercus_2018-03-19_161744393-BRAHMSOnlineData.csv")
summary(oaks.all)

quercus.list <- read.csv(file.path(path.dat, "ObservingLists_Quercus.csv"))
quercus.list$Obs.List <- as.factor(quercus.list$Obs.List)
summary(quercus.list); 
dim(quercus.list)


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

for(ID in unique(quercus.list$Obs.List) ){
  dat.tmp <- quercus.list[quercus.list$Obs.List==ID & !is.na(quercus.list$BgLatitude), !names(quercus.list)=="observer"]
  
  png(file.path(path.dat, paste0("ObservingList_Quercus_", stringr::str_pad(ID, 2, side="left", "0"), ".png")), height=8, width=10, units="in", res=120)
  print(
    ggplot() +
      coord_equal(xlim=extent.map[1:2], ylim=extent.map[3:4]) +
      geom_polygon(data=woods, aes(x=long, y=lat, group=group), fill="darkgreen", alpha=0.5) +
      geom_path(data=roads[roads$name=="main route east side",], aes(x=long, y=lat, group=group), size=5, color="gray80") +
      geom_path(data=paths, aes(x=long, y=lat, group=group), size=3, linetype="solid", color="brown") +
      geom_path(data=grid.crop, aes(x=long, y=lat, group=group), size=0.25, linetype="dotted", color="gray30") +
      geom_point(data=oaks.all, aes(x=BgLongitude, y=BgLatitude), color="black", size=1.5) +
      geom_point(data=quercus.list, aes(x=BgLongitude, y=BgLatitude), color="gray50", size=3) +
      geom_point(data=dat.tmp, aes(x=BgLongitude, y=BgLatitude), color="darkgreen", size=5) +
      geom_text(data=labs.x[2:nrow(labs.x),], aes(x=long, y=lat+0.0002, label=x.lab), color="black", fontface="bold") +
      geom_text(data=labs.y[2:nrow(labs.y),], aes(x=long+0.0005, y=lat, label=y.lab), color="black", fontface="bold") +
      # scale_color_manual(values=c("gray50", "darkolivegreen3", "green4")) +
      ggtitle(paste0("The Morton Arboretum\nQuercus Phenology: ", stringr::str_pad(ID, 2, side="left", "0"))) +
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
  
  write.csv(dat.tmp, file.path(path.dat, paste0("ObservingList_Quercus_", stringr::str_pad(ID, 2, side="left", "0"), ".csv")), row.names=F)
}


# ----------------------------
