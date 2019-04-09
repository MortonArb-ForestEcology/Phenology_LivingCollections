# -------------------------------------------------------------
# Checking weekly phenology observation data
# -------------------------------------------------------------


# -------------------------------------------------------------
# Set file paths, load libraries etc.
# -------------------------------------------------------------
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages
library(lubridate)

# Source my cleaning function
source("clean_google_form.R")

dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
# setwd(dir.base)

path.dat <- file.path(dir.base, "Observing Lists/2018_Quercus")
maps.out <- file.path(path.dat)
path.gis <- "/Volumes/GIS/Collections" # Path on a Mac
# path.gis <- "Y:/Collections" # Path on a PC
# -------------------------------------------------------------


# -------------------------------------------------------------
# Access & format the observations
# -------------------------------------------------------------
# get the data from a particular sheet
quercus <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Quercus", dat.yr=lubridate::year(Sys.Date()))
summary(quercus)

acer <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Acer", dat.yr=lubridate::year(Sys.Date()))
summary(acer)

dat.all <- rbind(quercus, acer)
summary(dat.all)

#----------------------------
# For QAQC, get rid of trees that have been removed
#----------------------------
# Querying the googlesheet for missing trees up front to make it easier
sheet.gone <- gs_title("Removed Trees - Phenology_LivingCollections")
sheet.gone # Prints all the metadata

# Get the particular sheet & coerce it into a data frame rather than something special
df.gone <- data.frame(gs_read(sheet.gone, ws="Removed Trees"))
summary(df.gone)

dat.all <- dat.all[!dat.all$PlantNumber %in% df.gone$PlantNumber,]
summary(dat.all)

# Also merge in the observing lists and volunteer assignments
quercus.list <- read.csv(file.path(dir.base, "Observing Lists/Quercus", "ObservingLists_Quercus.csv"))
acer.list <- read.csv(file.path(dir.base, "Observing Lists/Acer", "ObservingLists_Acer.csv"))
quercus.list$collection <- "Quercus"
acer.list$collection <- "Acer"
quercus.list$group1 <- paste(quercus.list$collection, quercus.list$group1, sep="-")
acer.list$group1 <- paste(acer.list$collection, acer.list$group1, sep="-")

summary(quercus.list)
summary(acer.list)

obs.list <- rbind(quercus.list, acer.list)
summary(obs.list)

dat.all <- merge(dat.all, obs.list[,c("group1", "collection", "PlantNumber")])
#----------------------------


#----------------------------
# QAQC sanity checks: Check for the following
#----------------------------
# Observation dates prior to the year we're observing
# Oberservation dates after todays date
# Missing plant numbers
#----------------------------
# dat.all[is.na(dat.all$PlantNumber),]
# summary(dat.all[dat.all$Species=="Quercus macrocarpa" & dat.all$Observer=="Dorrell",])

yr.wrong <- dat.all[lubridate::year(dat.all$Date.Observed)<lubridate::year(Sys.Date()),"Date.Observed"]
# dat.all[dat.all$Date.Observed==yr.wrong,"Date.Observed"] <- as.Date(paste(2018, month(yr.wrong), day(yr.wrong), sep="-"))

# dat.all <- droplevels(dat.all) # Get rid of unused levels
summary(dat.all)
#----------------------------

#----------------------------
# Check to make sure observers are entering their data right away
#----------------------------
# Check for our list of removed trees 
# Checking to make sure everybody has made observations in the past week
obs.gs <- gs_title("VolunteerAssignments_Phenology")
obs.all <- data.frame(gs_read(obs.gs, ws="2019"))[1:5]
obs.all$group1 <- paste(obs.all$Collection, obs.all$List, sep="-")
obs.all


obs.check <- aggregate(dat.all$Date.Observed, by=list(dat.all$Observer), FUN=max)
names(obs.check) <- c("Observer", "Observation.Last")
obs.check
obs.check[obs.check$Observation.Last < Sys.Date()-8,] # Return anybody that's more than 8 days old

# See if anybody has not enetered at all
obs.all[!obs.all$Observer.ID %in% obs.check$Observer,]

# Checking to make sure all trees have observations for the past week
acc.check <- aggregate(dat.all$Date.Observed, by=dat.all[,c("PlantNumber", "Species", "group1")], FUN=max)
names(acc.check)[which(names(acc.check)=="x")] <- "Observation.Last"
acc.check <- merge(acc.check, obs.all[,c("group1", "Observer.ID")], all.x=T)
summary(acc.check)
acc.check[acc.check$Observation.Last < Sys.Date()-8,] # Return any tree that hasn't been observed for more than 8 days
#----------------------------
# -------------------------------------------------------------

# -------------------------------------------------------------
# Adding status QAQC
# -------------------------------------------------------------
#----------------------------
# Subsetting to just things that have been observed in the past week
#----------------------------
# Finding just the most recent observation for each tree
# acc.check[acc.check$Observation.Last < Sys.Date()-8,] # Return any tree that hasn't been observed for more than 
pheno.now <- dat.all 

for(ID in unique(pheno.now$PlantNumber)){
  dat.ID <- pheno.now[pheno.now$PlantNumber==ID,]
  pheno.now <- pheno.now[pheno.now$PlantNumber!=ID | (pheno.now$PlantNumber==ID & pheno.now$Date.Observed==max(dat.ID$Date.Observed)),]
}
dim(pheno.now); dim(dat.all)

pheno.now$Status <- as.factor(ifelse(pheno.now$Date.Observed < Sys.Date()-7, "OLD", "Past Week" ))
summary(pheno.now$Status)
summary(pheno.now)

pheno.leaf <- names(pheno.now)[grep("leaf", names(pheno.now))]
pheno.flower <- names(pheno.now)[grep("flower", names(pheno.now))]
pheno.fruit <- names(pheno.now)[grep("fruit", names(pheno.now))]

pheno.table <- data.frame(stringr::str_split(c(pheno.leaf, pheno.flower, pheno.fruit), "[.]", simplify=T))
names(pheno.table) <- c("category", "phase", "type")

pdf(paste0("Phenology_LivingCollections_QAQC_", Sys.Date(), ".pdf"), width=11, height=8.5)
for(CAT in unique(pheno.table$category)){
 for(PHASE in unique(pheno.table$phase[pheno.table$category==CAT])){
   dat.tmp <- pheno.now[pheno.now$Status=="Past Week",]
   dat.tmp$obs <- dat.tmp[,paste(CAT, PHASE, "observed", sep=".")]
   if(PHASE == "falling"){ 
     dat.tmp$int <- NA
    } else {
      dat.tmp$int <- dat.tmp[,paste(CAT, PHASE, "intensity", sep=".")]
   }
  
   # Set ordered levels 
   if(length(grep("10,000", unique(dat.tmp$int))>0)){
     dat.tmp$int <- factor(dat.tmp$int, levels=c("0", "11-100", "101-1000", "1,001-10,000", "> 10,000", NA))
   } else if(length(grep("%", unique(dat.tmp$int))>0)){
     dat.tmp$int <- factor(dat.tmp$int, levels=c("0%", "< 5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%", NA))
   } else {
     dat.tmp$int <- factor(dat.tmp$int, levels=c("None", "Little", "Some", "Lots", NA))
   }
   
   plot.status <- ggplot(data=dat.tmp) +
     facet_grid(collection~., scales="free_y") +
     geom_histogram(aes(x=obs), stat="count") +
     scale_x_discrete(name="Observed") +
     ggtitle(paste(CAT, PHASE, "Observed")) +
     theme_bw()
   
   plot.intensity <- ggplot(data=dat.tmp) +
     facet_grid(collection~., scales="free_y") +
     geom_histogram(aes(x=int), stat="count") +
     scale_x_discrete(name="Intensity") +
     ggtitle(paste(CAT, PHASE, "Intensity")) +
     theme_bw()
   
   print(cowplot::plot_grid(plot.status, plot.intensity, ncol=2))
   
 } 
}
dev.off()
#----------------------------

# -------------------------------------------------------------



# -------------------------------------------------------------
# Save Maps & Animations
# -------------------------------------------------------------
# Read in data about all of the trees in the oak collection
quercus.all <- read.csv("../data/collections/Quercus_2018-03-19_161744393-BRAHMSOnlineData.csv")
acer.all <- read.csv("../data/collections/Acer_2019-03-12_190650301-BRAHMSOnlineData.csv")
summary(quercus.all)
summary(acer.all)

# Getting rid of duplicates
quercus.loc <- aggregate(quercus.all[,c("BgLatitude", "BgLongitude")],
                         by=quercus.all[,c("PlantNumber", "Accession")],
                         FUN=mean, na.rm=T)
acer.loc <- aggregate(acer.all[,c("BgLatitude", "BgLongitude")],
                         by=acer.all[,c("PlantNumber", "Accession")],
                         FUN=mean, na.rm=T)
quercus.loc$collection <- "Quercus"
acer.loc$collection <- "Acer"
trees.loc <- rbind(quercus.loc, acer.loc)
summary(trees.loc)

# Merge in the lat/lon 
pheno.now <- merge(pheno.now, trees.loc, all.x=T, all.y=F)
# summary(pheno.now); dim(pheno.now)


#----------------------------
# Read in & formate Arb GIS layers
#----------------------------
#Collection Boundaries
collections <- readOGR(file.path(path.gis, "Collections_outlines/coll_bndry_master_plan.shp"))
summary(collections)

# Roads, Trails
roads <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/circ_veh_rd_2011-2020_ctrln.shp")
paths <- readOGR("/Volumes/GIS/Collections/Transportation/trails_paths/paths.shp")
parking <- readOGR("/Volumes/GIS/Collections/Transportation/roads_parking/parking_lots.shp")
# summary(roads)
# summary(paths)


# Transforming our datalayers to lat/lon to mesh with the tree data
collections <- spTransform(collections, CRS("+proj=longlat"))
roads <- spTransform(roads, CRS("+proj=longlat"))
paths <- spTransform(paths, CRS("+proj=longlat"))
#----------------------------




#----------------------------
#----------------------------
# Making a snapshot update map
maps.out <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Figures/"
png(file.path(maps.out, paste0("Map_Phenology_All_BreakingLeafBuds_", Sys.Date(), ".png")), height=7.5, width=10, unit="in", res=180)
ggplot(data=pheno.now) +
  coord_equal() +
  geom_polygon(data=collections[collections$collection %in% c("Acer"),], aes(x=long, y=lat, group=group, fill="Acer"), alpha=0.2) +
  geom_polygon(data=collections[collections$collection %in% c("Quercus"),], aes(x=long, y=lat, group=group, fill="Quercus"), alpha=0.2) +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=Breaking.leaf.buds..Observed.), size=3) +
  scale_color_manual(name="Breaking\nLeaf Bud", values=c("chocolate4", "olivedrab3", "green3")) +
  scale_fill_manual(name="Collection", values=c("red", "blue")) +
  theme_bw() + 
  theme(legend.position="top",
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank())
dev.off()

png(file.path(maps.out, paste0("Map_Phenology_All_Flowers_", Sys.Date(), ".png")), height=7.5, width=10, unit="in", res=180)
ggplot(data=pheno.now) +
  coord_equal() +
  geom_polygon(data=collections[collections$collection %in% c("Acer"),], aes(x=long, y=lat, group=group, fill="Acer"), alpha=0.2) +
  geom_polygon(data=collections[collections$collection %in% c("Quercus"),], aes(x=long, y=lat, group=group, fill="Quercus"), alpha=0.2) +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=Flower.open.observed..Observed.)) +
  scale_color_manual(name="Breaking\nLeaf Bud", values=c("chocolate4", "maroon3", "green3")) +
  scale_fill_manual(name="Collection", values=c("red", "blue")) +
  theme_bw() + 
  theme(legend.position="top",
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank())
dev.off()

# -------------------------------------------------------------
