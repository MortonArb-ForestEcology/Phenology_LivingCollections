# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Updating all of our monitoring lists since we've had a lot of mortality, refinement in scope, etc. ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Collections of Interest: ----
# 1) Oaks (Quercus)
# 2) Maples (Acer)
# 3) Elms (Ulmus)
# 4) Linden (Tilia)
# 5) additional Q. macrocarpa
#
# Main taxonomic interest: ----
# 1. Species with wild types: theoretically allow for in situ vs. ex situ comparison; incl. cultivars
# 2. Hybrids with parents in above group
# 3. Interspecific hybrids + parents
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load in useful packages ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)

source("HelperFuncitons_ObservingLists.R")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish key file paths ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The basic steps for flex-coding on a mac
user.google <- dir("~/Library/CloudStorage/")
google.mac <- file.path("~/Library/CloudStorage", user.google, "My Drive/LivingCollections_Phenology")

# Lucien or PC users, put the path here using similar logic as setting things up for a mac
google.PC <- NA 

# Automatically pick mac or PC path depending on what actually works
path.google <- ifelse(dir.exists(google.mac), google.mac, google.PC)

# Establish google sheets authorization to make life easier
# googlesheets4::gs4_auth("crolllinson@mortonarb.org")

# Target List size: 15-20
listMin = 15
listMax = 25
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in the our old observing lists for reference ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dir(file.path(path.google, "Observing Lists/OLD", "Quercus"))

oldQuercus <- read.csv(file.path(path.google, "Observing Lists/OLD", "Quercus/ObservingList_Quercus.csv"))
oldQuercus <- oldQuercus[,!names(oldQuercus) %in% "X"]
oldQuercus$GardenLocalityName <- "Quercus"

oldAcer <- read.csv(file.path(path.google, "Observing Lists/OLD", "Acer/ObservingList_Acer.csv"))
oldAcer <- oldAcer[,!names(oldAcer) %in% "X"]
oldAcer$GardenLocalityName <- "Acer"

oldUlmus <- read.csv(file.path(path.google, "Observing Lists/OLD", "Ulmus/ObservingList_Ulmus.csv"))
oldUlmus <- oldUlmus[,!names(oldUlmus) %in% "X"]
oldUlmus$GardenLocalityName <- "Ulmus"

oldTilia <- read.csv(file.path(path.google, "Observing Lists/OLD", "Tilia/ObservingList_Tilia.csv"))
oldTilia <- oldTilia[,!names(oldTilia) %in% "X"]
oldTilia$GardenLocalityName <- "Tilia"

# Put all of the previous observing lists together and remove anything dead
oldAll <- rbind(oldQuercus, oldAcer, oldUlmus, oldTilia)
# rm(oldQuercus, oldAcer, oldUlmus, oldTilia)

# Now also grab the trees that had been removed
treesGone <- googlesheets4::read_sheet("16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ")

oldAll <- oldAll[!oldAll$PlantNumber %in% unique(treesGone$PlantNumber), ]
# names(oldAll)[names(oldAll) %in% c("BgLongitude", "BgLatitude")]
names(oldAll)
dim(oldAll)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in the main BRAHMS dataset: all living trees in the collections as of Jan 26 2023 ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
collectionsWant <- c("Quercus", "Acer", "Ulmus", "Tilia")

brahmsAll <- readxl::read_xlsx(file.path(path.google, "Observing Lists/MortonArb_BRAHMS_AllAliveTrees_2023-02-27.xlsx"))
summary(brahmsAll)

# Looking for where we can narrow down to the collections --> "GardenLocalityName"
unique(brahmsAll$GardenLocalityName)[order(unique(brahmsAll$GardenLocalityName))]

# Double check to make sure the collections we want are in the above list
collectionsWant %in% unique(brahmsAll$GardenLocalityName)[order(unique(brahmsAll$GardenLocalityName))]

# Subsetting everything down to just what's in our key collections all at once to start... 
# we can go collection by collection in a bit, but I want to start keeping things together
treesCollections <- data.frame(brahmsAll[brahmsAll$GardenLocalityName %in% collectionsWant,])

# Remove trees in the collections not of the genus we care about
treesCollections <- treesCollections[(treesCollections$GardenLocalityName=="Quercus" & treesCollections$GenusName=="Quercus") | 
                                       (treesCollections$GardenLocalityName=="Acer" & treesCollections$GenusName=="Acer") |
                                       (treesCollections$GardenLocalityName=="Ulmus" & treesCollections$GenusName=="Ulmus") |
                                       (treesCollections$GardenLocalityName=="Tilia" & treesCollections$GenusName=="Tilia"),]
# Remove trees missing a coordinate
treesCollections <- treesCollections[!is.na(treesCollections$Latitude),]

summary(treesCollections)

# Making it so that there are no blank species name just for our own sanity and record keeping
treesCollections$SpeciesName[is.na(treesCollections$SpeciesName)] <- "spp."
summary(treesCollections)


# Doing a quick plot of what we were observing versus what we _could_ observe
png(file.path(path.google, "Observing Lists", "Update2023_BRAHMS_v_2022.png"), height=10, width=10, units="in", res=220)
ggplot(data=treesCollections) +
  facet_wrap(~GardenLocalityName, scales="free") +
  # coord_map() +
  geom_point(aes(x=Longitude, y=Latitude, color="BRAHMS 2023"), size=2, alpha=0.5) +
  geom_point(data=oldAll, aes(x=BgLongitude, y=BgLatitude, color="Observing 2022"), size=1, alpha=0.8) +
  scale_color_manual(name="Tree Source", values=c("Observing 2022"="green3", "BRAHMS 2023"="black")) +
  theme_bw() +
  labs(caption=c("note: x/y axes not equal --> shape may be distrorted")) +
  theme(legend.position="top")
dev.off()

# Comparing Number of Trees we've been observing vs. what's in our collections
names(treesCollections); names(oldAll)

# Indicating whether a tree in the new mast list is part of the old
treesCollections$PastObserving <- ifelse(treesCollections$PlantId %in% oldAll$PlantNumber, T, F)
summary(treesCollections)

# Getting total potential and actual tree counts by Collection
CollectionStats <- aggregate(PlantId ~ GardenLocalityName, data=treesCollections, FUN=length)
names(CollectionStats)[names(CollectionStats)=="PlantId"] <- "nTreesCollection"
CollectionStats$PastObserve <- aggregate(PlantId ~ GardenLocalityName, data=treesCollections[treesCollections$PastObserving,], FUN=length)$PlantId
CollectionStats


sppListAll <- aggregate(PlantId ~ GardenLocalityName + GenusName + SpeciesName, data=treesCollections, FUN=length)
names(sppListAll)[names(sppListAll)=="PlantId"] <- "PotentialObserving"

sppListPast <- aggregate(PlantId ~ GardenLocalityName + GenusName + SpeciesName, data=treesCollections[treesCollections$PastObserving,], FUN=length)
names(sppListPast)[names(sppListPast)=="PlantId"] <- "PastObserving"

head(sppListAll)
dim(sppListAll)
dim(sppListPast)

# Merging the species lists together
sppListAll <- merge(sppListAll, sppListPast, all=T)
sppListAll$PastObserving[is.na(sppListAll$PastObserving)] <- 0
dim(sppListAll)
summary(sppListAll)

sppListAll[sppListAll$GardenLocalityName=="Quercus",]
sppListAll[sppListAll$GardenLocalityName=="Acer",]
sppListAll[sppListAll$GardenLocalityName=="Ulmus",]
sppListAll[sppListAll$GardenLocalityName=="Tilia",] # Note: misses non-specific hybrids

write.csv(sppListAll[sppListAll$GardenLocalityName=="Quercus",], file.path(path.google, "Observing Lists", "Update2023_BRAHMS_PotentialSpecies_Quercus.csv"), row.names=F)
write.csv(sppListAll[sppListAll$GardenLocalityName=="Acer",], file.path(path.google, "Observing Lists", "Update2023_BRAHMS_PotentialSpecies_Acer.csv"), row.names=F)
write.csv(sppListAll[sppListAll$GardenLocalityName=="Ulmus",], file.path(path.google, "Observing Lists", "Update2023_BRAHMS_PotentialSpecies_Ulmus.csv"), row.names=F)
write.csv(sppListAll[sppListAll$GardenLocalityName=="Tilia",], file.path(path.google, "Observing Lists", "Update2023_BRAHMS_PotentialSpecies_Tilia.csv"), row.names=F)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Making some decisions about collections ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(sppListAll[,])
summary(treesCollections)

list2023 <- data.frame(List=NA, PlantID=NA, Taxon=NA, Vernacular = NA, BgLatitude=NA, BgLongitude=NA, GardenGrid=NA, GardenSubgrid=NA)

# ~~~~~~~~~~~~~~~~
# Tilia (31 trees; observe all) ----
# Starting with Tilia because easiest --> monitor everything, 2 lists
# ~~~~~~~~~~~~~~~~
colSums(sppListAll[sppListAll$GardenLocalityName=="Tilia",c("PotentialObserving", "PastObserving")])

# Next looking at Ulmus --> next biggest
colSums(sppListAll[sppListAll$GardenLocalityName=="Ulmus", c("PotentialObserving", "PastObserving")])

ggplot(data=treesCollections[treesCollections$GardenLocalityName=="Tilia",]) +
  coord_equal() +
  geom_point(aes(x=Longitude, y=Latitude))


tilia2023 <- data.frame(List=NA, PlantID = treesCollections$PlantId[treesCollections$GardenLocalityName=="Tilia"], 
                        Taxon = paste(treesCollections$GenusName[treesCollections$GardenLocalityName=="Tilia"], treesCollections$SpeciesName[treesCollections$GardenLocalityName=="Tilia"], sep=" "), 
                        Vernacular = treesCollections$VernacularName[treesCollections$GardenLocalityName=="Tilia"], 
                        BgLatitude = treesCollections$Latitude[treesCollections$GardenLocalityName=="Tilia"], 
                        BgLongitude = treesCollections$Longitude[treesCollections$GardenLocalityName=="Tilia"], 
                        GardenGrid = treesCollections$GardenSubarea1[treesCollections$GardenLocalityName=="Tilia"], 
                        GardenSubgrid = treesCollections$GardenSubarea2[treesCollections$GardenLocalityName=="Tilia"])

tilia2023$Vernacular <- gsub("'", "", tilia2023$Vernacular)
tilia2023$Vernacular <- gsub('["]', '', tilia2023$Vernacular)
tilia2023
head(treesCollections)

# Run the spatial clustering on Tilia -- this is easy so just 2 lists shoudlwork
set.seed(241133)

tilia.kmeans <- kmeans(tilia2023[,c("BgLongitude", "BgLatitude")], centers=2) # 
summary(as.factor(tilia.kmeans$cluster))
tilia2023$List <- tilia.kmeans$cluster
summary(as.factor(tilia2023$List))

# This passes the gut test
png(file.path(path.google, "Observing Lists", "Tilia", "ObservingList_Tilia_2023.png"), height=8, width=8, units="in", res=180)
ggplot(data=tilia2023) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List)))+
  scale_shape_manual(values=c(1:18))
dev.off()

length(tilia2023$List)
summary(as.factor(tilia2023$List))

length(oldTilia$Obs.List)
summary(as.factor(oldTilia$Obs.List))


write.csv(tilia2023, file=file.path(path.google, "Observing Lists", "Tilia", "ObservingList_Tilia_2023.csv"), row.names=F)

tilia2023 <- read.csv(file.path(path.google, "Observing Lists", "Tilia", "ObservingList_Tilia_2023.csv"))

for(OBSLIST in unique(tilia2023$List)){
  write.csv(tilia2023[tilia2023$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Tilia", paste0("ObservingList_Tilia_2023_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
  
}
# ~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~
# Ulmus (157 potential trees; past observing: 146) ----
# Ulmus is a little weird in that there are a handful of individuals without proper species names
# Observing rules: up to 10 indiv
# ~~~~~~~~~~~~~~~~
sppListUlmus <- aggregate(PlantId ~ GardenLocalityName + CalcFullName, data=treesCollections[treesCollections$GardenLocalityName=="Ulmus",], FUN=length)
names(sppListUlmus)[names(sppListUlmus)=="PlantId"] <- "PotentialObserving"

sppListUlmusPast <- aggregate(PlantId ~ GardenLocalityName + CalcFullName, data=treesCollections[treesCollections$GardenLocalityName=="Ulmus" & treesCollections$PastObserving,], FUN=length)
names(sppListUlmusPast)[names(sppListUlmusPast)=="PlantId"] <- "PastObserving"

dim(sppListUlmus); dim(sppListUlmusPast)

sppListUlmus <- merge(sppListUlmus, sppListUlmusPast, all=T)
sppListUlmus[is.na(sppListUlmus$PastObserving),]
sppListUlmus

colSums(sppListUlmus[, c("PotentialObserving", "PastObserving")], na.rm=T)


sppListAll[sppListAll$GardenLocalityName=="Ulmus",]
sppListAll[sppListAll$GardenLocalityName=="Ulmus" & sppListAll$PastObserving==0,]
sppListAll[sppListAll$GardenLocalityName=="Ulmus" & sppListAll$PastObserving>10,]

ggplot(data=treesCollections[treesCollections$GardenLocalityName=="Ulmus",]) +
  coord_equal() +
  geom_point(aes(x=Longitude, y=Latitude))


# Starting by taking everything with less than 9 trees (3 x 3)
ulmusTemp <- treesCollections[treesCollections$GardenLocalityName=="Ulmus" & treesCollections$SpeciesName %in% sppListAll$SpeciesName[sppListAll$GenusName=="Ulmus" & sppListAll$PotentialObserving<=9], ]

# For the species with lots of varieties & cultivars, lets dig deeper:
# If there are >3 varieties, monitor a max of 4 per varieties 
# If there's only 1 variety, randomly select 9 from what we've monitored before OR all we've monitored before plus a random set of what's left
ulmusSppBIG <- sppListAll$SpeciesName[sppListAll$GardenLocalityName=="Ulmus" & sppListAll$PotentialObserving>9]

dim(ulmusTemp)
for(SPP in ulmusSppBIG){
  sppSubset <- subsetTrees(dataRaw=treesCollections[treesCollections$GardenLocalityName=="Ulmus",], SPP=SPP, nTreeSpp=9, nTreeVar=4, dataOut=NULL, seed=1153)
  
  ulmusTemp <- rbind(ulmusTemp, sppSubset)
  
} # End SPP list
summary(ulmusTemp)

ulmus2023 <- data.frame(List=NA, PlantID = ulmusTemp$PlantId, 
                        Taxon = paste(ulmusTemp$GenusName, ulmusTemp$SpeciesName, sep=" "), 
                        Vernacular = ulmusTemp$VernacularName, 
                        BgLatitude = ulmusTemp$Latitude, 
                        BgLongitude = ulmusTemp$Longitude, 
                        GardenGrid = ulmusTemp$GardenSubarea1, 
                        GardenSubgrid = ulmusTemp$GardenSubarea2)
ulmus2023$Vernacular <- gsub("'", "", ulmus2023$Vernacular)
ulmus2023$Vernacular <- gsub('["]', '', ulmus2023$Vernacular)

unique(ulmus2023$Vernacular)
summary(ulmus2023)

set.seed(1435)

# May need to play around with the seed function to get things to look good, but this function should make it easier
ulmus2023 <- clusterTreesNew(datIn=ulmus2023, clusterMin=15, clusterMax=25, seed=1521)

# This passes the gut test
png(file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2023.png"), height=8, width=8, units="in", res=180)
ggplot(data=ulmus2023) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List))) +
  scale_shape_manual(values=c(1:18))
dev.off()

length(ulmus2023$List)
summary(as.factor(ulmus2023$List))

length(oldUlmus$Obs.List)
summary(as.factor(oldUlmus$Obs.List))


write.csv(ulmus2023, file=file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2023.csv"), row.names=F)

ulmus2023 <- read.csv(file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2023.csv"))

for(OBSLIST in unique(ulmus2023$List)){
  write.csv(ulmus2023[ulmus2023$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Ulmus", paste0("ObservingList_Ulmus_2023_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
  
}

# ~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~
# Quercus (366 potential trees; 213 in past) ----
# ~~~~~~~~~~~~~~~~
colSums(sppListAll[sppListAll$GardenLocalityName=="Quercus",c("PotentialObserving", "PastObserving")])

# We have a weird little cluster -- this is over by the W end of the plant breeding nursery; if we want to remove this, we should do so prior to the "quercusTemp" generation stage
ggplot(data=treesCollections[treesCollections$GardenLocalityName=="Quercus",]) +
  coord_equal() +
  geom_point(aes(x=Longitude, y=Latitude))

treesCollections[treesCollections$GardenLocalityName=="Quercus" & treesCollections$Longitude< -88.056,]

# Made Notes on Google sheet that I'll pull in; current rules were:
# 1) All trees outside of sections Quercus & Lobatae
# 2) All trees that are ranked as something other than "least concern" in the 2020 Red List of Oaks
# 3) All trees in the USA NPN List
# 4) No more than 9 trees of any species unless key varieties
# 5) no hybrids with only 1 individual to monitor or didn't have multiple individuals of parent species (maybe ditch hybrids all together)
# 6) No trees with only 1 tree to observe (if not part of above)
# 7) All remaining
oak.priorities <- googlesheets4::read_sheet(ss = "1KlXfLvtGSpOKvd1uJ-8etRFvbgyWDwiye_3d71d2w24", sheet = "Quercus")
colSums(oak.priorities[,c("PotentialObserving", "PastObserving", "2023 Goal")], na.rm=T)

# Creating a subset where we get rid of some weird oak groups
treesCollections[(treesCollections$GardenLocalityName=="Quercus" & treesCollections$Longitude< -88.055),]

# Looking at how much of Q. oglethorpensis we lose -- all except 3.  I think I'm okay with that
nrow(treesCollections[(treesCollections$GardenLocalityName=="Quercus" & treesCollections$Longitude< -88.055) & treesCollections$SpeciesName=="oglethorpensis",])
nrow(treesCollections[(treesCollections$GardenLocalityName=="Quercus" & treesCollections$Longitude> -88.055) & treesCollections$SpeciesName=="oglethorpensis",])


treesCollectionsQ <- treesCollections[treesCollections$GardenLocalityName=="Quercus" & treesCollections$Longitude> -88.055,]
ggplot(data=treesCollectionsQ[treesCollectionsQ$GardenLocalityName=="Quercus",]) +
  coord_equal() +
  geom_point(aes(x=Longitude, y=Latitude))


# Starting by taking everything with less than 9 trees (3 x 3)
quercusTemp <- treesCollectionsQ[treesCollectionsQ$GardenLocalityName=="Quercus" & treesCollectionsQ$SpeciesName %in% oak.priorities$SpeciesName[oak.priorities$`2023 Goal`>0 & oak.priorities$`2023 Goal`<9], ]
dim(quercusTemp)

# ggplot(data=quercusTemp[,]) +
#   coord_equal() +
#   geom_point(aes(x=Longitude, y=Latitude))

# For the species with lots of varieties & cultivars, lets dig deeper:
# If there are >3 varieties, monitor a max of 4 per varieties 
# If there's only 1 variety, randomly select 9 from what we've monitored before OR all we've monitored before plus a random set of what's left
QuercusSppBIG <- oak.priorities$SpeciesName[oak.priorities$`2023 Goal`>=9]
# QuercusSppBIG <- sppListAll$SpeciesName[sppListAll$GardenLocalityName=="Quercus" & sppListAll$PotentialObserving>9]
QuercusSppBIG

dim(quercusTemp)
for(SPP in QuercusSppBIG){
  # seed=1153
  sppSubset <- subsetTrees(dataRaw=treesCollectionsQ, SPP=SPP, nTreeSpp=9, stratVar = F, nTreeVar=2, dataOut=NULL, seed=1153)
  
  quercusTemp <- rbind(quercusTemp, sppSubset)
  
} # End SPP list
summary(quercusTemp)
dim(quercus)

quercus2023 <- data.frame(List=NA, PlantID = quercusTemp$PlantId, 
                        Taxon = paste(quercusTemp$GenusName, quercusTemp$SpeciesName, sep=" "), 
                        Vernacular = quercusTemp$VernacularName, 
                        BgLatitude = quercusTemp$Latitude, 
                        BgLongitude = quercusTemp$Longitude, 
                        GardenGrid = quercusTemp$GardenSubarea1, 
                        GardenSubgrid = quercusTemp$GardenSubarea2)
quercus2023$Vernacular <- gsub("'", "", quercus2023$Vernacular)
quercus2023$Vernacular <- gsub('["]', '', quercus2023$Vernacular)
unique(quercus2023$Vernacular)
summary(quercus2023)

# May need to play around with the seed function to get things to look good, but this function should make it easier
# Note: excluding the Q. oglethorpensis cluster and making that its own mini-list
# Pretty good: **303; new: 342; 1543; *1544
quercus2023 <- clusterTreesNew(datIn=quercus2023[,], clusterMin=15, clusterMax=25, seed=1546)
# quercusMain <- clusterTreesNew(datIn=quercus2023[quercus2023$BgLongitude>-88.056,], clusterMin=15, clusterMax=25, seed=303)
# quercusMini <- quercus2023[quercus2023$BgLongitude< -88.056,]
# quercusMini$List <- max(as.numeric(quercusMain$List))+1
# 1656
# dim(quercusMain)
# dim(quercusMini)

# quercus2023 <- rbind(quercusMain, quercusMini)
dim(quercus2023)


# This passes the gut test
png(file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2023.png"), height=8, width=8, units="in", res=180)
ggplot(data=quercus2023) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List))) +
  scale_shape_manual(values=c(1:18))
dev.off()

length(quercus2023$List)
summary(as.factor(quercus2023$List))

length(oldQuercus$Obs.List)
summary(as.factor(oldQuercus$Obs.List))


write.csv(quercus2023, file=file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2023.csv"), row.names=F)

quercus2023 <- read.csv(file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2023.csv"))

for(OBSLIST in unique(quercus2023$List)){
  write.csv(quercus2023[quercus2023$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Quercus", paste0("ObservingList_Quercus_2023_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
  
}

# ~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~
# Acer (223 potential species; 111 past) ----
# ~~~~~~~~~~~~~~~~
colSums(sppListAll[sppListAll$GardenLocalityName=="Acer",c("PotentialObserving", "PastObserving")])
sppListAll[sppListAll$GardenLocalityName=="Acer",c("PotentialObserving", "PastObserving")]


sppListAll[sppListAll$GardenLocalityName=="Acer",]
sppListAll[sppListAll$GardenLocalityName=="Acer" & sppListAll$PastObserving==0,]
sppListAll[sppListAll$GardenLocalityName=="Acer" & sppListAll$PastObserving>10,]

ggplot(data=treesCollections[treesCollections$GardenLocalityName=="Acer",]) +
  coord_equal() +
  geom_point(aes(x=Longitude, y=Latitude))


# Removing some trees that we know are hard to observe
treesCollections[treesCollections$GardenLocalityName=="Acer"  & treesCollections$Latitude>41.819,]

treesCollections[treesCollections$GardenLocalityName=="Acer"  & treesCollections$Latitude>41.818 & treesCollections$Longitude>-88.050,]

# Remove the first group; second is more questionable
treesCollectionsA <- treesCollections[treesCollections$GardenLocalityName=="Acer" & treesCollections$Latitude<41.819 & !(treesCollections$Latitude>41.818 & treesCollections$Longitude>-88.050) & !(treesCollections$Longitude< -88.053),]

# Starting by taking everything with less than 9 trees (3 x 3)
acerTemp <- treesCollectionsA[treesCollectionsA$GardenLocalityName=="Acer" & treesCollectionsA$SpeciesName %in% sppListAll$SpeciesName[sppListAll$PotentialObserving<=9], ]

# For the species with lots of varieties & cultivars, lets dig deeper:
# If there are >3 varieties, monitor a max of 4 per varieties 
# If there's only 1 variety, randomly select 9 from what we've monitored before OR all we've monitored before plus a random set of what's left
acerSppBIG <- sppListAll$SpeciesName[sppListAll$GardenLocalityName=="Acer" & sppListAll$PotentialObserving>9]

dim(acerTemp)
for(SPP in acerSppBIG){
  sppSubset <- subsetTrees(dataRaw=treesCollectionsA[treesCollectionsA$GardenLocalityName=="Acer",], SPP=SPP, nTreeSpp=9, nTreeVar=4, dataOut=NULL, seed=1153)
  
  acerTemp <- rbind(acerTemp, sppSubset)
  
} # End SPP list
summary(acerTemp)

acer2023 <- data.frame(List=NA, PlantID = acerTemp$PlantId, 
                        Taxon = paste(acerTemp$GenusName, acerTemp$SpeciesName, sep=" "), 
                        Vernacular = acerTemp$VernacularName, 
                        BgLatitude = acerTemp$Latitude, 
                        BgLongitude = acerTemp$Longitude, 
                        GardenGrid = acerTemp$GardenSubarea1, 
                        GardenSubgrid = acerTemp$GardenSubarea2)
acer2023$Vernacular <- gsub("'", "", acer2023$Vernacular)
acer2023$Vernacular <- gsub('["]', '', acer2023$Vernacular)

unique(acer2023$Vernacular)
summary(acer2023)

set.seed(1435)

# May need to play around with the seed function to get things to look good, but this function should make it easier
# works well: 1521
acer2023 <- clusterTreesNew(datIn=acer2023, clusterMin=15, clusterMax=25, seed=1521)

# This passes the gut test
png(file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2023.png"), height=8, width=8, units="in", res=180)
ggplot(data=acer2023) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List))) +
  scale_shape_manual(values=c(1:18))
dev.off()

length(acer2023$List)
summary(as.factor(acer2023$List))

length(oldAcer$Obs.List)
summary(as.factor(oldAcer$Obs.List))

write.csv(acer2023, file=file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2023.csv"), row.names=F)

acer2023 <- read.csv(file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2023.csv"))

for(OBSLIST in unique(acer2023$List)){
  write.csv(acer2023[acer2023$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Acer", paste0("ObservingList_Acer_2023_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
  
}
# ~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~
# Combine all lists and put them in the PhenoApp Folder
# ~~~~~~~~~~~~~~~~
listsAll <- rbind(quercus2023, acer2023, ulmus2023, tilia2023)

write.csv(listsAll, file=file.path(path.google, "Observing Lists", "ObservingList_AllCombined_2023.csv"), row.names=F)
write.csv(listsAll, file=file.path(path.google, "DataApp", "ObservingList_AllCombined_2023.csv"), row.names=F)

# ~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

