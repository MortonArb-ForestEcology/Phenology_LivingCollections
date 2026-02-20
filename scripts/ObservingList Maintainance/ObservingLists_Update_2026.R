library(ggplot2)

# Update the most recent round of observing lists to make sure removed trees are no longer listed
path.google <- "~/Google Drive/My Drive/LivingCollections_Phenology/"

# 1. Read in the existing lists --> ALWAYS START WITH THIS ----
quercusObs <- read.csv(file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2025.csv"))
acerObs <- read.csv(file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2025.csv"))
ulmusObs <- read.csv(file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2025.csv"))

# summary(quercusObs)
dim(quercusObs); dim(acerObs); dim(ulmusObs)
# Quercus: 221, Acer: 165, Ulmus: 140

# 2. remove trees no longer in BOL ----
quercusAll <- readxl::read_xlsx(file.path(path.google, "Observing Lists", "2026-02-20_BRAHMSOnlineData_Quercus.xlsx"))
acerAll <- readxl::read_xlsx(file.path(path.google, "Observing Lists", "2026-02-20_BRAHMSOnlineData_Acer.xlsx"))
ulmusAll <- readxl::read_xlsx(file.path(path.google, "Observing Lists", "2026-02-20_BRAHMSOnlineData_Ulmus.xlsx"))

quercusObs <- quercusObs[quercusObs$PlantID %in% quercusAll$PlantNumber,]
acerObs <- acerObs[acerObs$PlantID %in% acerAll$PlantNumber,]
ulmusObs <- ulmusObs[ulmusObs$PlantID %in% ulmusAll$PlantNumber,]

dim(quercusObs); dim(acerObs); dim(ulmusObs)
# Quercus: 220, Acer: 162, Ulmus: 135


# 3. Check for removed trees ----
#    3.a. Pull our existing removed trees list -----
googlesheets4::gs4_auth(email="breidy@mortonarb.org")
removed <- googlesheets4::read_sheet("16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ", sheet="Removed Trees")
names(removed)

quercusObs <- quercusObs[!quercusObs$PlantID %in% removed$PlantNumber,]
acerObs <- acerObs[!acerObs$PlantID %in% removed$PlantNumber,]
ulmusObs <- ulmusObs[!ulmusObs$PlantID %in% removed$PlantNumber,]

dim(quercusObs); dim(acerObs); dim(ulmusObs)
# Quercus: 220, Acer: 161, Ulmus: 134

#    3.b. Search last year's data for trees that say they're gone;  -----
quercus25 <- read.csv(file.path(path.google, "Data_observations/LivingCollectionPhenology_ObservationData_Quercus_2025_FINAL.csv"))
quercus25$Date.Observed <- as.Date(quercus25$Date.Observed)
summary(quercus25)

acer25 <- read.csv(file.path(path.google, "Data_observations/LivingCollectionPhenology_ObservationData_Acer_2025_FINAL.csv"))
acer25$Date.Observed <- as.Date(acer25$Date.Observed)
summary(acer25)

ulmus25 <- read.csv(file.path(path.google, "Data_observations/LivingCollectionPhenology_ObservationData_Ulmus_2025_FINAL.csv"))
ulmus25$Date.Observed <- as.Date(ulmus25$Date.Observed)
summary(ulmus25)

# Searching for a bunch of strings that people could use to indicate something is gone/dead/etc. -----
quercusRem <- unique(quercus25$PlantNumber[grep("removed", quercus25$Notes, ignore.case=T)])
quercusGo <- unique(quercus25$PlantNumber[grep("gone", quercus25$Notes, ignore.case=T)])
quercusDead <- unique(quercus25$PlantNumber[grep("dead", quercus25$Notes, ignore.case=T)])
quercusMiss <- unique(quercus25$PlantNumber[grep("missing", quercus25$Notes, ignore.case=T)])
quercusGone <- c(quercusRem, quercusGo, quercusDead, quercusMiss)

acerRem <- unique(acer25$PlantNumber[grep("removed", acer25$Notes, ignore.case=T)])
acerGo <- unique(acer25$PlantNumber[grep("gone", acer25$Notes, ignore.case=T)])
acerDead <- unique(acer25$PlantNumber[grep("dead", acer25$Notes, ignore.case=T)])
acerMiss <- unique(acer25$PlantNumber[grep("missing", acer25$Notes, ignore.case=T)])
acerGone <- c(acerRem, acerGo, acerDead, acerMiss)

ulmusRem <- unique(ulmus25$PlantNumber[grep("removed", ulmus25$Notes, ignore.case=T)])
ulmusGo <- unique(ulmus25$PlantNumber[grep("gone", ulmus25$Notes, ignore.case=T)])
ulmusDead <- unique(ulmus25$PlantNumber[grep("dead", ulmus25$Notes, ignore.case=T)])
ulmusMiss <- unique(ulmus25$PlantNumber[grep("missing", ulmus25$Notes, ignore.case=T)])
ulmusGone <- c(ulmusRem, ulmusGo, ulmusDead, ulmusMiss)



# Checking to see which of these are still in our observing lists -----
quercusGone <- quercusGone[quercusGone %in% quercusObs$PlantID]
acerGone <- acerGone[acerGone %in% acerObs$PlantID]
ulmusGone <- ulmusGone[ulmusGone %in% ulmusObs$PlantID]

# Making sure these didn't actually have any observations after mid-summer in 2025 -----
quercusGoneCt <- data.frame(PlantNumber=quercusGone, LeafObsSept=NA)
for(i in 1:nrow(quercusGoneCt)){
  quercusGoneCt$LeafObsSept[i] = length(which(quercus25$leaf.present.observed[quercus25$PlantNumber %in% quercusGone[i] & quercus25$Date.Observed>as.Date("2025-08-01")]=="Yes")
  )
}
quercusGoneCt

quercusGone <- quercusGone[quercusGoneCt$LeafObsSept<2] # Only count it as gone if there are one or fewer observations after August
quercusGone

acerGoneCt <- data.frame(PlantNumber=acerGone, LeafObsSept=NA)
for(i in 1:nrow(acerGoneCt)){
  acerGoneCt$LeafObsSept[i] = length(which(acer25$leaf.present.observed[acer25$PlantNumber %in% acerGone[i] & acer25$Date.Observed>as.Date("2025-08-01")]=="Yes")
  )
}
acerGoneCt

acerGone <- acerGone[acerGoneCt$LeafObsSept<2] # Only count it as gone if there are one or fewer observations after August
acerGone

ulmusGoneCt <- data.frame(PlantNumber=ulmusGone, LeafObsSept=NA)
for(i in 1:nrow(ulmusGoneCt)){
  ulmusGoneCt$LeafObsSept[i] = length(which(ulmus25$leaf.present.observed[ulmus25$PlantNumber %in% ulmusGone[i] & ulmus25$Date.Observed>as.Date("2025-08-01")]=="Yes")
  )
}
ulmusGoneCt

ulmusGone <- ulmusGone[ulmusGoneCt$LeafObsSept<2] # Only count it as gone if there are one or fewer observations after August
ulmusGone

# Remove form-reported trees from the observing lists
quercusObs <- quercusObs[!quercusObs$PlantID %in% quercusGone,]
acerObs <- acerObs[!acerObs$PlantID %in% acerGone,]
ulmusObs <- ulmusObs[!ulmusObs$PlantID %in% ulmusGone,]

dim(quercusObs); dim(acerObs); dim(ulmusObs)
# Quercus: 215, Acer: 138, Ulmus: 133

length(unique(quercusObs$Taxon))
length(unique(acerObs$Taxon))
length(unique(ulmusObs$Taxon))
# Quercus: 52, Acer: 47, Ulmus:28


#####################################
# creating quick lists of trees that were dropped
#####################################
quercusOrig <- read.csv(file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2025.csv"))
acerOrig <- read.csv(file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2025.csv"))
ulmusOrig <- read.csv(file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2025.csv"))

quercusDrop <- quercusOrig[!quercusOrig$PlantID %in% quercusObs$PlantID,]
acerDrop <- acerOrig[!acerOrig$PlantID %in% acerObs$PlantID,]
ulmusDrop <- ulmusOrig[!ulmusOrig$PlantID %in% ulmusObs$PlantID,]

dim(quercusDrop); dim(acerDrop); dim(ulmusDrop)
#Quercus: 6, Acer:7, Ulmus:7
write.csv(quercusDrop, file=file.path(path.google, "Observing Lists", "ObservingList_QuercusDropped_2025.csv"), row.names=F)
write.csv(acerDrop, file=file.path(path.google, "Observing Lists", "ObservingList_AcerDropped_2025.csv"), row.names=F)
write.csv(ulmusDrop, file=file.path(path.google, "Observing Lists", "ObservingList_UlmusDropped_2025.csv"), row.names=F)


#####################################
# 4. Update the existing lists to remove trees that we know are now gone ----
#####################################
# Check the Distribution of Trees (small list = <15 trees)
summary(as.factor(quercusObs$List)) # Small List: List 7 (9 trees); List 4 (11 trees); List 1 (12 trees)
summary(as.factor(acerObs$List)) # Small List: List 2 (10 trees), List 1 (13 trees); List 5 (14 trees), List 7 (14 trees) 
summary(as.factor(ulmusObs$List)) # No small lists 

png(file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2026.png"), height=8, width=8, units="in", res=180)
ggplot(data=quercusObs) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List))) +
  scale_shape_manual(values=c(1:18))
dev.off()


png(file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2026.png"), height=8, width=8, units="in", res=180)
ggplot(data=acerObs) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List))) +
  scale_shape_manual(values=c(1:18))
dev.off()


png(file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2026.png"), height=8, width=8, units="in", res=180)
ggplot(data=ulmusObs) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List))) +
  scale_shape_manual(values=c(1:18))
dev.off()


write.csv(quercusObs, file=file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2026.csv"), row.names=F)
write.csv(acerObs, file=file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2026.csv"), row.names=F)
write.csv(ulmusObs, file=file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2026.csv"), row.names=F)

for(OBSLIST in unique(quercusObs$List)){
  write.csv(quercusObs[quercusObs$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Quercus", paste0("ObservingList_Quercus_2026_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
}

for(OBSLIST in unique(acerObs$List)){
  write.csv(acerObs[acerObs$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Acer", paste0("ObservingList_Acer_2026_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
}

for(OBSLIST in unique(ulmusObs$List)){
  write.csv(ulmusObs[ulmusObs$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Ulmus", paste0("ObservingList_Ulmus_2026_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
}

# ~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~
# Combine all lists and put them in the PhenoApp Folder
# ~~~~~~~~~~~~~~~~
listsAll <- rbind(quercusObs, acerObs, ulmusObs)

write.csv(listsAll, file=file.path(path.google, "Observing Lists", "ObservingList_AllCombined_2026.csv"), row.names=F)
write.csv(listsAll, file=file.path(path.google, "DataApp", "ObservingList_AllCombined_2026.csv"), row.names=F)



# 4.2. Move the 2025 lists to the "OLD" folder
quercus25 <- dir(file.path(path.google, "Observing Lists", "Quercus"), "2025")
for(i in 1:length(quercus25)){
  file.copy(from=file.path(path.google, "Observing Lists", "Quercus", quercus25[i]),
            to=file.path(path.google, "Observing Lists", "OLD", "Quercus", quercus25[i]))
  
  file.remove(file.path(path.google, "Observing Lists", "Quercus", quercus25[i]))
}


acer25 <- dir(file.path(path.google, "Observing Lists", "Acer"), "2025")
for(i in 1:length(acer25)){
  file.copy(from=file.path(path.google, "Observing Lists", "Acer", acer25[i]),
            to=file.path(path.google, "Observing Lists", "OLD", "Acer", acer25[i]))
  
  file.remove(file.path(path.google, "Observing Lists", "Acer", acer25[i]))
}

ulmus25 <- dir(file.path(path.google, "Observing Lists", "Ulmus"), "2025")
for(i in 1:length(ulmus25)){
  file.copy(from=file.path(path.google, "Observing Lists", "Ulmus", ulmus25[i]),
            to=file.path(path.google, "Observing Lists", "OLD", "Ulmus", ulmus25[i]))
  
  file.remove(file.path(path.google, "Observing Lists", "Ulmus", ulmus25[i]))
}
