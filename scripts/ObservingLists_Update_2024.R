library(ggplot2)

# Update the most recent round of observing lists to make sure removed trees are no longer listed
path.google <- "~/Google Drive/My Drive/LivingCollections_Phenology/"

# 1. Read in the existing lists --> ALWAYS START WITH THIS ----
quercusObs <- read.csv(file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2023.csv"))
acerObs <- read.csv(file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2023.csv"))
ulmusObs <- read.csv(file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2023.csv"))

# summary(quercusObs)
dim(quercusObs); dim(acerObs); dim(ulmusObs)
# Quercus: 285, Acer: 191, Ulmus: 142

# 2. remove trees no longer in BOL ----
quercusAll <- readxl::read_xlsx(file.path(path.google, "Observing Lists", "2024-02-27_BRAHMSOnlineData_Quercus.xlsx"))
acerAll <- readxl::read_xlsx(file.path(path.google, "Observing Lists", "2024-02-27_BRAHMSOnlineData_Acer.xlsx"))
ulmusAll <- readxl::read_xlsx(file.path(path.google, "Observing Lists", "2024-02-27_BRAHMSOnlineData_Ulmus.xlsx"))

quercusObs <- quercusObs[quercusObs$PlantID %in% quercusAll$PlantNumber,]
acerObs <- acerObs[acerObs$PlantID %in% acerAll$PlantNumber,]
ulmusObs <- ulmusObs[ulmusObs$PlantID %in% ulmusAll$PlantNumber,]

dim(quercusObs); dim(acerObs); dim(ulmusObs)
# Quercus: 236, Acer: 183, Ulmus: 142


# 3. Check for removed trees ----
#    3.a. Pull our existing removed trees list -----
googlesheets4::gs4_auth(email="crollinson@mortonarb.org")
removed <- googlesheets4::read_sheet("16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ", sheet="Removed Trees")
names(removed)

quercusObs <- quercusObs[!quercusObs$PlantID %in% removed$PlantNumber,]
acerObs <- acerObs[!acerObs$PlantID %in% removed$PlantNumber,]
ulmusObs <- ulmusObs[!ulmusObs$PlantID %in% removed$PlantNumber,]

dim(quercusObs); dim(acerObs); dim(ulmusObs)
# Quercus: 235, Acer: 177, Ulmus: 140

#    3.b. Search last year's data for trees that say they're gone;  -----
quercus23 <- read.csv(file.path(path.google, "Data_observations/LivingCollectionPhenology_ObservationData_Quercus_2023_FINAL.csv"))
quercus23$Date.Observed <- as.Date(quercus23$Date.Observed)
summary(quercus23)

acer23 <- read.csv(file.path(path.google, "Data_observations/LivingCollectionPhenology_ObservationData_Acer_2023_FINAL.csv"))
acer23$Date.Observed <- as.Date(acer23$Date.Observed)
summary(acer23)

ulmus23 <- read.csv(file.path(path.google, "Data_observations/LivingCollectionPhenology_ObservationData_Ulmus_2023_FINAL.csv"))
ulmus23$Date.Observed <- as.Date(ulmus23$Date.Observed)
summary(ulmus23)

# Searching for a bunch of strings that people could use to indicate something is gone/dead/etc. -----
quercusRem <- unique(quercus23$PlantNumber[grep("removed", quercus23$Notes, ignore.case=T)])
quercusGo <- unique(quercus23$PlantNumber[grep("gone", quercus23$Notes, ignore.case=T)])
quercusDead <- unique(quercus23$PlantNumber[grep("dead", quercus23$Notes, ignore.case=T)])
quercusMiss <- unique(quercus23$PlantNumber[grep("missing", quercus23$Notes, ignore.case=T)])
quercusGone <- c(quercusRem, quercusGo, quercusDead, quercusMiss)

acerRem <- unique(acer23$PlantNumber[grep("removed", acer23$Notes, ignore.case=T)])
acerGo <- unique(acer23$PlantNumber[grep("gone", acer23$Notes, ignore.case=T)])
acerDead <- unique(acer23$PlantNumber[grep("dead", acer23$Notes, ignore.case=T)])
acerMiss <- unique(acer23$PlantNumber[grep("missing", acer23$Notes, ignore.case=T)])
acerGone <- c(acerRem, acerGo, acerDead, acerMiss)

ulmusRem <- unique(ulmus23$PlantNumber[grep("removed", ulmus23$Notes, ignore.case=T)])
ulmusGo <- unique(ulmus23$PlantNumber[grep("gone", ulmus23$Notes, ignore.case=T)])
ulmusDead <- unique(ulmus23$PlantNumber[grep("dead", ulmus23$Notes, ignore.case=T)])
ulmusMiss <- unique(ulmus23$PlantNumber[grep("missing", ulmus23$Notes, ignore.case=T)])
ulmusGone <- c(ulmusRem, ulmusGo, ulmusDead, ulmusMiss)



# Checking to see which of these are still in our observing lists -----
quercusGone <- quercusGone[quercusGone %in% quercusObs$PlantID]
acerGone <- acerGone[acerGone %in% acerObs$PlantID]
ulmusGone <- ulmusGone[ulmusGone %in% ulmusObs$PlantID]

# Making sure these didn't actually have any observations after mid-summer in 2023 -----
quercusGoneCt <- data.frame(PlantNumber=quercusGone, LeafObsSept=NA)
for(i in 1:nrow(quercusGoneCt)){
  quercusGoneCt$LeafObsSept[i] = length(which(quercus23$leaf.present.observed[quercus23$PlantNumber %in% quercusGone[i] & quercus23$Date.Observed>as.Date("2023-08-01")]=="Yes")
)
}
quercusGoneCt

quercusGone <- quercusGone[quercusGoneCt$LeafObsSept<3] # Only count it as gone if there are one or fewer observations after August
quercusGone

acerGoneCt <- data.frame(PlantNumber=acerGone, LeafObsSept=NA)
for(i in 1:nrow(acerGoneCt)){
  acerGoneCt$LeafObsSept[i] = length(which(acer23$leaf.present.observed[acer23$PlantNumber %in% acerGone[i] & acer23$Date.Observed>as.Date("2023-08-01")]=="Yes")
  )
}
acerGoneCt

acerGone <- acerGone[acerGoneCt$LeafObsSept<3] # Only count it as gone if there are one or fewer observations after August
acerGone

ulmusGoneCt <- data.frame(PlantNumber=ulmusGone, LeafObsSept=NA)
for(i in 1:nrow(ulmusGoneCt)){
  ulmusGoneCt$LeafObsSept[i] = length(which(ulmus23$leaf.present.observed[ulmus23$PlantNumber %in% ulmusGone[i] & ulmus23$Date.Observed>as.Date("2023-08-01")]=="Yes")
  )
}
ulmusGoneCt

ulmusGone <- ulmusGone[ulmusGoneCt$LeafObsSept<3] # Only count it as gone if there are one or fewer observations after August
ulmusGone

# Remove form-reported trees from the observing lists
quercusObs <- quercusObs[!quercusObs$PlantID %in% quercusGone,]
acerObs <- acerObs[!acerObs$PlantID %in% acerGone,]
ulmusObs <- ulmusObs[!ulmusObs$PlantID %in% ulmusGone,]

dim(quercusObs); dim(acerObs); dim(ulmusObs)

length(unique(quercusObs$Taxon))
length(unique(acerObs$Taxon))
length(unique(ulmusObs$Taxon))
# Quercus: 230, Acer: 170, Ulmus: 140


#####################################
# 4. Update the existing lists to remove trees that we know are now gone ----
#####################################
# Check the Distribution of Trees (small list = <15 trees)
summary(as.factor(quercusObs$List)) # Small List: List 7 (9 trees); List 4 (11 trees); List 1 (12 trees)
summary(as.factor(acerObs$List)) # Small List: List 2 (10 trees), List 1 (13 trees); List 5 (14 trees), List 7 (14 trees) 
summary(as.factor(ulmusObs$List)) # No small lists 

png(file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2024.png"), height=8, width=8, units="in", res=180)
ggplot(data=quercusObs) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List))) +
  scale_shape_manual(values=c(1:18))
dev.off()


png(file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2024.png"), height=8, width=8, units="in", res=180)
ggplot(data=acerObs) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List))) +
  scale_shape_manual(values=c(1:18))
dev.off()


png(file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2024.png"), height=8, width=8, units="in", res=180)
ggplot(data=ulmusObs) +
  coord_equal() +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=as.factor(List), shape=as.factor(List))) +
  scale_shape_manual(values=c(1:18))
dev.off()


write.csv(quercusObs, file=file.path(path.google, "Observing Lists", "Quercus", "ObservingList_Quercus_2024.csv"), row.names=F)
write.csv(acerObs, file=file.path(path.google, "Observing Lists", "Acer", "ObservingList_Acer_2024.csv"), row.names=F)
write.csv(ulmusObs, file=file.path(path.google, "Observing Lists", "Ulmus", "ObservingList_Ulmus_2024.csv"), row.names=F)

for(OBSLIST in unique(quercusObs$List)){
  write.csv(quercusObs[quercusObs$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Quercus", paste0("ObservingList_Quercus_2024_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
}

for(OBSLIST in unique(acerObs$List)){
  write.csv(acerObs[acerObs$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Acer", paste0("ObservingList_Acer_2024_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
}

for(OBSLIST in unique(ulmusObs$List)){
  write.csv(ulmusObs[ulmusObs$List==OBSLIST,], file=file.path(path.google, "Observing Lists", "Ulmus", paste0("ObservingList_Ulmus_2024_LIST-", stringr::str_pad(OBSLIST, 2, "left", "0"), ".csv")), row.names=F)
}

# ~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~
# Combine all lists and put them in the PhenoApp Folder
# ~~~~~~~~~~~~~~~~
listsAll <- rbind(quercusObs, acerObs, ulmusObs)

write.csv(listsAll, file=file.path(path.google, "Observing Lists", "ObservingList_AllCombined_2024.csv"), row.names=F)
write.csv(listsAll, file=file.path(path.google, "DataApp", "ObservingList_AllCombined_2024.csv"), row.names=F)



# 4.2. Move the 2023 lists to the "OLD" folder
quercus23 <- dir(file.path(path.google, "Observing Lists", "Quercus"), "2023")
for(i in 1:length(quercus23)){
  file.copy(from=file.path(path.google, "Observing Lists", "Quercus", quercus23[i]),
            to=file.path(path.google, "Observing Lists", "OLD", "Quercus", quercus23[i]))
  
  file.remove(file.path(path.google, "Observing Lists", "Quercus", quercus23[i]))
}


acer23 <- dir(file.path(path.google, "Observing Lists", "Acer"), "2023")
for(i in 1:length(acer23)){
  file.copy(from=file.path(path.google, "Observing Lists", "Acer", acer23[i]),
            to=file.path(path.google, "Observing Lists", "OLD", "Acer", acer23[i]))
  
  file.remove(file.path(path.google, "Observing Lists", "Acer", acer23[i]))
}

ulmus23 <- dir(file.path(path.google, "Observing Lists", "Ulmus"), "2023")
for(i in 1:length(ulmus23)){
  file.copy(from=file.path(path.google, "Observing Lists", "Ulmus", ulmus23[i]),
            to=file.path(path.google, "Observing Lists", "OLD", "Ulmus", ulmus23[i]))
  
  file.remove(file.path(path.google, "Observing Lists", "Ulmus", ulmus23[i]))
}
