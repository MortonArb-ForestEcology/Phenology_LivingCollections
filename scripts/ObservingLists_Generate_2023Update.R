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
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in the our old observing lists for reference ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dir(file.path(path.google, "Observing Lists", "Quercus"))

oldQuercus <- read.csv(file.path(path.google, "Observing Lists", "Quercus/ObservingList_Quercus.csv"))
oldQuercus <- oldQuercus[,!names(oldQuercus) %in% "X"]
oldQuercus$GardenLocalityName <- "Quercus"

oldAcer <- read.csv(file.path(path.google, "Observing Lists", "Acer/ObservingList_Acer.csv"))
oldAcer <- oldAcer[,!names(oldAcer) %in% "X"]
oldAcer$GardenLocalityName <- "Acer"

oldUlmus <- read.csv(file.path(path.google, "Observing Lists", "Ulmus/ObservingList_Ulmus.csv"))
oldUlmus <- oldUlmus[,!names(oldUlmus) %in% "X"]
oldUlmus$GardenLocalityName <- "Ulmus"

oldTilia <- read.csv(file.path(path.google, "Observing Lists", "Tilia/ObservingList_Tilia.csv"))
oldTilia <- oldTilia[,!names(oldTilia) %in% "X"]
oldTilia$GardenLocalityName <- "Tilia"


# Now also grab the trees that had been removed
treesGone <- googlesheets4::read_sheet("16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ")

head(oldQuercus)
head(oldAcer)
head(oldUlmus)
head(oldTilia)

# Put all of the previous observing lists together and remove anything dead
oldAll <- rbind(oldQuercus, oldAcer, oldUlmus, oldTilia)
rm(oldQuercus, oldAcer, oldUlmus, oldTilia)

oldAll <- oldAll[!oldAll$PlantNumber %in% unique(treesGone$PlantNumber), ]
# names(oldAll)[names(oldAll) %in% c("BgLongitude", "BgLatitude")]
names(oldAll)
dim(oldAll)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in the main BRAHMS dataset: all living trees in the collections as of Jan 26 2023 ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
collectionsWant <- c("Quercus", "Acer", "Ulmus", "Tilia")

brahmsAll <- readxl::read_xlsx(file.path(path.google, "Observing Lists/MortonArb_BRHAMS_AllAliveTrees_2023-01-26.xlsx"))
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
summary(treesCollections)

# Doing a quick plot of what we were observing versus what we _could_ observe
png(file.path(path.google, "Observing Lists", "Update2023_BRAHMS_v_2022.png"), height=10, width=10, units="in", res=220)
ggplot(data=TreesCollections) +
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
nrow(oldAll); nrow(treesCollections); 
nrow(oldAll[oldAll$GardenLocalityName=="Quercus",]); nrow(treesCollections[treesCollections$GardenLocalityName=="Quercus",]) 
nrow(oldAll[oldAll$GardenLocalityName=="Acer",]); nrow(treesCollections[treesCollections$GardenLocalityName=="Acer",])
nrow(oldAll[oldAll$GardenLocalityName=="Ulmus",]); nrow(treesCollections[treesCollections$GardenLocalityName=="Ulmus",]) 
nrow(oldAll[oldAll$GardenLocalityName=="Tilia",]); nrow(treesCollections[treesCollections$GardenLocalityName=="Tilia",]) 

# Comparing number of taxa we've been observing vs. what's in our collections
length(unique(oldAll$Taxon[oldAll$GardenLocalityName=="Quercus"])); length(unique(treesCollections$SpeciesName[treesCollections$GardenLocalityName=="Quercus"])); length(unique(treesCollections$CalcFullName[treesCollections$GardenLocalityName=="Quercus"]))

length(unique(oldAll$Taxon[oldAll$GardenLocalityName=="Acer"])); length(unique(treesCollections$SpeciesName[treesCollections$GardenLocalityName=="Acer"])); length(unique(treesCollections$CalcFullName[treesCollections$GardenLocalityName=="Acer"]))

length(unique(oldAll$Taxon[oldAll$GardenLocalityName=="Ulmus"])); length(unique(treesCollections$SpeciesName[treesCollections$GardenLocalityName=="Ulmus"])); length(unique(treesCollections$CalcFullName[treesCollections$GardenLocalityName=="Ulmus"]))

length(unique(oldAll$Taxon[oldAll$GardenLocalityName=="Tilia"])); length(unique(treesCollections$SpeciesName[treesCollections$GardenLocalityName=="Tilia"])); length(unique(treesCollections$CalcFullName[treesCollections$GardenLocalityName=="Tilia"]))

oldAll[oldAll$GardenLocalityName=="Tilia",]
treesCollections[treesCollections$GardenLocalityName=="Tilia", c("PlantId", "LivingStatus", "CalcFullName", "GenusName", "SpeciesName")]

names(oldAll)

QuercusCollAgg <- aggregate(PlantId ~ GenusName + SpeciesName, data=treesCollections[treesCollections$GardenLocalityName=="Quercus",], FUN=length)
AcerCollAgg <- aggregate(PlantId ~ GenusName + SpeciesName, data=treesCollections[treesCollections$GardenLocalityName=="Acer",], FUN=length)
UlmusCollAgg <- aggregate(PlantId ~ GenusName + SpeciesName, data=treesCollections[treesCollections$GardenLocalityName=="Ulmus",], FUN=length)
TiliaCollAgg <- aggregate(PlantId ~ GenusName + SpeciesName, data=treesCollections[treesCollections$GardenLocalityName=="Tilia",], FUN=length)

write.csv(QuercusCollAgg, file.path(path.google, "Observing Lists", "Update2023_BRAHMS_PotentialSpecies_Quercus.csv"), row.names=F)
write.csv(AcerCollAgg, file.path(path.google, "Observing Lists", "Update2023_BRAHMS_PotentialSpecies_Acer.csv"), row.names=F)
write.csv(UlmusCollAgg, file.path(path.google, "Observing Lists", "Update2023_BRAHMS_PotentialSpecies_Ulmus.csv"), row.names=F)
write.csv(TiliaCollAgg, file.path(path.google, "Observing Lists", "Update2023_BRAHMS_PotentialSpecies_Tilia.csv"), row.names=F)

oakPhenoAgg <- aggregate(PlantNumber ~ Taxon, data=oldAll[oldAll$GardenLocalityName=="Quercus",], FUN=length)

sum(oakCollAgg$PlantId)
sum(oakPhenoAgg$PlantNumber)

length(which(is.na(treesCollections$SpeciesName[treesCollections$GardenLocalityName=="Quercus"])))
sum(ifelse(QuercusCollAgg$PlantId>10, 8, QuercusCollAgg$PlantId))
sum(oakPhenoAgg$PlantNumber)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

