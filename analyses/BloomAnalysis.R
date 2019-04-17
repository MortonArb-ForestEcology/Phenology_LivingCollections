# Analyzing peak bloom data from Ed Hedborn
library(readxl); library(ggplot2)

# ------------------------------------
# Reading in and doing some quick summaries
# ------------------------------------
path.dat <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/BloomData_Hedborn/BRAHMS-FULL-BLOOM-EXTRACT_2019-04-15.xlsx"
dat.bloom <- data.frame(read_excel(path.dat, sheet = "Sheet1"))
dat.bloom <- dat.bloom[,1:8]
dat.bloom$PlantID <- as.factor(dat.bloom$PlantID)
dat.bloom$FullName <- as.factor(dat.bloom$FullName)
dat.bloom$Collection <- as.factor(dat.bloom$Collection)
dat.bloom$FlowerStage <- as.factor(dat.bloom$FlowerStage)
dat.bloom$Year <- lubridate::year(dat.bloom$Date)
dat.bloom$DOY <- lubridate::yday(dat.bloom$Date)
summary(dat.bloom)

# Getting a rough genus & species
name.summary <- summary(dat.bloom$FullName)
name.summary[1:10]

# Loop through and give just the first 2 assignments as a genus and species
bloom.spp <- stringr::str_split(dat.bloom$FullName, " ")
for(i in 1:nrow(dat.bloom)){
  dat.bloom[i,"Genus"] <- bloom.spp[[i]][1]
  dat.bloom[i,"Species"] <- bloom.spp[[i]][2]
}
dat.bloom$Genus <- as.factor(dat.bloom$Genus)
dat.bloom$Species <- as.factor(dat.bloom$Species)
dat.bloom$Taxon <- as.factor(paste(dat.bloom$Genus, dat.bloom$Species, sep=" "))
summary(dat.bloom)

length(unique(dat.bloom$FullName))
length(unique(dat.bloom$Taxon))
tax.summary <- summary(dat.bloom$Taxon)
gen.summary <- summary(dat.bloom$Genus)
tax.summary[1:10]
gen.summary[1:10]

length(which(name.summary>200))
name.summary[which(name.summary>200)]

length(which(tax.summary>500))
tax.summary[which(tax.summary>400)]
# ------------------------------------


# ------------------------------------
# Analyzing change in a few key species
# -- Cercis canadensis
# -- Cornus mas
# -- Amelanchier sanguinea
# -- Vinca minor
# -- Hydrangea quercifolia
# ------------------------------------
library(nlme)
# Doing a general trend analysis
mod.bloom <- lme(DOY ~ Year, random=list(Taxon=~1, FullName=~1, PlantID=~1), data=dat.bloom[!is.na(dat.bloom$Year),])
summary(mod.bloom)

summary(dat.bloom[dat.bloom$Taxon=="Cercis canadensis",])
mod.ceca <- lme(DOY ~ Year, random=list(PlantID=~1), data=dat.bloom[dat.bloom$Taxon=="Cercis canadensis",])
summary(mod.ceca)
22*.2
# ------------------------------------
