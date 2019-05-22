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
# path.gis <- "/Volumes/GIS/Collections" # Path on a Mac
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
dat.all$fruit.drop.intensity <- as.factor(dat.all$fruit.drop.intensity)
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
dat.all$group1 <- as.factor(dat.all$group1)
dat.all$collection <- as.factor(dat.all$collection)
summary(dat.all)

length(unique(dat.all$Species[dat.all$collection=="Quercus"]))
length(unique(dat.all$PlantNumber[dat.all$collection=="Quercus"]))
# -------------------------------------------------------------
