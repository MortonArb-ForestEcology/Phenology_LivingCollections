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
#----------------------------


#----------------------------
# QAQC sanity checks: Check for the following
#----------------------------
# Observation dates prior to the year we're observing
# Oberservation dates after todays date
# Missing plant numbers
#----------------------------
summary(dat.all)
dat.all[is.na(dat.all$PlantNumber),]
# summary(dat.all[dat.all$Species=="Quercus macrocarpa" & dat.all$Observer=="Dorrell",])

dat.all[dat.all$Date.Observed>Sys.Date(),] # Check for anything observed in future
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

# Checking some oddballs
pheno.now[pheno.now$fruit.ripe.observed=="Yes",]
# summary(pheno.now[pheno.now$leaf.buds.observed=="Yes",])
summary(pheno.now[pheno.now$leaf.buds.observed=="Yes" & pheno.now$collection=="Quercus",])
summary(pheno.now[pheno.now$leaf.present.observed=="No",])

summary(pheno.now[pheno.now$leaf.buds.observed=="No" & pheno.now$leaf.present.observed=="No",])

summary(pheno.now[pheno.now$leaf.present.observed=="Yes",])
summary(pheno.now[pheno.now$leaf.present.observed=="Yes",])

# Checking for likely mis-entries
pheno.now[pheno.now$leaf.buds.observed %in% c("No", "Did not look for") & !pheno.now$leaf.buds.intensity %in% c("0", NA),]
pheno.now[pheno.now$leaf.present.observed %in% c("No", "Did not look for") & !pheno.now$leaf.present.intensity %in% c("0%", NA),]
pheno.now[pheno.now$leaf.increasing.observed %in% c("No", "Did not look for") & !pheno.now$leaf.increasing.intensity %in% c("0%", NA),]
pheno.now[pheno.now$leaf.color.observed %in% c("No", "Did not look for") & !pheno.now$leaf.color.intensity %in% c("0%", NA),]
pheno.now[!pheno.now$leaf.falling.observed %in% c("No", "Did not look for"),]

pheno.now[pheno.now$flower.buds.observed %in% c("No", "Did not look for") & !pheno.now$flower.buds.intensity %in% c("0", NA),]
pheno.now[pheno.now$flower.open.observed %in% c("No", "Did not look for") & !pheno.now$flower.open.intensity %in% c("0%", NA),]
pheno.now[pheno.now$flower.pollen.observed %in% c("No", "Did not look for") & !pheno.now$flower.pollen.intensity %in% c("None", NA),]
pheno.now[pheno.now$fruit.present.observed %in% c("No", "Did not look for") & !pheno.now$fruit.present.intensity %in% c("0", NA),]
pheno.now[pheno.now$fruit.ripe.observed %in% c("No", "Did not look for") & !pheno.now$fruit.ripe.intensity %in% c("0%", NA),]
pheno.now[pheno.now$fruit.drop.observed %in% c("No", "Did not look for") & !pheno.now$fruit.drop.intensity %in% c("0", NA),]

summary(pheno.now[!pheno.now$fruit.present.observed %in% c("No", "Did not look for") ,])
pheno.now[!pheno.now$fruit.ripe.intensity %in% c(NA, "0%"),]

pheno.now[! pheno.now$leaf.color.intensity %in% c(NA, "0%"),]
summary(pheno.now[!pheno.now$flower.pollen.intensity %in% c(NA, "None") & pheno.now$collection=="Quercus",])


pheno.leaf <- names(pheno.now)[grep("leaf", names(pheno.now))]
pheno.flower <- names(pheno.now)[grep("flower", names(pheno.now))]
pheno.fruit <- names(pheno.now)[grep("fruit", names(pheno.now))]

pheno.table <- data.frame(stringr::str_split(c(pheno.leaf, pheno.flower, pheno.fruit), "[.]", simplify=T))
names(pheno.table) <- c("category", "phase", "type")

pdf(file.path(dir.base, "Data_Observations/data_QAQC", paste0("Phenology_LivingCollections_QAQC_", Sys.Date(), ".pdf")), width=11, height=8.5)
for(CAT in unique(pheno.table$category)){
 for(PHASE in unique(pheno.table$phase[pheno.table$category==CAT])){
   # dat.tmp <- pheno.now[pheno.now$Status=="Past Week",]
   dat.tmp <- pheno.now[,]
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
     # geom_histogram(aes(x=obs), stat="count") +
     geom_histogram(aes(x=obs, fill=Status), stat="count") +
     scale_x_discrete(name="Observed") +
     ggtitle(paste(CAT, PHASE, "Observed")) +
     theme_bw() +
     theme(legend.position="top")
   
   plot.intensity <- ggplot(data=dat.tmp) +
     facet_grid(collection~., scales="free_y") +
     # geom_histogram(aes(x=int), stat="count") +
     geom_histogram(aes(x=int, fill=Status), stat="count") +
     scale_x_discrete(name="Intensity") +
     ggtitle(paste(CAT, PHASE, "Intensity")) +
     theme_bw() +
     theme(legend.position="top")
   
   print(cowplot::plot_grid(plot.status, plot.intensity, ncol=2))
   
 } 
}
dev.off()


# Doing some more QAQC
summary(droplevels(pheno.now[pheno.now$collection=="Quercus" & pheno.now$leaf.buds.observed=="Yes",]))
pheno.now[pheno.now$collection=="Quercus" & pheno.now$leaf.buds.observed=="Yes",]

summary(droplevels(pheno.now[pheno.now$collection=="Acer" & pheno.now$leaf.buds.observed=="Yes",]))
#----------------------------
# -------------------------------------------------------------

# -------------------------------------------------------------
# Additional QAQC: Options
# - maps showing patterns of activity
# - our obs versus NPN
# -------------------------------------------------------------
# 
# -------------------------------------------------------------
