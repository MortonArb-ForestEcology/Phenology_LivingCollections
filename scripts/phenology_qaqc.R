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

# dir.base <- "/Volumes/"
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
# ----------------
# get the data from each collection
# ----------------
quercus <- clean.google(collection="Quercus", dat.yr=lubridate::year(Sys.Date()))
summary(quercus)
# quercus[quercus$Date.Observed>Sys.Date(),1:6]

acer <- clean.google(collection="Acer", dat.yr=lubridate::year(Sys.Date()))
summary(acer)

ulmus <- clean.google(collection="Ulmus", dat.yr=lubridate::year(Sys.Date()))
summary(ulmus)
# ----------------

# Put the data together
dat.all <- rbind(quercus, acer, ulmus)
summary(dat.all)
summary(dat.all[is.na(dat.all$leaf.buds.observed),])

phenophase.obs <- names(dat.all)[grep(".observed", names(dat.all))] 


summary(dat.all$Observer)
summary(dat.all[,"Observer"])

for(PHENO in phenophase.obs){
  dat.all[is.na(dat.all[,PHENO]),PHENO] <- "No Observation"
  # dat.all[,PHENO] <- factor(dat.all[,PHENO], levels=c("No", "Yes", "Unsure", "Did not look for"))
}
summary(dat.all)
summary

range(dat.all$Date.Observed)
dat.all[lubridate::year(dat.all$Date.Observed)<lubridate::year(Sys.Date()),1:6]
dat.all[dat.all$Date.Observed>Sys.Date(),1:6]

# dim(dat.all)


#----------------------------
# For QAQC, get rid of trees that have been removed
#----------------------------
# Querying the googlesheet for missing trees up front to make it easier
sheet.gone <- gs_title("Removed Trees - Phenology_LivingCollections")
sheet.gone # Prints all the metadata

# Get the particular sheet & coerce it into a data frame rather than something special
df.gone <- data.frame(gs_read(sheet.gone, ws="Removed Trees"))
summary(df.gone)

dim(dat.all)
dat.all <- dat.all[!dat.all$PlantNumber %in% df.gone$PlantNumber,]
summary(dat.all)

# Also merge in the observing lists and volunteer assignments
quercus.list <- read.csv(file.path(dir.base, "Observing Lists/Quercus", "ObservingLists_Quercus.csv"))
acer.list <- read.csv(file.path(dir.base, "Observing Lists/Acer", "ObservingLists_Acer.csv"))
quercus.list$collection <- "Quercus"
acer.list$collection <- "Acer"
quercus.list$Obs.List <- paste(quercus.list$collection, quercus.list$Obs.List, sep="-")
acer.list$Obs.List <- paste(acer.list$collection, acer.list$Obs.List, sep="-")

summary(quercus.list)
summary(acer.list)
head(acer.list)

obs.list <- rbind(quercus.list, acer.list)
summary(obs.list)

dat.all <- merge(dat.all, obs.list[,c("Obs.List", "collection", "PlantNumber")])
dat.all$Obs.List <- as.factor(dat.all$Obs.List)
dat.all$collection <- as.factor(dat.all$collection)
summary(dat.all)

# Quick stats:
length(unique(dat.all$Observer))
length(unique(dat.all$PlantNumber))
length(unique(dat.all$Species))

# Saving the figures that show observations for each individual
for(PHENO in phenophase.obs){
  pdf(file.path(dir.base, "Data_Observations/Pheno_Status/by_Phenophase", paste0("2019_Observations_", PHENO, "_by_List_byTree.pdf")), width=11, height=8.5)
  for(OBS in levels(dat.all$Obs.List)){
    dat.tmp <- dat.all[dat.all$Obs.List==OBS, ]
    dat.tmp$Phenophase <- dat.tmp[,PHENO]
    print(
      ggplot(data=dat.tmp[,]) +
        ggtitle(paste0(OBS)) +
        facet_grid(Species*PlantNumber~., scales="free_y", switch="y") +
        geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=Phenophase), binwidth=7) +
        scale_fill_manual(PHENO, values=c("gray50", "green4", "blue2", "black") ) +
        scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
        scale_y_discrete(expand=c(0,0)) +
        scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
        theme(legend.position="bottom",
              legend.text = element_text(size=rel(1)),
              legend.title = element_text(size=rel(1)),
              plot.title = element_text(size=rel(1), face="bold", hjust=0.5),
              panel.grid = element_blank(),
              panel.background=element_rect(fill=NA, color="black"),
              panel.spacing=unit(0, "lines"),
              axis.text.x=element_text(size=rel(1)),
              axis.title.x=element_text(size=rel(1), face="bold"),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text.y=element_text(size=rel(1), angle=180))
    )
  }
  dev.off()
}

for(OBS in levels(dat.all$Obs.List)){
  dat.tmp <- dat.all[dat.all$Obs.List==OBS, ]
  pdf(file.path(dir.base, "Data_Observations/Pheno_Status/by_ObservingList", paste0("2019_Observations_", OBS, "_by_Phenophase_byTree.pdf")), width=11, height=8.5)
  for(PHENO in phenophase.obs){
    dat.tmp$Phenophase <- dat.tmp[,PHENO]
    print(
      ggplot(data=dat.tmp[,]) +
        ggtitle(paste0(PHENO)) +
        facet_grid(Species*PlantNumber~., scales="free_y", switch="y") +
        geom_bin2d(aes(x=Date.Observed, y=PlantNumber, fill=Phenophase), binwidth=7) +
        scale_fill_manual(PHENO, values=c("gray50", "green4", "blue2", "black") ) +
        scale_x_date(name="Date", limits = range(dat.all$Date.Observed), expand=c(0,0)) +
        scale_y_discrete(expand=c(0,0)) +
        scale_alpha_continuous(name= "Prop. Obs.", limits=c(0,1), range=c(0.1,1)) +
        theme(legend.position="bottom",
              legend.text = element_text(size=rel(1)),
              legend.title = element_text(size=rel(1)),
              plot.title = element_text(size=rel(1), face="bold", hjust=0.5),
              panel.grid = element_blank(),
              panel.background=element_rect(fill=NA, color="black"),
              panel.spacing=unit(0, "lines"),
              axis.text.x=element_text(size=rel(1)),
              axis.title.x=element_text(size=rel(1), face="bold"),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              strip.text.y=element_text(size=rel(1), angle=180))
    )
  }
  dev.off()
}
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
obs.all$Obs.List <- paste(obs.all$Collection, obs.all$List, sep="-")
obs.all


obs.check <- aggregate(dat.all$Date.Observed, by=list(dat.all$Observer), FUN=max)
names(obs.check) <- c("Observer", "Observation.Last")
obs.check
obs.check[obs.check$Observation.Last < Sys.Date()-8,] # Return anybody that's more than 8 days old

# See if anybody has not enetered at all
obs.all[!obs.all$Observer.ID %in% obs.check$Observer,]

# Checking to make sure all trees have observations for the past week
acc.check <- aggregate(dat.all$Date.Observed, by=dat.all[,c("PlantNumber", "Species", "Obs.List")], FUN=max)
names(acc.check)[which(names(acc.check)=="x")] <- "Observation.Last"
acc.check <- merge(acc.check, obs.all[,c("Obs.List", "Observer.ID")], all.x=T)
summary(acc.check)
acc.check[acc.check$Observation.Last < Sys.Date()-8,] # Return any tree that hasn't been observed for more than 8 days

# Ignore known "troublemakers"
acc.check[acc.check$Observation.Last < Sys.Date()-8 & acc.check$Observer.ID!="Populorum" & !(acc.check$Observer.ID=="Buerger" & acc.check$Species=="Acer barbatum"),]

nrow(acc.check[acc.check$Observation.Last < Sys.Date()-8,])/nrow(acc.check)
nrow(acc.check[acc.check$Observation.Last < Sys.Date()-8 & acc.check$Observer.ID!="Populorum",])/nrow(acc.check)
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

pheno.now[pheno.now$Date.Observed<as.Date("2019-08-01"),]


# Checking some oddballs
pheno.now[pheno.now$fruit.ripe.observed=="Yes",]
# summary(pheno.now[pheno.now$leaf.buds.observed=="Yes",])
summary(pheno.now[pheno.now$leaf.buds.observed=="Yes",])
summary(pheno.now[pheno.now$leaf.buds.observed=="Yes" & pheno.now$Status=="Past Week",])
summary(pheno.now[pheno.now$leaf.buds.observed=="Yes" & pheno.now$collection=="Acer",])
summary(pheno.now[pheno.now$leaf.present.observed=="No",])
summary(pheno.now[pheno.now$leaf.present.observed=="No" & pheno.now$Observer=="Bigsby",])

summary(pheno.now[pheno.now$leaf.buds.observed=="No" & pheno.now$leaf.present.observed=="No",])

summary(pheno.now[pheno.now$leaf.present.observed=="Yes",])
summary(pheno.now[pheno.now$leaf.present.observed=="Yes",])
pheno.now[pheno.now$leaf.present.observed=="?",]

# Checking for likely mis-entries
pheno.now[pheno.now$leaf.buds.observed %in% c("No", "Did not look for") & !pheno.now$leaf.buds.intensity %in% c("0", NA),]
pheno.now[pheno.now$leaf.present.observed %in% c("No", "Did not look for") & !pheno.now$leaf.present.intensity %in% c("0%", NA),]
pheno.now[pheno.now$leaf.increasing.observed %in% c("No", "Did not look for") & !pheno.now$leaf.increasing.intensity %in% c("0%", NA),]
pheno.now[pheno.now$leaf.color.observed %in% c("No", "Did not look for") & !pheno.now$leaf.color.intensity %in% c("0%", NA),]
pheno.now[!pheno.now$leaf.falling.observed %in% c("No", "Did not look for"),]
summary(pheno.now[!pheno.now$leaf.color.observed %in% c("No", "Did not look for"),])
pheno.now[pheno.now$leaf.color.observed %in% c("Yes") & pheno.now$leaf.color.intensity %in% (">95%"),]


pheno.now[pheno.now$flower.buds.observed %in% c("No", "Did not look for") & !pheno.now$flower.buds.intensity %in% c("0", NA),]
pheno.now[pheno.now$flower.open.observed %in% c("No", "Did not look for") & !pheno.now$flower.open.intensity %in% c("0%", NA),]
pheno.now[pheno.now$flower.pollen.observed %in% c("No", "Did not look for") & !pheno.now$flower.pollen.intensity %in% c("None", NA),]
pheno.now[pheno.now$fruit.present.observed %in% c("No", "Did not look for") & !pheno.now$fruit.present.intensity %in% c("0", NA),]
pheno.now[pheno.now$fruit.ripe.observed %in% c("No", "Did not look for") & !pheno.now$fruit.ripe.intensity %in% c("0%", NA),]
pheno.now[pheno.now$fruit.drop.observed %in% c("No", "Did not look for") & !pheno.now$fruit.drop.intensity %in% c("0", NA),]
pheno.now[!pheno.now$fruit.drop.observed %in% c("No", "Did not look for"),]

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
