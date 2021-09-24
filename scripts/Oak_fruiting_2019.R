# -------------------------------------------------------------
# Getting oak fruiting data for Andrew
# 1. Parse data so it's just QUMA
# 2. Get just dat for fruiting + intensity
# 3. All QURU works, but if we can just do fruit data, bonus!
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
# get the data from a particular sheet
quercus <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Quercus", dat.yr=lubridate::year(Sys.Date()))
summary(quercus)


# --------------------- 
# 1. Parse data so it's just QUMA
# --------------------- 
head(quercus)

# start with just getting QUMA
summary(quercus$Species=="Quercus macrocarpa")
quma <- quercus[quercus$Species=="Quercus macrocarpa",]
summary(quma)

quma.fr <- quma[quma$fruit.present.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "fruit.present.observed", "fruit.present.intensity", "fruit.ripe.observed", "fruit.ripe.intensity", "fruit.drop.observed", "fruit.drop.intensity")]
summary(quma.fr)
dim(quma.fr)

# hmmmm... fruit present intensity is all blank... is that column broken in the get data script?
summary(quercus$fruit.present.intensity) #hmmmm... some 0, but a whole lot of NA
summary(!is.na(quercus$fruit.present.intensity))
# After checkign the google sheet, the clean google form script is broken... I'll fix it... later

# Making the ripe fruit intensity in a particular order
levels(quma.fr$fruit.ripe.intensity)
quma.fr$fruit.ripe.intensity <- factor(quma.fr$fruit.ripe.intensity, levels=c("0%", "< 5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%"))
length(unique(quma.fr$PlantNumber)) # Seeing how many unique trees we have

# Making 0% the same as NA
# !is.na(quma.fr$fruit.ripe.intensity) is same as quma.fr$fruit.ripe.intensity!=NA
quma.fr[quma.fr$fruit.ripe.intensity=="0%" & !is.na(quma.fr$fruit.ripe.intensity),"fruit.ripe.intensity"] <- NA
summary(quma.fr$fruit.ripe.intensity=="0%" & !is.na(quma.fr$fruit.ripe.intensity))

# Lets just save this in our current folder and email it to Andrew
png("../figures/QUMA_fruits_2019.png", height=4, width=6, units="in", res=120)
ggplot(data=quma.fr, aes(x=Date.Observed)) +
  geom_histogram(aes(fill=fruit.ripe.intensity), binwidth=7) +
  ggtitle("Number Trees with Acorns")
dev.off()

# Also give Andrew all the data so he can play with it himself
write.csv(quma.fr, "../data/QUMA_fruits_2019.csv", row.names=F)
# --------------------- 

# --------------------- 
# 2. Get just dat for fruiting + intensity
# --------------------- 
# --------------------- 

# --------------------- 
# 3. All QURU works, but if we can just do fruit data, bonus!
# --------------------- 
# --------------------- 



# -------------------------------------------------------------
