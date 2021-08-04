# A new script to see the mean of phenophases for each individual year per collection
library(ggplot2)
# path.figs <- "/Volumes/GoogleDrive/LivingCollections_Phenology/Reports/2021_01_MidYear_Report/figures_spring_2021"
path.figs <- "G://My Drive/LivingCollections_Phenology/Reports/2021_01_MidYear_Report/figures_spring_2021"

if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")
#year 2021
acer21 <- clean.google(collection="Acer", dat.yr=2021)
acer21$Collection <- as.factor("Acer")
acer21$Year <- lubridate::year(acer21$Date.Observed)
summary(acer21)

acer19 <- clean.google(collection="Acer", dat.yr=2019)
acer19$Collection <- as.factor("Acer")
acer19$Year <- lubridate::year(acer19$Date.Observed)
summary(acer19)

quercus21 <- clean.google(collection="Quercus", dat.yr=2021)
quercus21$Collection <- as.factor("Quercus")
quercus21$Year <- lubridate::year(quercus21$Date.Observed)
summary(quercus21)

quercus19 <- clean.google(collection="Quercus", dat.yr=2019)
quercus19$Collection <- as.factor("Quercus")
quercus19$Year <- lubridate::year(quercus19$Date.Observed)
summary(quercus19)

quercus18 <- clean.google(collection="Quercus", dat.yr=2018)
quercus18$Collection <- as.factor("Quercus")
quercus18$Year <- lubridate::year(quercus18$Date.Observed)
summary(quercus18)

#####
##### indivudal year means for phenophases###
#####
#leaves increasing 21,19,18 Quercus
#21
quercus.li21 <- quercus21[quercus21$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.increasing.intensity", "leaf.increasing.observed")]
quercus.li21 <- quercus.li21[!is.na(quercus.li21$PlantNumber),]
summary(quercus.li21)
head(quercus.li21)

#Setting a yday
quercus.li21$yday <- lubridate::yday(quercus.li21$Date.Observed)
quercus.li21 <- quercus.li21 [quercus.li21$yday<=180,]
summary(quercus.li21)

#finding the minimimum and maximum range and mean of the dates Leaf increasing was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.li21$Date.Observed)
max(quercus.li21$Date.Observed)
range(quercus.li21$Date.Observed)
mean(quercus.li21$Date.Observed,na.rm=T)

########################
#19
quercus.li19 <- quercus19[quercus.all$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.increasing.intensity", "leaf.increasing.observed")]
quercus.li19 <- quercus.li19[!is.na(quercus.li19$PlantNumber),]
summary(quercus.li19)
head(quercus.li19)

#Setting a yday
quercus.li19$yday <- lubridate::yday(quercus.li19$Date.Observed)
quercus.li19 <- quercus.li19 [quercus.li19$yday<=180,]
summary(quercus.li19)

#finding the minimimum and maximum range and mean of the dates Leaf increasing was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.li19$Date.Observed)
max(quercus.li19$Date.Observed)
range(quercus.li19$Date.Observed)
mean(quercus.li19$Date.Observed,na.rm=T)


########################
#18
quercus.li18 <- quercus18[quercus.all$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.increasing.intensity", "leaf.increasing.observed")]
quercus.li18 <- quercus.li18[!is.na(quercus.li18$PlantNumber),]
summary(quercus.li18)
head(quercus.li18)

#Setting a yday
quercus.li18$yday <- lubridate::yday(quercus.li18$Date.Observed)
quercus.li18 <- quercus.li18 [quercus.li18$yday<=180,]
summary(quercus.li18)

#finding the minimimum and maximum range and mean of the dates Leaf increasing was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.li18$Date.Observed)
max(quercus.li18$Date.Observed)
range(quercus.li18$Date.Observed)
mean(quercus.li18$Date.Observed,na.rm=T)

#leaves increasing 21,19 Acer
#21
acer.li21 <- acer21[acer.all$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.increasing.intensity", "leaf.increasing.observed")]
acer.li21 <- acer.li21[!is.na(acer.li21$PlantNumber),]
summary(acer.li21)
head(acer.li21)

#Setting a yday
acer.li21$yday <- lubridate::yday(acer.li21$Date.Observed)
acer.li21 <- acer.li21 [acer.li21$yday<=180,]
summary(acer.li21)

#finding the minimimum and maximum range and mean of the dates Leaf increasing was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.li21$Date.Observed)
max(acer.li21$Date.Observed)
range(acer.li21$Date.Observed)
mean(acer.li21$Date.Observed,na.rm=T)

########################
#19
acer.li19 <-acer19[acer.all$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.increasing.intensity", "leaf.increasing.observed")]
quercus.li19 <- quercus.li19[!is.na(acer.li19$PlantNumber),]
summary(acer.li19)
head(acer.li19)

#Setting a yday
acer.li19$yday <- lubridate::yday(acer.li19$Date.Observed)
acer.li19 <- acer.li19 [acer.li19$yday<=180,]
summary(acer.li19)

#finding the minimimum and maximum range and mean of the dates Leaf increasing was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.li19$Date.Observed)
max(acer.li19$Date.Observed)
range(acer.li19$Date.Observed)
mean(acer.li19$Date.Observed,na.rm=T)

###################
##################
#leaves 21,19,18 Quercus
#21
quercus.lp21 <- quercus21[quercus.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed")]
quercus.lp21 <- quercus.lp21[!is.na(quercus.lp21$PlantNumber),]
summary(quercus.lp21)
head(quercus.lp21)

#Setting a yday
quercus.lp21$yday <- lubridate::yday(quercus.lp21$Date.Observed)
quercus.lp21 <- quercus.lp21 [quercus.lp21$yday<=180,]
summary(quercus.lp21)

min(quercus.lp21$Date.Observed)
max(quercus.lp21$Date.Observed)
range(quercus.lp21$Date.Observed)
mean(quercus.lp21$Date.Observed,na.rm=T)

#aggregating for just an individual tree
quercus.lp21 <- aggregate(yday ~ Species + PlantNumber, data=quercus.lp21, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.lp21)



########################
#19
quercus.lp19 <- quercus19[quercus.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed")]
quercus.lp19 <- quercus.lp19[!is.na(quercus.lp19$PlantNumber),]
summary(quercus.lp19)
head(quercus.lp19)

#Setting a yday
quercus.lp19$yday <- lubridate::yday(quercus.lp19$Date.Observed)
quercus.lp19 <- quercus.lp19 [quercus.lp19$yday<=180,]
summary(quercus.lp19)

min(quercus.lp19$Date.Observed)
max(quercus.lp19$Date.Observed)
range(quercus.lp19$Date.Observed)
mean(quercus.lp19$Date.Observed,na.rm=T)

#aggregating for just an individual tree
quercus.lp19 <- aggregate(yday ~ Species + PlantNumber, data=quercus.lp19, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.lp19)

########################
#18
quercus.lp18 <- quercus18[quercus.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed")]
quercus.lp18 <- quercus.lp18[!is.na(quercus.lp18$PlantNumber),]
summary(quercus.lp18)
head(quercus.lp18)

#Setting a yday
quercus.lp18$yday <- lubridate::yday(quercus.lp18$Date.Observed)
quercus.lp18 <- quercus.lp18 [quercus.lp18$yday<=180,]
summary(quercus.lp18)

min(quercus.lp18$Date.Observed)
max(quercus.lp18$Date.Observed)
range(quercus.lp18$Date.Observed)
mean(quercus.lp18$Date.Observed,na.rm=T)

#aggregating for just an individual tree
quercus.lp18 <- aggregate(yday ~ Species + PlantNumber, data=quercus.lp18, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.lp18)

#leaves 21,19 Acer
#21
acer.lp21 <- acer21[acer.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed")]
acer.lp21 <- acer.lp21[!is.na(acer.lp21$PlantNumber),]
summary(acer.lp21)
head(acer.lp21)

#Setting a yday
acer.lp21$yday <- lubridate::yday(acer.lp21$Date.Observed)
acer.lp21 <-acer.lp21 [acer.lp21$yday<=180,]
summary(acer.lp21)

min(acer.lp21$Date.Observed)
max(acer.lp21$Date.Observed)
range(acer.lp21$Date.Observed)
mean(acer.lp21$Date.Observed,na.rm=T)

#aggregating for just an individual tree
acer.lp21 <- aggregate(yday ~ Species + PlantNumber, data=acer.lp21, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.lp21)



########################
#19
acer.lp19 <- acer19[acer.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed")]
acer.lp19 <- acer.lp19[!is.na(acer.lp19$PlantNumber),]
summary(acer.lp19)
head(acer.lp19)

#Setting a yday
acer.lp19$yday <- lubridate::yday(acer.lp19$Date.Observed)
acer.lp19 <- acer.lp19 [acer.lp19$yday<=180,]
summary(acer.lp19)

min(acer.lp19$Date.Observed)
max(acer.lp19$Date.Observed)
range(acer.lp19$Date.Observed)
mean(acer.lp19$Date.Observed,na.rm=T)

#aggregating for just an individual tree
acer.lp19 <- aggregate(yday ~ Species + PlantNumber, data=acer.lp19, FUN=mean, na.rm=T)
####find the mean in the summary
summary(acer.lp19)

###################
##################
#flowerbuds 21,19,18 Quercus
#21
quercus.fb21 <- quercus21[quercus.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed")]
quercus.fb21 <- quercus.fb21[!is.na(quercus.lp21$PlantNumber),]
summary(quercus.fb21)
head(quercus.fb21)

#Setting a yday
quercus.fb21$yday <- lubridate::yday(quercus.fb21$Date.Observed)
quercus.fb21 <- quercus.fb21 [quercus.fb21$yday<=180,]
summary(quercus.fb21)

#aggregating for just an individual tree
quercus.fb21 <- aggregate(yday ~ Species + PlantNumber, data=quercus.fb21, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.fb21)

############
#19
quercus.fb19 <- quercus19[quercus.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed")]
quercus.fb19 <- quercus.fb19[!is.na(quercus.fb19$PlantNumber),]
summary(quercus.fb19)
head(quercus.fb19)

#Setting a yday
quercus.fb19$yday <- lubridate::yday(quercus.fb19$Date.Observed)
quercus.fb19<- quercus.fb19 [quercus.fb19$yday<=180,]
summary(quercus.fb19)

#aggregating for just an individual tree
quercus.fb19 <- aggregate(yday ~ Species + PlantNumber, data=quercus.fb19, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.fb19)

############
#18
quercus.fb18 <- quercus18[quercus.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed")]
quercus.fb18 <- quercus.fb18[!is.na(quercus.fb18$PlantNumber),]
summary(quercus.fb18)
head(quercus.fb18)

#Setting a yday
quercus.fb18$yday <- lubridate::yday(quercus.fb18$Date.Observed)
quercus.fb18<- quercus.fb18 [quercus.fb18$yday<=180,]
summary(quercus.fb18)

#aggregating for just an individual tree
quercus.fb18 <- aggregate(yday ~ Species + PlantNumber, data=quercus.fb18, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.fb18)

###########################
#flowerbuds 21,19 acer
#21
acer.fb21 <- acer21[acer.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed")]
acer.fb21 <- acer.fb21[!is.na(acer.fb21$PlantNumber),]
summary(acer.fb21)
head(acer.fb21)

#Setting a yday
acer.fb21$yday <- lubridate::yday(acer.fb21$Date.Observed)
acer.fb21 <- acer.fb21 [acer.fb21$yday<=180,]
summary(acer.fb21)

#aggregating for just an individual tree
acer.fb21 <- aggregate(yday ~ Species + PlantNumber, data=acer.fb21, FUN=mean, na.rm=T)
####find the mean in the summary
summary(acer.fb21)

############
#19
acer.fb19 <- acer19[acer.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed")]
acer.fb19 <- acer.fb19[!is.na(acer.fb19$PlantNumber),]
summary(acer.fb19)
head(acer.fb19)

#Setting a yday
acer.fb19$yday <- lubridate::yday(acer.fb19$Date.Observed)
acer.fb19<- acer.fb19 [acer.fb19$yday<=180,]
summary(acer.fb19)

#aggregating for just an individual tree
acer.fb19 <- aggregate(yday ~ Species + PlantNumber, data=acer.fb19, FUN=mean, na.rm=T)
####find the mean in the summary
summary(acer.fb19)

###################
##################
#open flowers 21,19,18 Quercus
#21
quercus.fo21 <- quercus21[quercus.all$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed")]
quercus.fo21 <- quercus.fo21[!is.na(quercus.fo21$PlantNumber),]
summary(quercus.fo21)
head(quercus.fo21)

#Setting a yday
quercus.fo21$yday <- lubridate::yday(quercus.fo21$Date.Observed)
quercus.fo21 <- quercus.fo21 [quercus.fo21$yday<=180,]
summary(quercus.fo21)

#aggregating for just an individual tree
quercus.fo21 <- aggregate(yday ~ Species + PlantNumber, data=quercus.fo21, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.fo21)

############
#19
quercus.fo19 <- quercus19[quercus.all$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed")]
quercus.fo19 <- quercus.fo19[!is.na(quercus.fo19$PlantNumber),]
summary(quercus.fo19)
head(quercus.fo19)

#Setting a yday
quercus.fo19$yday <- lubridate::yday(quercus.fo19$Date.Observed)
quercus.fo19<- quercus.fo19 [quercus.fo19$yday<=180,]
summary(quercus.fo19)

#aggregating for just an individual tree
quercus.fo19 <- aggregate(yday ~ Species + PlantNumber, data=quercus.fo19, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.fo19)

############
#18
quercus.fo18 <- quercus18[quercus.all$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed")]
quercus.fo18 <- quercus.fo18[!is.na(quercus.fo18$PlantNumber),]
summary(quercus.fo18)
head(quercus.fo18)

#Setting a yday
quercus.fo18$yday <- lubridate::yday(quercus.fo18$Date.Observed)
quercus.fo18<- quercus.fo18 [quercus.fo18$yday<=180,]
summary(quercus.fo18)

#aggregating for just an individual tree
quercus.fo18 <- aggregate(yday ~ Species + PlantNumber, data=quercus.fo18, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.fo18)

###########################
#open flowers 21,19 acer
#21
acer.fo21 <- acer21[acer.all$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed")]
acer.fo21 <- acer.fo21[!is.na(acer.lp21$PlantNumber),]
summary(acer.fo21)
head(acer.fo21)

#Setting a yday
acer.fo21$yday <- lubridate::yday(acer.fo21$Date.Observed)
acer.fo21 <- acer.fo21 [acer.fo21$yday<=180,]
summary(acer.fo21)

#aggregating for just an individual tree
acer.fo21 <- aggregate(yday ~ Species + PlantNumber, data=acer.fo21, FUN=mean, na.rm=T)
####find the mean in the summary
summary(acer.fo21)

############
#19
acer.fo19 <- acer19[acer.all$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed")]
acer.fo19 <- acer.fo19[!is.na(acer.fo19$PlantNumber),]
summary(acer.fo19)
head(acer.fo19)

#Setting a yday
acer.fo19$yday <- lubridate::yday(acer.fo19$Date.Observed)
acer.fo19<- acer.fo19 [acer.fo19$yday<=180,]
summary(acer.fo19)

#aggregating for just an individual tree
acer.fo19 <- aggregate(yday ~ Species + PlantNumber, data=acer.fo19, FUN=mean, na.rm=T)
####find the mean in the summary
summary(acer.fo19)

###################
##################
#flower pollen 21,19,18 Quercus
#21
quercus.fp21 <- quercus21[quercus.all$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.pollen.observed")]
quercus.fp21 <- quercus.fp21[!is.na(quercus.fp21$PlantNumber),]
summary(quercus.fp21)
head(quercus.fp21)

#Setting a yday
quercus.fp21$yday <- lubridate::yday(quercus.fp21$Date.Observed)
quercus.fp21 <- quercus.fp21 [quercus.fp21$yday<=180,]
summary(quercus.fp21)

#aggregating for just an individual tree
quercus.fp21 <- aggregate(yday ~ Species + PlantNumber, data=quercus.fp21, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.fp21)

############
#19
quercus.fp19 <- quercus19[quercus.all$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.pollen.observed")]
quercus.fp19 <- quercus.fp19[!is.na(quercus.fp19$PlantNumber),]
summary(quercus.fp19)
head(quercus.fp19)

#Setting a yday
quercus.fp19$yday <- lubridate::yday(quercus.fp19$Date.Observed)
quercus.fp19<- quercus.fp19 [quercus.fp19$yday<=180,]
summary(quercus.fp19)

#aggregating for just an individual tree
quercus.fp19 <- aggregate(yday ~ Species + PlantNumber, data=quercus.fp19, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.fp19)

############
#18
quercus.fp18 <- quercus18[quercus.all$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.pollen.observed")]
quercus.fp18 <- quercus.fp18[!is.na(quercus.fp18$PlantNumber),]
summary(quercus.fp18)
head(quercus.fp18)

#Setting a yday
quercus.fp18$yday <- lubridate::yday(quercus.fp18$Date.Observed)
quercus.fp18<- quercus.fp18 [quercus.fp18$yday<=180,]
summary(quercus.fp18)

#aggregating for just an individual tree
quercus.fp18 <- aggregate(yday ~ Species + PlantNumber, data=quercus.fp18, FUN=mean, na.rm=T)
####find the mean in the summary
summary(quercus.fp18)

###########################
#flower pollen 21,19 acer
#21
acer.fp21 <- acer21[acer.all$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.pollen.observed")]
acer.fp21 <- acer.fp21[!is.na(acer.fp21$PlantNumber),]
summary(acer.fp21)
head(acer.fp21)

#Setting a yday
acer.fp21$yday <- lubridate::yday(acer.fp21$Date.Observed)
acer.fp21 <- acer.fp21 [acer.fp21$yday<=180,]
summary(acer.fp21)

#aggregating for just an individual tree
acer.fp21 <- aggregate(yday ~ Species + PlantNumber, data=acer.fp21, FUN=mean, na.rm=T)
####find the mean in the summary
summary(acer.fp21)

############
#19
acer.fp19 <- acer19[acer.all$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.pollen.observed")]
acer.fp19 <- acer.fp19[!is.na(acer.fp19$PlantNumber),]
summary(acer.fp19)
head(acer.fp19)

#Setting a yday
acer.fp19$yday <- lubridate::yday(acer.fp19$Date.Observed)
acer.fp19<- acer.fp19 [acer.fp19$yday<=180,]
summary(acer.fp19)

#aggregating for just an individual tree
acer.fp19 <- aggregate(yday ~ Species + PlantNumber, data=acer.fp19, FUN=mean, na.rm=T)
####find the mean in the summary
summary(acer.fp19)


###########################
#fruit present 21,19 acer
#21
acer.f21 <- acer21[acer.all$fruit.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.present.observed")]
acer.f21 <- acer.f21[!is.na(acer.f21$PlantNumber),]
summary(acer.f21)
head(acer.f21)

#Setting a yday
acer.f21$yday <- lubridate::yday(acer.f21$Date.Observed)
acer.f21 <- acer.f21 [acer.f211$yday<=180,]
summary(acer.f21)

#aggregating for just an individual tree
acer.f21 <- aggregate(yday ~ Species + PlantNumber, data=acer.f21, FUN=mean, na.rm=T)
####find the mean in the summary
summary(acer.f21)

############
#19
acer.f19 <- acer19[acer.all$fruit.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.present.observed")]
acer.f19 <- acer.f19[!is.na(acer.f19$PlantNumber),]
summary(acer.f19)
head(acer.f19)

#Setting a yday
acer.f19$yday <- lubridate::yday(acer.f19$Date.Observed)
acer.f19<- acer.f19 [acer.f19$yday<=180,]
summary(acer.f19)

#aggregating for just an individual tree
acer.f19 <- aggregate(yday ~ Species + PlantNumber, data=acer.f19, FUN=mean, na.rm=T)
####find the mean in the summary
summary(acer.f19)
