# A new script with some combined collections analyses/graphs for the end of the year report
library(ggplot2)
library(stringi)
library(lubridate)
library(ggiraph)
library(viridis)
library(ggnewscale)
library(geomtextpath)
library(cowplot)
library(tidyverse)
library(gganimate)
library(gifski)
library(dplyr)
###setting the file path to mac or windows##
path.google <- "/Volumes/GoogleDrive/My Drive/" # Mac
path.out <- file.path(path.google, "G://My Drive/LivingCollections_Phenology/Reports")
#path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2022_02_EndOfYear_Report/figures_2022_end")
# this is for google -># 
path.figs <- "G://My Drive/LivingCollections_Phenology/Reports/2022_02_EndOfYear_Report/figures_2022_end"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")
#year 2022
acer22 <- clean.google(collection="Acer", dat.yr=2022)
acer22$Collection <- as.factor("Acer")
acer22$Year <- lubridate::year(acer22$Date.Observed)
summary(acer22)

acer21 <- clean.google(collection="Acer", dat.yr=2021)
acer21$Collection <- as.factor("Acer")
acer21$Year <- lubridate::year(acer21$Date.Observed)
summary(acer21)

acer20 <- clean.google(collection="Acer", dat.yr=2020)
acer20$Collection <- as.factor("Acer")
acer20$Year <- lubridate::year(acer20$Date.Observed)
summary(acer20)

acer19 <- clean.google(collection="Acer", dat.yr=2019)
acer19$Collection <- as.factor("Acer")
acer19$Year <- lubridate::year(acer19$Date.Observed)
summary(acer19)

quercus22 <- clean.google(collection="Quercus", dat.yr=2022)
quercus22$Collection <- as.factor("Quercus")
quercus22$Year <- lubridate::year(quercus22$Date.Observed)
summary(quercus22)

quercus21 <- clean.google(collection="Quercus", dat.yr=2021)
quercus21$Collection <- as.factor("Quercus")
quercus21$Year <- lubridate::year(quercus21$Date.Observed)
summary(quercus21)

quercus20 <- clean.google(collection="Quercus", dat.yr=2020)
quercus20$Collection <- as.factor("Quercus")
quercus20$Year <- lubridate::year(quercus20$Date.Observed)
summary(quercus20)

quercus19 <- clean.google(collection="Quercus", dat.yr=2019)
quercus19$Collection <- as.factor("Quercus")
quercus19$Year <- lubridate::year(quercus19$Date.Observed)
summary(quercus19)

quercus18 <- clean.google(collection="Quercus", dat.yr=2018)
quercus18$Collection <- as.factor("Quercus")
quercus18$Year <- lubridate::year(quercus18$Date.Observed)
summary(quercus18)

ulmus22 <- clean.google(collection="Ulmus", dat.yr=2022)
ulmus22$Collection <- as.factor("Ulmus")
ulmus22$Year <- lubridate::year(ulmus22$Date.Observed)
summary(ulmus22)

ulmus21 <- clean.google(collection="Ulmus", dat.yr=2021)
ulmus21$Collection <- as.factor("Ulmus")
ulmus21$Year <- lubridate::year(ulmus21$Date.Observed)
summary(ulmus21)


ulmus20 <- clean.google(collection="Ulmus", dat.yr=2020)
ulmus20$Collection <- as.factor("Ulmus")
ulmus20$Year <- lubridate::year(ulmus20$Date.Observed)
summary(ulmus20)

tilia22 <- clean.google(collection="Tilia", dat.yr=2022)
tilia22$Collection <- as.factor("Tilia")
tilia22$Year <- lubridate::year(tilia22$Date.Observed)
summary(tilia22)

#binding, but leaving tilia out because 
dat.all <- rbind(ulmus22, quercus22, acer22, ulmus20, ulmus21, quercus18, quercus19, quercus20, quercus21, acer19, acer20, acer21)
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
summary(dat.all)

#creating a df for spring only obs for quercus and acer since we did not collect data for ulmus in spring 2021 or any trees in spring 2020
dat.spring <- rbind(quercus22, acer22, quercus18, quercus19, quercus21, acer19, acer21)
dat.spring$spring <- lubridate::yday(dat.spring$Date.Observed)
summary(dat.spring)

#########generating a 2022 only df for funsies
dat.22 <- rbind(quercus22,acer22, ulmus22, tilia22)
dat.22$yday <- lubridate::yday(dat.22$Date.Observed)
summary(dat.22)

#Getting a graph of colored leaf observations
###########
###########
dat.lc <- dat.all[dat.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.observed", "Collection")]
dat.lc <- dat.lc[!is.na(dat.lc$PlantNumber),]
summary(dat.lc)
head(dat.lc)

#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.lc$Date.Observed)
max(dat.lc$Date.Observed)
range(dat.lc$Date.Observed)
mean(dat.lc$Date.Observed,na.rm=T)

#Now make my Yday
dat.lc$yday <- lubridate::yday(dat.lc$Date.Observed)
summary(dat.lc)


#only looking at trees that showed fall color from 9/1 on
dat.llc <- dat.lc [dat.lc$yday>=180,]
summary(dat.llc)

#aggregating quercus.lf so it shows me the date of first leaf color for  every plant number and species 
leaf.color <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.llc, FUN=min, na.rm=T)
summary(leaf.color)
head(leaf.color)

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#Graphing
ggplot(data=leaf.color) +
  #png(file.path(path.figs,"All_First_Leaf_Color.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection~ .,scales="free_y") + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year),color=as.factor(Year))) +
  xlim(150, 365)+
 scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
 scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Mean Day of First Leaf Color Present", x="Day of Year", fill="Year")
dev.off()

ggplot(data=leaf.color) +
  # png(file.path(path.figs,"All_First_Leaf_Color.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection~ . ) + # This is the code that will stack everything
  geom_histogram(alpha=0.5, binwidth =10, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Leaf Color Present", x="Day of Year")
dev.off()
### getting leaf color intensity
dat.lci <- dat.all[dat.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.intensity", "Collection")]
summary(dat.lci)
dat.lci <- dat.lci[!is.na(dat.lci$PlantNumber),]
summary(dat.lci)

#Checking to make sure date ranges are correct
min(dat.lci$Date.Observed)
max(dat.lci$Date.Observed)
mean(dat.lci$Date.Observed)
range(dat.lci$Date.Observed)

#Setting my yday
dat.lci$yday <- lubridate::yday(dat.lci$Date.Observed)
summary(dat.lci)

#setting my yday to only show dates later in the season and the current date
dat.lci <- dat.lci [dat.lci$yday>=200,]
#dat.lci <- dat.lci [dat.lci$yday<=Sys.Date(),]
summary(dat.lci)

#removing "0 and NA's
dat.lci <- aggregate(yday ~ PlantNumber + Species + Year + Collection + leaf.color.intensity + Date.Observed , dat=dat.lci, FUN=min, NA.rm=T)
summary(dat.lci)
head(dat.lci)

dat.lci$yday <- lubridate::yday(dat.lci$Date.Observed)
summary(dat.lci)

#leaves.present.intensity <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lci, FUN=min, NA.rm=T)
#summary(leaves.present.intensity)
#head(leaves.present.intensity)

#png(file.path(path.figs,"Leaf_Present_Intensity.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.lci) +
  geom_histogram(alpha=1.5, binwidth =10, aes(x=yday, fill=leaf.color.intensity,))+
  facet_grid(Collection~ .)+
  #scale_fill_manual(name= "leaf.present.intensity", values=c("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
  #scale_color_manual(name="leaf.present.intensity", values=c("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
  theme_bw()+
  labs(title="Leaf color Intensity", x="Day of Year",)
dev.off()

###########
###########
#Getting a graph of colored leaf observations
###########
###########


#doing freq
ggplot(data=leaf.color) +
#  png(file.path(path.figs,"All_First_Leaf_Color_freqpoly.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_freqpoly(alpha=0.5, bins = 45, aes(x=yday,color=as.factor(Year), fill=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of First Leaf Color", x="Day of Year")
dev.off()


##########
##########
#Getting a graph of falling leaf observations
###########
###########
dat.fl <- dat.all[dat.all$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed", "Collection")]
dat.fl <- dat.fl[!is.na(dat.fl$PlantNumber),]
summary(dat.fl)
head(dat.fl)

#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fl$Date.Observed)
max(dat.fl$Date.Observed)
range(dat.fl$Date.Observed)
mean(dat.fl$Date.Observed,na.rm=T)

#Now make my Yday
dat.fl$yday <- lubridate::yday(dat.fl$Date.Observed)
summary(dat.fl)


#only looking at trees that showed fall color in the last half of the year
dat.ffl <- dat.fl [dat.fl$yday>=180,]
summary(dat.ffl)

falling.leaves <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.ffl, FUN=min, na.rm=T)
summary(falling.leaves)
head(falling.leaves)

#Graphing
ggplot(data=falling.leaves) +
  #png(file.path(path.figs,"All_First_Falling_Leaf_dens.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection ~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of First Falling Leaves", x="Day of Year")
dev.off()
###########
###########
#Getting a graph of breaking leaf bud observations
###########
###########
dat.lb <- dat.spring[dat.spring$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.breaking.buds.observed", "Collection")]
dat.lb <- dat.lb[!is.na(dat.lb$PlantNumber),]
summary(dat.lb)
head(dat.lb)

#finding the minimimum and maximum range and mean of the dates breaking leaf buds were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.lb$Date.Observed)
max(dat.lb$Date.Observed)
range(dat.lb$Date.Observed)
mean(dat.lb$Date.Observed,na.rm=T)

#Now make my Yday
dat.lb$yday <- lubridate::yday(dat.lb$Date.Observed)
summary(dat.lb)


#only looking at trees that showed breaking leaf buds in the first half of the year
dat.lb <- dat.lb [dat.lb$yday<=180,]
summary(dat.lb)

#aggregating quercus.lf so it shows me the date of first breaking leaf buds for  every plant number and species 
breaking.buds <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lb, FUN=min, na.rm=T)
summary(breaking.buds)
head(breaking.buds)

#Graphing
#png(file.path(path.figs,"Leaf_Breaking_Buds_dens.png"), height=4, width=6, units="in", res=320)
ggplot(data=breaking.buds) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  labs(title="Average Day of First Breaking Leaf Buds", x="Day of Year")
dev.off()

###########
###########
#Getting a graph of leaves present observations
###########
###########
dat.lp <- dat.spring[dat.spring$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed", "Collection")]
dat.lp <- dat.lp[!is.na(dat.lp$PlantNumber),]
summary(dat.lp)
head(dat.lp)

#finding the minimimum and maximum range and mean of the dates leaf present was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.lp$Date.Observed)
max(dat.lp$Date.Observed)
range(dat.lp$Date.Observed)
mean(dat.lp$Date.Observed,na.rm=T)

#Now make my Yday
dat.lp$yday <- lubridate::yday(dat.lp$Date.Observed)
summary(dat.lp)


#only looking at trees that showed leaf present in the first half of the year
dat.lp <- dat.lp [dat.lp$yday<=250,]
#summary(dat.lp)

#aggregating quercus.lf so it shows me the date of first leaf present for  every plant number and species 
leaves.present <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lp, FUN=min, na.rm=T)
summary(leaves.present)
head(leaves.present)

#Graphing
#png(file.path(path.figs,"Leaf_Present_dens.png"), height=4, width=6, units="in", res=320)
ggplot(data=leaves.present) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of Leaves Present", x="Day of Year")
dev.off()
###########
###########
#Getting a graph of leaves present intensity 
###########
###########
dat.lpi <- dat.all[dat.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.present.intensity", "Collection")]
summary(dat.lpi)
dat.lpi <- dat.lpi[!is.na(dat.lpi$PlantNumber),]
summary(dat.lpi)

#Checking to make sure date ranges are correct
min(dat.lpi$Date.Observed)
max(dat.lpi$Date.Observed)
mean(dat.lpi$Date.Observed)
range(dat.lpi$Date.Observed)

#Setting my yday
dat.lpi$yday <- lubridate::yday(dat.lpi$Date.Observed)
summary(dat.lpi)

#setting my yday to only show dates later in the season and the current date
#dat.lpi <- dat.lpi [dat.lpi$yday>=180,]
#dat.lpi <- dat.lpi [dat.lpi$yday<=Sys.Date(),]
#summary(dat.lpi)

#removing "0 and NA's
dat.lpi <- aggregate(yday ~ PlantNumber + Species + Year + Collection + leaf.present.intensity + Date.Observed , dat=dat.lpi, FUN=min, NA.rm=T)
summary(dat.lpi)
head(dat.lpi)

dat.lpi$yday <- lubridate::yday(dat.lpi$Date.Observed)
summary(dat.lpi)

#leaves.present.intensity <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lpi, FUN=min, NA.rm=T)
#summary(leaves.present.intensity)
#head(leaves.present.intensity)

#png(file.path(path.figs,"Leaf_Present_Intensity.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.lpi) +
  geom_histogram(alpha=1.5, binwidth =10, aes(x=yday, fill=leaf.present.intensity,))+
  facet_grid(Year~Collection)+
  #scale_fill_manual(name= "leaf.present.intensity", values=c("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
  #scale_color_manual(name="leaf.present.intensity", values=c("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
  theme_bw()+
  labs(title="Leaves Present Intensity", x="Day of Year",)
dev.off()


###########
###########
#Getting a graph of leaves increasing in size observations
###########
###########
dat.li <- dat.spring[dat.spring$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.increasing.observed", "Collection")]
dat.li <- dat.li[!is.na(dat.li$PlantNumber),]
summary(dat.li)
head(dat.li)

#finding the minimimum and maximum range and mean of the dates leaves increasing in size was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.li$Date.Observed)
max(dat.li$Date.Observed)
range(dat.li$Date.Observed)
mean(dat.li$Date.Observed,na.rm=T)

#Now make my Yday
dat.li$yday <- lubridate::yday(dat.li$Date.Observed)
summary(dat.li)


#only looking at trees that showed leaves increasing in size in the first half of the year
dat.li <- dat.li [dat.li$yday<=180,]
dat.li <- dat.li [dat.li$yday>=61,]
summary(dat.li)

#aggregating quercus.lf so it shows me the date of first leaf increasing in size for  every plant number and species 
leaves.increasing <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.li, FUN=min, na.rm=T)
summary(leaves.increasing)
head(leaves.increasing)

#Graphing
#png(file.path(path.figs,"Leaf_Increasing_dens.png"), height=4, width=6, units="in", res=320)
ggplot(data=leaves.increasing) +
  facet_grid(Collection~ ., scales = "free_x") + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) + xlim(70, 180)+
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of Leaves Increasing in Size Observed", x="Day of Year")
dev.off()

ggplot(data=leaves.increasing) +
  facet_grid(Collection~ ., scales = "free_x") + # This is the code that will stack everything
  geom_histogram(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of Leaves Increasing in Size Observed", x="Day of Year")
dev.off()
###########
###########
#Getting a graph of flower buds observations
##########
###########
dat.fb <- dat.spring[dat.spring$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed", "Collection")]
dat.fb <- dat.fb[!is.na(dat.fb$PlantNumber),]
summary(dat.fb)
head(dat.fb)

#finding the minimimum and maximum range and mean of the dates flower buds were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fb$Date.Observed)
max(dat.fb$Date.Observed)
range(dat.fb$Date.Observed)
mean(dat.fb$Date.Observed,na.rm=T)

#Now make my Yday
dat.fb$yday <- lubridate::yday(dat.fb$Date.Observed)
summary(dat.fb)


#only looking at trees that showed flower buds in the first half of the year
dat.fb <- dat.fb [dat.fb$yday<=180,]
summary(dat.fb)

#aggregating quercus.lf so it shows me the date of first flower buds for  every plant number and species 
flower.buds <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fb, FUN=min, na.rm=T)
summary(flower.buds)
head(flower.buds)

#Graphing
#png(file.path(path.figs,"All_Flowers_or_Flower_Buds.png"), height=4, width=6, units="in", res=320)
ggplot(data=flower.buds) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +  xlim(65,180)+
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of Flower Buds or Flowers Observed", x="Day of Year")
dev.off()


###########
###########
#Getting a graph of open flowers observations
###########
###########
dat.fo <- dat.spring[dat.spring$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed", "Collection")]
dat.fo <- dat.fo[!is.na(dat.fo$PlantNumber),]
summary(dat.fo)
head(dat.fo)

#finding the minimimum and maximum range and mean of the dates open flowers were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fo$Date.Observed)
max(dat.fo$Date.Observed)
range(dat.fo$Date.Observed)
mean(dat.fo$Date.Observed,na.rm=T)

#Now make my Yday
dat.fo$yday <- lubridate::yday(dat.fo$Date.Observed)
summary(dat.fo)


#only looking at trees that showed open flowers in the first half of the year
#dat.fo <- dat.fo [dat.fo$yday<=180,]
summary(dat.fo)

#aggregating quercus.lf so it shows me the date of open flowers for  every plant number and species 
flower.open <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fo, FUN=min, na.rm=T)
summary(flower.open)
head(flower.open)
#removing 2020 because there were no spring observations
flower.open <- flower.open[!flower.open$Year=="2020",]
summary(flower.open)

#Graphing
#png(file.path(path.figs,"All_Flowers_Open.png"), height=4, width=6, units="in", res=320)
ggplot(data=flower.open) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) + xlim(65,300)+
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of Open Flower Observed", x="Day of Year")
dev.off()

###########
###########
#Getting a graph of pollen observations
###########
###########
dat.fp <- dat.[dat.spring$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.pollen.observed", "Collection")]
dat.fp <- dat.fp[!is.na(dat.fp$PlantNumber),]
summary(dat.fp)
head(dat.fp)

#finding the minimimum and maximum range and mean of the dates pollen was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fp$Date.Observed)
max(dat.fp$Date.Observed)
range(dat.fp$Date.Observed)
mean(dat.fp$Date.Observed,na.rm=T)

#Now make my Yday
dat.fp$yday <- lubridate::yday(dat.fp$Date.Observed)
summary(dat.fp)


#only looking at trees that showed pollen in the first half of the year
#dat.fp <- dat.fp [dat.fp$yday<=180,]
summary(dat.fp)

#aggregating quercus.lf so it shows me the date of first pollen for  every plant number and species 
flower.pollen <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fp, FUN=min, na.rm=T)
summary(flower.pollen)
head(flower.pollen)
#removing 2020 because there were no spring observations
flower.pollen <- flower.pollen[!flower.pollen$Year=="2020",]

#Graphing
#png(file.path(path.figs,"All_Flowers_Pollen.png"), height=4, width=6, units="in", res=320)
ggplot(data=flower.pollen) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) + xlim(60,175)+
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of Flower Pollen Observed", x="Day of Year")
dev.off()


######## Need to add fruit phenophases Now

#########subsetting out for fruit present

dat.fr <- dat.spring[dat.spring$fruit.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed", "Collection")]
summary(dat.fr)
dat.fr <- dat.fr[!is.na(dat.fr$PlantNumber),]
summary(dat.fr)

#Checking to make sure date ranges are correct
min(dat.fr$Date.Observed)
max(dat.fr$Date.Observed)
mean(dat.fr$Date.Observed)
range(dat.fr$Date.Observed)

#Setting my yday
dat.fr$yday <- lubridate::yday(dat.fr$Date.Observed)
summary(dat.fr)

#setting my yday to only show dates later in the season and the current date
#dat.fr <- dat.fr [dat.fr$yday<=09,]
#dat.fr <- dat.fr [dat.fr$yday<=Sys.Date(),]
#summary(dat.fr)

#aggregating to only show me observations that are present
fruit.present <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fr, FUN=min, na.rm=T)
summary(fruit.present)
head(fruit.present)
#removing 2020 because there were no spring observations
fruit.present <- fruit.present[!fruit.present$Year=="2020",]


ggplot(data=fruit.present) +
#  png(file.path(path.figs,"Fruit_present_Oak_Maple.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year)))+ xlim(60, 300)+
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of Fruit Present Observed", x="Day of Year")
dev.off()

########
#subsetting out for ripe fruit
dat.rf <- dat.spring[dat.spring$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed", "Collection")]
summary(dat.rf)
dat.rf <- dat.rf[!is.na(dat.rf$PlantNumber),]
summary(dat.rf)

#Checking to make sure date ranges are correct
min(dat.rf$Date.Observed)
max(dat.rf$Date.Observed)
mean(dat.rf$Date.Observed)
range(dat.rf$Date.Observed)

#Setting my yday
dat.rf$yday <- lubridate::yday(dat.rf$Date.Observed)
summary(dat.rf)

#setting my yday to only show dates later in the season and the current date
#dat.rf <- dat.rf [dat.rf$yday>=180,]
#dat.rf <- dat.rf [dat.rf$yday<=Sys.Date(),]
#summary(dat.rf)

#aggregating to only show me observations that are present
ripe.fruit <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.rf, FUN=min, na.rm=T)
summary(ripe.fruit)
head(ripe.fruit)
#removing 2020 because there were no spring observations
ripe.fruit <- ripe.fruit[!ripe.fruit$Year=="2020",]


ggplot(data=ripe.fruit) +
#  png(file.path(path.figs,"Ripe_Fruit_Present_All.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection~., scales = "free_y") + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +xlim(60, 365)+
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of Ripe Fruit Observed", x="Day of Year")
dev.off()

##############
#subsetting out for fruit drop
dat.fd <- dat.spring[dat.spring$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed", "Collection")]
summary(dat.fd)
dat.fd <- dat.fd[!is.na(dat.fd$PlantNumber),]
summary(dat.fd)

#Checking to make sure date ranges are correct
min(dat.fd$Date.Observed)
max(dat.fd$Date.Observed)
mean(dat.fd$Date.Observed)
range(dat.fd$Date.Observed)

#Setting my yday
dat.fd$yday <- lubridate::yday(dat.fd$Date.Observed)
summary(dat.fd)

#setting my yday to only show dates later in the season and the current date
#dat.fd <- dat.fd [dat.fd$yday>=180,]
#dat.fd <- dat.fd [dat.fd$yday<=Sys.Date(),]
#summary(dat.fd)

#aggregating to only show me observations that are present
fruit.drop <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fd, FUN=min, na.rm=T)
summary(fruit.drop)
head(fruit.drop)
#removing 2020 because there were no spring observations
ripe.fruit <- ripe.fruit[!ripe.fruit$Year=="2020",]


ggplot(data=fruit.drop) +
#  png(file.path(path.figs,"Fruit__Drop_Present_All.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection~ ., scales = "free_y") + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) + xlim(40, 365)+
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Average Day of Fruit Drop Observed", x="Day of Year")
dev.off()

############
#getting averages for date of  phenophases occurace in certain years
###########
####Open flowers quercus
dat.ofa18 <- quercus18[quercus18$flower.open.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "flower.open.observed")]
summary(dat.of)
#####Fruit Present quercus & acer
#2018 quercus
dat.fpa18 <- quercus18[quercus18$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpa18)
#2019 quercus
dat.fpa19 <- quercus19[quercus19$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpa19)
#2021 quercus
dat.fpa21 <- quercus21[quercus21$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpa21)
#2019 acer
dat.afpa19 <- acer19[acer19$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.afpa19)
#2021 acer
dat.afpa21 <- acer21[acer21$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.afpa21)
##### Ripe fruit####
#quercus 21
dat.rfa21 <- quercus21[quercus21$fruit.ripe.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.rfa21)
#2019 acer
dat.arfa19 <- acer19[acer19$fruit.ripe.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.arfa19)
#2021 acer
dat.arfa21 <- acer21[acer21$fruit.ripe.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.arfa21)

### Fruit Drop
#quercus 21
dat.fda21 <- quercus21[quercus21$fruit.drop.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.rfa21)
#2019 acer
dat.afda19 <- acer19[acer19$fruit.drop.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.arfa19)
#2021 acer
dat.afda21 <- acer21[acer21$fruit.drop.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.arfa21)


#############
#trying something with visualizations
###


#Getting an animated polar graph of colored leaf observations
###########
###########
dat.lc22 <- dat.22[dat.22$leaf.color.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.observed", "Collection")]
dat.lc22 <- dat.lc22[!is.na(dat.lc22$PlantNumber),]
summary(dat.lc22)
head(dat.lc22)

#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.lc22$Date.Observed)
max(dat.lc22$Date.Observed)
range(dat.lc22$Date.Observed)
mean(dat.lc22$Date.Observed,na.rm=T)

#Now make my Yday
dat.lc22$yday <- lubridate::yday(dat.lc22$Date.Observed)
summary(dat.lc22)


#only looking at trees that showed fall color from 9/1 on
dat.llc22 <- dat.lc22 [dat.lc22$yday>=180,]
summary(dat.llc22)

#aggregating quercus.lf so it shows me the date of first leaf color for  every plant number and species 
leaf.color22 <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.llc22, FUN=min, na.rm=T)
summary(leaf.color22)
head(leaf.color22)

#Graphing

ggplot(data=leaf.color22) +
  # png(file.path(path.figs,"All_First_Leaf_Color.png"), height=4, width=6, units="in", res=320)+
  #facet_grid(Collection~ . ) + # This is the code that will stack everything
  geom_bar(alpha=5,aes(x=yday, fill=as.factor(Collection), color=as.factor(Collection))) + ylim(-100,120) +
  #scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442")) +
 # scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442")) +
  theme_bw()+
  labs(title="Leaf Color Present", x="Day of Year")+
  coord_polar(start = 0)

###### First animated 
P = ggplot(data=leaf.color22)+ 
  geom_bar (aes(x =yday, fill=as.factor(Collection), color=as.factor(Collection))) + ylim(-100,120)+
  theme_bw()+
  #labs(title="Leaf Color Present", x="Day of Year")+
 #coord_polar(start = 0)+
  labs(title = "Data over time")


anim = P + transition_states(yday, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP per Year : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data")

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4
anim_save("animation.mp4", animation = for_mp4 )

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))

  
  
  a <- data.frame(group=c("A","B","C"), values=c(3,2,4), frame=rep('a',3))
  b <- data.frame(group=c("A","B","C"), values=c(5,3,7), frame=rep('b',3))
  data <- rbind(a,b)  
  
  # Basic barplot:
  ggplot(a, aes(x=group, y=values, fill=group)) + 
    geom_bar(stat='identity')
  
  # Make a ggplot, but add frame=year: one image per year
  ggplot(data, aes(x=group, y=values, fill=group)) + 
    geom_bar(stat='identity') +
    theme_bw() +
    # gganimate specific bits:
    transition_states(
      frame,
      transition_length = 2,
      state_length = 1
    ) +
    ease_aes('sine-in-out')
  anim_save("288-animated-barplot-transition.gif",animation = for_gif)  
  
  
  
#######
  dat.lci22 <- dat.22[dat.22$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.intensity", "Collection")]
  summary(dat.lci22)
  dat.lci22 <- dat.lci22[!is.na(dat.lci22$PlantNumber),]
  summary(dat.lci22)

dat.lci22$leaf.color.intensity <- gsub("[%]","", dat.lci22$leaf.color.intensity)


  #Checking to make sure date ranges are correct
  min(dat.lci22$Date.Observed)
  max(dat.lci22$Date.Observed)
  mean(dat.lci22$Date.Observed)
  range(dat.lci22$Date.Observed)
  
  #Setting my yday
  dat.lci22$yday <- lubridate::yday(dat.lci22$Date.Observed)
  summary(dat.lci22)
  
  #setting my yday to only show dates later in the season and the current date
  dat.lci22 <- dat.lci22 [dat.lci22$yday>=200,]
  #dat.lci22 <- dat.lci22 [dat.lci22$yday<=Sys.Date(),]
  summary(dat.lci22)
  
  #removing "0 and NA's
  dat.lci22 <- aggregate(yday ~ PlantNumber + Species + Year + Collection + leaf.color.intensity + Date.Observed , dat=dat.lci22, FUN=min, NA.rm=T)
  summary(dat.lci22)
  head(dat.lci22)
  
  dat.lci22$yday <- lubridate::yday(dat.lci22$Date.Observed)
  summary(dat.lci22)
  
  #leaves.present.intensity <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lci, FUN=min, NA.rm=T)
  #summary(leaves.present.intensity)
  #head(leaves.present.intensity)
  
  #png(file.path(path.figs,"Leaf_Present_Intensity.png"), height=4, width=6, units="in", res=320)
  ggplot(data=dat.lci22) +
    geom_histogram(alpha=1.5, binwidth =10, aes(x=yday, fill=leaf.color.intensity,))+
    facet_grid(Collection~ .)+
    #scale_fill_manual(name= "leaf.present.intensity", values=c("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
    #scale_color_manual(name="leaf.present.intensity", values=c("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
    theme_bw()+
    labs(title="Leaf color Intensity", x="Day of Year",)
  dev.off()
 
    ggplot(data=dat.lci22)+
  geom_col(aes(x= yday, y= leaf.color.intensity,fill=leaf.color.intensity,))+
    labs(title = "Data over time", x = "Day", y = "Value")
  
    
     p + transition_time(day) +
     labs(title = "Data over time {frame_time}")  
   