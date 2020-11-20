# End of the year phenology report for Ulmus
# install.packages("devtools")
library('devtools')
# devtools::install_github("usa-npn/rnpn")

library(googlesheets4); library(car); library(lubridate)
library(ggplot2)

#
# Making some directories to help with organization
## NOTE: This assumes you opened R by double-clicking this script in your github folder.  Your working directory (in the bar of the "Console" tab) should be [SOMETHIGN]/Collections-Habitat/scripts
# If it's not, you'll need to set your working directory to be here
# Once you do that, we can use the same file paths without having to worry about differences in where your github folder is vs. mine

if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")

#Downloading 2020 data for acer
acer20 <- clean.google(collection="Acer", dat.yr=2020)
acer20$Collection <- as.factor("Acer")
acer20$Year <- lubridate::year(acer20$Date.Observed)
summary(acer20)

#Getting the data for all Acer
# Downloading 2019 data
acer19 <- clean.google(collection="Acer", dat.yr=2019)
acer19$Collection <- as.factor("Acer")
acer19$Year <- lubridate::year(acer19$Date.Observed)
summary(acer19)


# Putting 2020 and 2019 into the same data frame to make working with it easier (in the long run; might be hard at first)
acer.all <- rbind(acer19, acer20)
summary(acer.all)

#subseting out for individual phenophases
acer.lc <- acer.all[acer.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.observed")]
acer.lc <- acer.lc[!is.na(acer.lc$PlantNumber),]
summary(acer.lc)
head(acer.lc)


#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.lc$Date.Observed)
max(acer.lc$Date.Observed)
range(acer.lc$Date.Observed)
mean(acer.lc$Date.Observed,na.rm=T)

#Now make my Yday
acer.lc$yday <- lubridate::yday(acer.lc$Date.Observed)
summary(acer.lc)

#Also looking at year as well not as important but nice to have
#acer.lc$year <- lubridate::year(acer.lc$Date.Observed)
#summary(acer.lc)

#only looking at trees that showed fall color in the last half of the year
acer.lf <- acer.lc [acer.lc$yday>=180,]
summary(acer.lf)


#aggregating quercus.lf so it shows me the date of first leaf color for  every plant number and species 
afirst.tree <- aggregate (yday ~ PlantNumber + Species + Year, data=acer.lf, FUN=min, na.rm=T)
summary(afirst.tree)
head(afirst.tree)

# making a box plot of all of the species of maple earliest date of leaf color showing in that acer.lf data frame
ggplot(data=afirst.tree) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and maple as needed
ggplot(data=afirst.tree[afirst.tree$Species %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves showed fall color per species
#not per individual. So what is the average day per species that leaves showed fall color
ameanfirst.tree <- aggregate(yday ~ Species + Year, data=afirst.tree, FUN=mean, na.rm=T)
summary(ameanfirst.tree)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
ameanfirst.tree[ameanfirst.tree$Species %in% c("Acer saccharum"),]

# messing aroung with some different plots
ggplot(data=ameanfirst.tree) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=ameanfirst.tree[ameanfirst.tree$Species %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#####
#subseting out for individual phenophases- falling leaves
acer.fl <- acer.all[acer.all$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed")]
acer.fl <- acer.fl[!is.na(acer.fl$PlantNumber),]
summary(acer.fl)
head(acer.fl)


#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.fl$Date.Observed)
max(acer.fl$Date.Observed)
range(acer.fl$Date.Observed)
mean(acer.fl$Date.Observed,na.rm=T)

#Now make my Yday
acer.fl$yday <- lubridate::yday(acer.fl$Date.Observed)
summary(acer.fl)

#Also looking at year as well not as important but nice to have
#acer.lc$year <- lubridate::year(acer.lc$Date.Observed)
#summary(acer.lc)

#only looking at trees that showed falling leaves in the last half of the year
acer.afl <- acer.fl [acer.fl$yday>=180,]
summary(acer.afl)


#aggregating acer.afl so it shows me the date of first falling leaf  for  every plant number and species 
afalling.leaves <- aggregate (yday ~ PlantNumber + Species + Year, data=acer.afl, FUN=min, na.rm=T)
summary(afalling.leaves)
head(afalling.leaves)

# making a box plot of all of the species of maple earliest date of falling leaves showing in that acer.lf data frame
ggplot(data=afalling.leaves) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and maple as needed
ggplot(data=afalling.leaves[afalling.leaves %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves fell per species
#not per individual. So what is the average day per species that leaves showed fall color
afalling.leaves <- aggregate(yday ~ Species + Year, data=afalling.leaves, FUN=mean, na.rm=T)
summary(afalling.leaves)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
afalling.leaves[afalling.leaves$Species %in% c("Acer saccharum"),]

# messing aroung with some different plots
ggplot(data=afalling.leaves) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=afalling.leaves[afalling.leaves$Species %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))