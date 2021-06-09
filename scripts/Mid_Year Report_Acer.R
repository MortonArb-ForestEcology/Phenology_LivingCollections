# End of the year phenology report for Acer
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

#Downloading 2021 data for acer
acer21 <- clean.google(collection="Acer", dat.yr=2021)
acer21$Collection <- as.factor("Acer")
acer21$Year <- lubridate::year(acer21$Date.Observed)
summary(acer21)

#Getting the data for all Acer
# Downloading 2020 data
acer20 <- clean.google(collection="Acer", dat.yr=2020)
acer20$Collection <- as.factor("Acer")
acer20$Year <- lubridate::year(acer20$Date.Observed)
summary(acer20)

# Downloading 2019 data
acer19 <- clean.google(collection="Acer", dat.yr=2019)
acer19$Collection <- as.factor("Acer")
acer19$Year <- lubridate::year(acer19$Date.Observed)
summary(acer19)


# Putting 2021, 2020 and 2019 into the same data frame to make working with it easier (in the long run; might be hard at first)
acer.all <- rbind(acer19, acer20, acer21)
summary(acer.all)

# Putting only 2021, 2019 into the same data frame since 2020 is weird
acer1920.all <- rbind(acer19, acer21)
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

ggplot(data=ameanfirst.tree) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Colored Leaves in the Acer Collection", x="Day of Year")

#####
#subseting out for individual phenophases- flowering buds
acer.fb <- acer.all[acer.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed")]
acer.fb <- acer.fb[!is.na(acer.fb$PlantNumber),]
summary(acer.fb)
head(acer.fb)


#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.fb$Date.Observed)
max(acer.fb$Date.Observed)
range(acer.fb$Date.Observed)
mean(acer.fb$Date.Observed,na.rm=T)

#Now make my Yday
acer.fb$yday <- lubridate::yday(acer.fb$Date.Observed)
summary(acer.fb)

#Also looking at year as well not as important but nice to have
#acer.lc$year <- lubridate::year(acer.lc$Date.Observed)
#summary(acer.lc)

#only looking at trees that showed falling leaves in the last half of the year
#acer.afl <- acer.fl [acer.fl$yday>=180,]
#summary(acer.afl)


#aggregating acer.afl so it shows me the date of first falling leaf  for  every plant number and species 
aflower.buds <- aggregate (yday ~ PlantNumber + Species + Year, data=acer.fb, FUN=min, na.rm=T)
summary(aflower.buds)
head(aflower.buds)

# making a box plot of all of the species of maple earliest date of falling leaves showing in that acer.lf data frame
ggplot(data=aflower.buds) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and maple as needed
ggplot(data=aflower.buds[aflower.buds %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves fell per species
#not per individual. So what is the average day per species that leaves showed fall color
aflower.buds <- aggregate(yday ~ Species + Year, data=aflower.buds, FUN=mean, na.rm=T)
summary(aflower.buds)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
aflower.buds[aflower.buds$Species %in% c("Acer saccharum"),]

# messing aroung with some different plots
ggplot(data=aflower.buds) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=aflower.buds[aflower.buds$Species %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

ggplot(data=aflower.buds) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Flower buds in the Acer Collection", x="Day of Year")




#####
#subseting out for individual phenophases- oper flowers
acer.fo <- acer.all[acer.all$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed")]
acer.fo <- acer.fo[!is.na(acer.fo$PlantNumber),]
summary(acer.fo)
head(acer.fo)


#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.fo$Date.Observed)
max(acer.fo$Date.Observed)
range(acer.fo$Date.Observed)
mean(acer.fo$Date.Observed,na.rm=T)

#Now make my Yday
acer.fo$yday <- lubridate::yday(acer.fo$Date.Observed)
summary(acer.fo)

#Also looking at year as well not as important but nice to have
#acer.lc$year <- lubridate::year(acer.lc$Date.Observed)
#summary(acer.lc)

#only looking at trees that showed falling leaves in the last half of the year
#acer.afl <- acer.fl [acer.fl$yday>=180,]
#summary(acer.afl)


#aggregating acer.afl so it shows me the date of first falling leaf  for  every plant number and species 
aflower.open <- aggregate (yday ~ PlantNumber + Species + Year, data=acer.fo, FUN=min, na.rm=T)
summary(aflower.open)
head(aflower.open)

# making a box plot of all of the species of maple earliest date of falling leaves showing in that acer.lf data frame
ggplot(data=aflower.open) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and maple as needed
ggplot(data=aflower.open[aflower.open %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves fell per species
#not per individual. So what is the average day per species that leaves showed fall color
aflower.open <- aggregate(yday ~ Species + Year, data=aflower.open, FUN=mean, na.rm=T)
summary(aflower.buds)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
aflower.open[aflower.open$Species %in% c("Acer saccharum"),]

# messing aroung with some different plots
ggplot(data=aflower.open) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=aflower.open[aflower.open$Species %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

ggplot(data=aflower.open) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Open Flowers in the Acer Collection", x="Day of Year")



#####
#subseting out for individual phenophases- fruit present
acer.fp <- acer.all[acer.all$fruit.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.present.observed")]
acer.fp <- acer.fp[!is.na(acer.fp$PlantNumber),]
summary(acer.fp)
head(acer.fp)


#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.fp$Date.Observed)
max(acer.fp$Date.Observed)
range(acer.fp$Date.Observed)
mean(acer.fp$Date.Observed,na.rm=T)

#Now make my Yday
acer.fp$yday <- lubridate::yday(acer.fp$Date.Observed)
summary(acer.fp)

#Also looking at year as well not as important but nice to have
#acer.lc$year <- lubridate::year(acer.lc$Date.Observed)
#summary(acer.lc)

#only looking at trees that showed falling leaves in the last half of the year
#acer.afl <- acer.fl [acer.fl$yday>=180,]
#summary(acer.afl)


#aggregating acer.afl so it shows me the date of first falling leaf  for  every plant number and species 
afruit.present <- aggregate (yday ~ PlantNumber + Species + Year, data=acer.fp, FUN=min, na.rm=T)
summary(afruit.present)
head(afruit.present)

# making a box plot of all of the species of maple earliest date of falling leaves showing in that acer.lf data frame
ggplot(data=afruit.present) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and maple as needed
ggplot(data=afruit.present[afruit.present %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves fell per species
#not per individual. So what is the average day per species that leaves showed fall color
afruit.present <- aggregate(yday ~ Species + Year, data=afruit.present, FUN=mean, na.rm=T)
summary(afruit.present)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
afruit.present[afruit.present %in% c("Acer saccharum"),]

# messing aroung with some different plots
ggplot(data=afruit.present) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=afruit.present[afruit.present$Species %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

ggplot(data=afruit.present) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Fruit appearing in the Acer Collection", x="Day of Year")

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

ggplot(data=afalling.leaves) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Falling Leaves in the Acer Collection", x="Day of Year")

#####
#subseting out for individual phenophases- ripe fruit
acer.rf <- acer.all[acer.all$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.ripe.observed")]
acer.rf <- acer.rf[!is.na(acer.rf$PlantNumber),]
summary(acer.rf)
head(acer.rf)


#finding the minimimum and maximum range and mean of the dates ripe fruit was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.rf$Date.Observed)
max(acer.rf$Date.Observed)
range(acer.rf$Date.Observed)
mean(acer.rf$Date.Observed,na.rm=T)


#Now make my Yday
acer.rf$yday <- lubridate::yday(acer.rf$Date.Observed)
summary(acer.rf)

#Also looking at year as well not as important but nice to have
#acer.lc$year <- lubridate::year(acer.lc$Date.Observed)
#summary(acer.lc)

#only looking at trees that showed ripe fruit in the last half of the year
#acer.arf <- acer.rf [acer.rf$yday>=180,]
#summary(acer.arf)


#aggregating acer.afl so it shows me the date of first ripe fruit  for  every plant number and species 
aripe.fruit <- aggregate (yday ~ PlantNumber + Species + Year, data=acer.rf, FUN=min, na.rm=T)
summary(aripe.fruit)
head(aripe.fruit)



# making a box plot of all of the species of maple earliest date of ripe fruit showing in that acer.arf data frame
ggplot(data=aripe.fruit) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and maple as needed
ggplot(data=aripe.fruit[aripe.fruit %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day frit was ripe per species
#not per individual. So what is the average day per species that fruit was ripe
aripe.fruit <- aggregate(yday ~ Species + Year, data=aripe.fruit, FUN=mean, na.rm=T)
summary(aripe.fruit)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
aripe.fruit[aripe.fruit$Species %in% c("Acer saccharum"),]

# messing aroung with some different plots
ggplot(data=aripe.fruit) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=afalling.leaves[afalling.leaves$Species %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))


ggplot(data=aripe.fruit) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Ripe Fruit in the Acer Collection", x="Day of Year")

#####
#subseting out for individual phenophases- fruit drop
acer.fd <- acer.all[acer.all$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.drop.observed")]
acer.fd <- acer.fd[!is.na(acer.fd$PlantNumber),]
summary(acer.fd)
head(acer.fd)


#finding the minimimum and maximum range and mean of the dates fruit drop  was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.fd$Date.Observed)
max(acer.fd$Date.Observed)
range(acer.fd$Date.Observed)
mean(acer.fd$Date.Observed,na.rm=T)

#Now make my Yday
acer.fd$yday <- lubridate::yday(acer.fd$Date.Observed)
summary(acer.fd)

#Also looking at year as well not as important but nice to have
#acer.lc$year <- lubridate::year(acer.lc$Date.Observed)
#summary(acer.lc)

#only looking at trees that showed fruit drop in the last half of the year
#acer.arf <- acer.rf [acer.rf$yday>=180,]
#summary(acer.arf)


#aggregating acer.fd so it shows me the date of first fruit drop  for  every plant number and species 
afruit.drop <- aggregate (yday ~ PlantNumber + Species + Year, data=acer.fd, FUN=min, na.rm=T)
summary(afruit.drop)
head(afruit.drop)

# making a box plot of all of the species of maple earliest date of fruit drop showing in that acer.fd data frame
ggplot(data=afruit.drop) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and maple as needed
ggplot(data=afruit.drop[afruit.drop %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day frit was ripe per species
#not per individual. So what is the average day per species that fruit was ripe
afruit.drop <- aggregate(yday ~ Species + Year, data=afruit.drop, FUN=mean, na.rm=T)
summary(afruit.drop)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
afruit.drop[afruit.drop$Species %in% c("Acer saccharum"),]

# messing aroung with some different plots
ggplot(data=afruit.drop) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=afruit.drop[afruit.drop$Species %in% c("Acr saccharum", "Acer rubrum", "Acer negundo", "Acer henryi", "Acer macrophyllum"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))


ggplot(data=afruit.drop) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Fruit Drop in the Acer Collection", x="Day of Year")

##Getting mean date of falling leaves for individual leaves
#2020
acer.fl <- acer20[acer20$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed")]
acer.fl <- acer.fl[!is.na(acer.fl$PlantNumber),]
summary(acer.fl)
head(acer.fl)


#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.fl$Date.Observed)
max(acer.fl$Date.Observed)
range(acer.fl$Date.Observed)
mean(acer.fl$Date.Observed,na.rm=T)

#2019
acer.fl <- acer19[acer19$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed")]
acer.fl <- acer.fl[!is.na(acer.fl$PlantNumber),]
summary(acer.fl)
head(acer.fl)


#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer.fl$Date.Observed)
max(acer.fl$Date.Observed)
range(acer.fl$Date.Observed)
mean(acer.fl$Date.Observed,na.rm=T)


###############################
#doing this but for intensity
############################
#fruit present intensity
acer1920.fpi <- acer1920.all[acer1920.all$fruit.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.present.intensity", "fruit.present.observed")]
acer1920.fpi <- acer1920.fpi[!is.na(acer1920.fpi$PlantNumber),]
summary(acer1920.fpi)
head(acer1920.fpi)

#finding the minimimum and maximum range and mean of the dates ripe fruit was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer1920.fpi$Date.Observed)
max(acer1920.fpi$Date.Observed)
range(acer1920.fpi$Date.Observed)
mean(acer1920.fpi$Date.Observed,na.rm=T)

#Now make my Yday
acer1920.fpi$yday <- lubridate::yday(acer1920.fpi$Date.Observed)
summary(acer1920.fpi)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed first fruit in the first half of the year
acer1920.fpi <- acer1920.fpi [acer1920.fpi$yday<=180,]
summary(acer1920.fpi)


#aggregating acer1920.fpi so it shows me the date of first fruit present  for  every plant number and species 
ifirst.fruit <- aggregate(yday ~ PlantNumber + Species + Year+ fruit.present.intensity, data=acer1920.fpi, FUN=min, na.rm=T)
summary(ifirst.fruit)
head(ifirst.fruit)

#aggregating the data so it only shows us the average of the first day there were ripe fruit per species
#not per individual. So what is the average day per species that first ripe fruit appeared
meanifirst.fruit <- aggregate(yday ~ Species + Year+ fruit.present.intensity, data=ifirst.fruit, FUN=mean, na.rm=T)
summary(meanifirst.fruit)


##mean fruit present intensity per year per indiviual 
ggplot(data=ifirst.fruit) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=fruit.present.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Fruit Present Intensity", x="Day of Year")

##mean fruit ripe intensity per year per species 
ggplot(data=meanifirst.fruit) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=fruit.present.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Fruit Present Intensity", x="Day of Year")
############
###############################
#doing this but for intensity
############################
#fruit ripe intensity
acer1920.fri <- acer1920.all[acer1920.all$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.ripe.intensity", "fruit.ripe.observed")]
acer1920.fri <- acer1920.fri[!is.na(acer1920.fri$PlantNumber),]
summary(acer1920.fri)
head(acer1920.fri)

#finding the minimimum and maximum range and mean of the dates ripe fruit was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer1920.fri$Date.Observed)
max(acer1920.fri$Date.Observed)
range(acer1920.fri$Date.Observed)
mean(acer1920.fri$Date.Observed,na.rm=T)

#Now make my Yday
acer1920.fri$yday <- lubridate::yday(acer1920.fri$Date.Observed)
summary(acer1920.fri)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed first fruit in the first half of the year
acer1920.fri <- acer1920.fri [acer1920.fri$yday<=180,]
summary(acer1920.fri)


#aggregating acer1920.fpi so it shows me the date of ripe fruit present  for  every plant number and species 
iripe.fruit <- aggregate(yday ~ PlantNumber + Species + Year+ fruit.ripe.intensity, data=acer1920.fri, FUN=min, na.rm=T)
summary(iripe.fruit)
head(iripe.fruit)

#aggregating the data so it only shows us the average of the first day there were ripe fruit per species
#not per individual. So what is the average day per species that first ripe fruit appeared
meaniripe.fruit <- aggregate(yday ~ Species + Year+ fruit.ripe.intensity, data=iripe.fruit, FUN=mean, na.rm=T)
summary(meanifirst.fruit)


##mean fruit ripe intensity per year per indiviual 
ggplot(data=iripe.fruit) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=fruit.ripe.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Fruit Ripe Intensity", x="Day of Year")

##mean fruit ripe intensity per year per species 
ggplot(data=meaniripe.fruit) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=fruit.ripe.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Fruit Ripe Intensity", x="Day of Year")
############
#################
#flower buds intensity
acer1920.fbi <- acer1920.all[acer1920.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.intensity", "flower.buds.observed")]
acer1920.fbi <- acer1920.fbi[!is.na(acer1920.fbi$PlantNumber),]
summary(acer1920.fbi)
head(acer1920.fbi)

#finding the minimimum and maximum range and mean of the dates flower buds were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer1920.fbi$Date.Observed)
max(acer1920.fbi$Date.Observed)
range(acer1920.fbi$Date.Observed)
mean(acer1920.fbi$Date.Observed,na.rm=T)

#Now make my Yday
acer1920.fbi$yday <- lubridate::yday(acer1920.fbi$Date.Observed)
summary(acer1920.fbi)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed flower buds in the first half of the year
acer1920.fbi <- acer1920.fbi [acer1920.fbi$yday<=180,]
summary(acer1920.fbi)


#aggregating acer1920.fbi so it shows me the date offlower buds present  for  every plant number and species 
iflower.buds <- aggregate(yday ~ PlantNumber + Species + Year+ flower.buds.intensity, data=acer1920.fbi, FUN=min, na.rm=T)
summary(iflower.buds)
head(iflower.buds)

#aggregating the data so it only shows us the average of the first day there were flower buds per species
#not per individual. So what is the average day per species that first flower buds appeared
meaniflower.buds <- aggregate(yday ~ Species + Year+ flower.buds.intensity, data=iflower.buds, FUN=mean, na.rm=T)
summary(meaniflower.buds)


##mean flower bud intensity per year per indiviual 
ggplot(data=iflower.buds) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=flower.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Flower Bud Intensity", x="Day of Year")

##mean fruit ripe intensity per year per species 
ggplot(data=meaniflower.buds) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=flower.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Flower Buds Intensity", x="Day of Year")


##########
###############
#Open flower intensity
acer1920.foi <- acer1920.all[acer1920.all$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.intensity", "flower.open.observed")]
acer1920.foi <- acer1920.foi[!is.na(acer1920.foi$PlantNumber),]
summary(acer1920.foi)
head(acer1920.foi)

#finding the minimimum and maximum range and mean of the dates open flower were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(acer1920.foi$Date.Observed)
max(acer1920.foi$Date.Observed)
range(acer1920.foi$Date.Observed)
mean(acer1920.foi$Date.Observed,na.rm=T)

#Now make my Yday
acer1920.foi$yday <- lubridate::yday(acer1920.foi$Date.Observed)
summary(acer1920.foi)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed open flower in the first half of the year
acer1920.foi <- acer1920.foi [acer1920.foi$yday<=180,]
summary(acer1920.foi)


#aggregating acer1920.foi so it shows me the date of open flowers present  for  every plant number and species 
iflower.open <- aggregate(yday ~ PlantNumber + Species + Year+ flower.open.intensity, data=acer1920.foi, FUN=min, na.rm=T)
summary(iflower.open)
head(iflower.open)

#aggregating the data so it only shows us the average of the first day there were open flowers per species
#not per individual. So what is the average day per species that first open flowers  appeared
meaniflower.open <- aggregate(yday ~ Species + Year+ flower.open.intensity, data=iflower.open, FUN=mean, na.rm=T)
summary(meaniflower.open)


##mean Open flower intensity per year per indiviual 
ggplot(data=iflower.open) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=flower.open.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Open Flower Intensity", x="Day of Year")

##mean Open Flower intensity per year per species 
ggplot(data=meaniflower.open) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=flower.open.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Open Flowers Intensity", x="Day of Year")
