# Attempting to get the min and max for leaves in one Quercus Macrocarpa

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
path.figs <- "G://My Drive/LivingCollections_Phenology/Reports/2021_01_MidYear_Report/figures_spring_2021"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")



# Downloading 2021 data
quercus21 <- clean.google(collection="Quercus", dat.yr=2021)
quercus21$Collection <- as.factor("Quercus")
quercus21$Year <- lubridate::year(quercus21$Date.Observed)
summary(quercus21)

# Putting 2021 into the a data frame to make working with it easier
quercus21 <- rbind(quercus21)
summary(quercus21) # makign sure this worked; check date & year columns to make sure they make sense

head(quercus21)

# Downloading 2020 data
quercus20 <- clean.google(collection="Quercus", dat.yr=2020)
quercus20$Collection <- as.factor("Quercus")
quercus20$Year <- lubridate::year(quercus20$Date.Observed)
summary(quercus20)

# Downloading 2019 data
quercus19 <- clean.google(collection="Quercus", dat.yr=2019)
quercus19$Collection <- as.factor("Quercus")
quercus19$Year <- lubridate::year(quercus19$Date.Observed)
summary(quercus19)

# Downloading 2018 data
quercus18 <- clean.google(collection="Quercus", dat.yr=2018)
quercus18$Collection <- as.factor("Quercus")
quercus18$Year <- lubridate::year(quercus18$Date.Observed)
summary(quercus18)

# Putting 2021, 2020, 2019 and 2018 into the same data frame to make working with it easier (in the long run; might be hard at first)
quercus.all <- rbind(quercus18, quercus19, quercus20, quercus21 )
summary(quercus.all)

#Only getting 18,19,and 21 since spring 2020 was a loss
quercus.891 <- rbind(quercus18, quercus19, quercus21 )
summary(quercus.891)

###Subsetting according to leaf breaking buds
quercus.bb <- quercus.all[quercus.all$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.breaking.buds.observed")]
quercus.bb <- quercus.bb[!is.na(quercus.bb$PlantNumber),]
summary(quercus.bb)
head(quercus.bb)

#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.bb$Date.Observed)
max(quercus.bb$Date.Observed)
range(quercus.bb$Date.Observed)
mean(quercus.bb$Date.Observed,na.rm=T)

#Now make my Yday
quercus.bb$yday <- lubridate::yday(quercus.bb$Date.Observed)
summary(quercus.bb)

#Also looking at year as well not as important but nice to have
#quercus.lc$year <- lubridate::year(quercus.lc$Date.Observed)
#summary(quercus.lc)

#only looking at trees that showed fall color in the last half of the year
#quercus.lf <- quercus.lc [quercus.lc$yday>=180,]
#summary(quercus.lf)


#aggregating quercus.bb so it shows me the date of first leaf color for  every plant number and species 
first.tree <- aggregate(yday ~ PlantNumber + Species + Year, data=quercus.bb, FUN=min, na.rm=T)
summary(first.tree)
head(first.tree)
# making a box plot of all of the species of oak earliest date of leaf color showing in that quercus.lf data frame
ggplot(data=first.tree) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and oak as needed
ggplot(data=first.tree[first.tree$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves showed fall color per species
#not per individual. So what is the average day per species that leaves showed fall color
meanfirst.tree <- aggregate(yday ~ Species + Year, data=first.tree, FUN=mean, na.rm=T)
summary(meanfirst.tree)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
meanfirst.tree[meanfirst.tree$Species %in% c("Quercus macrocarpa"),]

# messing aroung with some different plots
ggplot(data=meanfirst.tree) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=meanfirst.tree[meanfirst.tree$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) 


###Subsetting according to leaf color
#maybe this?
#quercus.fr <- subset(quercus.all [,c("PlantNumber","leaf.color.observed")], simplify = TRUE, drop = TRUE)
#summary (quercus.fr)

#I can also try this
quercus.lc <- quercus.all[quercus.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.observed")]
quercus.lc <- quercus.lc[!is.na(quercus.lc$PlantNumber),]
summary(quercus.lc)
head(quercus.lc)

#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.lc$Date.Observed)
max(quercus.lc$Date.Observed)
range(quercus.lc$Date.Observed)
mean(quercus.lc$Date.Observed,na.rm=T)

#Now make my Yday
quercus.lc$yday <- lubridate::yday(quercus.lc$Date.Observed)
summary(quercus.lc)

#Also looking at year as well not as important but nice to have
#quercus.lc$year <- lubridate::year(quercus.lc$Date.Observed)
#summary(quercus.lc)

#only looking at trees that showed fall color in the last half of the year
quercus.lf <- quercus.lc [quercus.lc$yday>=180,]
summary(quercus.lf)


#aggregating quercus.lf so it shows me the date of first leaf color for  every plant number and species 
first.tree <- aggregate(yday ~ PlantNumber + Species + Year, data=quercus.lf, FUN=min, na.rm=T)
summary(first.tree)
head(first.tree)
# making a box plot of all of the species of oak earliest date of leaf color showing in that quercus.lf data frame
ggplot(data=first.tree) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and oak as needed
ggplot(data=first.tree[first.tree$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves showed fall color per species
#not per individual. So what is the average day per species that leaves showed fall color
meanfirst.tree <- aggregate(yday ~ Species + Year, data=first.tree, FUN=mean, na.rm=T)
summary(meanfirst.tree)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
meanfirst.tree[meanfirst.tree$Species %in% c("Quercus macrocarpa"),]

# messing aroung with some different plots
ggplot(data=meanfirst.tree) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=meanfirst.tree[meanfirst.tree$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) 

#########
#subsetting out for individual phenophases- falling leaves
quercus.lfl <- quercus.all[quercus.all$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed")]
quercus.lfl <- quercus.lfl[!is.na(quercus.lfl$PlantNumber),]
summary(quercus.lfl)
head(quercus.lfl)

#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.lfl$Date.Observed)
max(quercus.lfl$Date.Observed)
range(quercus.lfl$Date.Observed)
mean(quercus.lfl$Date.Observed,na.rm=T)

#Now make my Yday
quercus.lfl$yday <- lubridate::yday(quercus.lfl$Date.Observed)
summary(quercus.lfl)

#Also looking at year as well not as important but nice to have
#quercus.lc$year <- lubridate::year(quercus.lc$Date.Observed)
#summary(quercus.lc)

#only looking at trees that showed falling leaves in the last half of the year
quercus.lfl <- quercus.lfl [quercus.lfl$yday>=180,]
summary(quercus.lfl)


#aggregating quercus.lf so it shows me the date of first leaf color for  every plant number and species 
falling.leaves <- aggregate(yday ~ PlantNumber + Species + Year, data=quercus.lfl, FUN=min, na.rm=T)
summary(falling.leaves)
head(falling.leaves)
# making a box plot of all of the species of oak earliest date of leaf color showing in that quercus.lf data frame
ggplot(data=falling.leaves) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and oak as needed
ggplot(data=falling.leaves[falling.leaves$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves showed fall color per species
#not per individual. So what is the average day per species that leaves showed fall color
meanfalling.leaves <- aggregate(yday ~ Species + Year, data=falling.leaves, FUN=mean, na.rm=T)
summary(meanfalling.leaves)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
meanfalling.leaves[meanfalling.leaves$Species %in% c("Quercus macrocarpa"),]

# messing aroung with some different plots
ggplot(data=meanfalling.leaves) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=meanfalling.leaves[meanfalling.leaves$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) 


ggplot(data=meanfalling.leaves) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Falling Leaves", x="Day of Year")


#########
#subsetting out for individual phenophases fruit present
quercus.fp <- quercus.all[quercus.all$fruit.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.present.observed")]
quercus.fp <- quercus.fp[!is.na(quercus.fp$PlantNumber),]
summary(quercus.fp)
head(quercus.fp)

#finding the minimimum and maximum range and mean of the dates fruits were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fp$Date.Observed)
max(quercus.fp$Date.Observed)
range(quercus.fp$Date.Observed)
mean(quercus.fp$Date.Observed,na.rm=T)

#Now make my Yday
quercus.fp$yday <- lubridate::yday(quercus.fp$Date.Observed)
summary(quercus.fp)

#Also looking at year as well not as important but nice to have
#quercus.lc$year <- lubridate::year(quercus.lc$Date.Observed)
#summary(quercus.lc)

#only looking at trees that showed fruit in the last half of the year
quercus.lfp <- quercus.fp [quercus.fp$yday>=180,]
summary(quercus.lfp)


#aggregating quercus.lfp so it shows me the date of first fruit presence for  every plant number and species 
fruit.present<- aggregate(yday ~ PlantNumber + Species + Year, data=quercus.lfp, FUN=min, na.rm=T)
summary(fruit.present)
head(falling.leaves)
# making a box plot of all of the species of oak earliest date of fruit present showing in that quercus.lf data frame
ggplot(data=fruit.present) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and oak as needed
ggplot(data=fruit.present[fruit.present$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day fruit was present per species
#not per individual. So what is the average day per species that leaves showed fall color
meanfruit.present <- aggregate(yday ~ Species + Year, data=fruit.present, FUN=mean, na.rm=T)
summary(meanfruit.present)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
meanfruit.present[meanfruit.present$Species %in% c("Quercus macrocarpa"),]

# messing aroung with some different plots
ggplot(data=meanfruit.present) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=meanfruit.present[meanfruit.present$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) 


ggplot(data=meanfruit.present) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Fruit Present", x="Day of Year")


#########
#subsetting out for individual phenophases fruit ripe
quercus.fr <- quercus.all[quercus.all$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.ripe.observed")]
quercus.fr <- quercus.fr[!is.na(quercus.fr$PlantNumber),]
summary(quercus.fr)
head(quercus.fr)

#finding the minimimum and maximum range and mean of the dates ripe fruits were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fr$Date.Observed)
max(quercus.fr$Date.Observed)
range(quercus.fr$Date.Observed)
mean(quercus.fr$Date.Observed,na.rm=T)

#Now make my Yday
quercus.fr$yday <- lubridate::yday(quercus.fr$Date.Observed)
summary(quercus.fr)

#Also looking at year as well not as important but nice to have
#quercus.lc$year <- lubridate::year(quercus.lc$Date.Observed)
#summary(quercus.lc)

#only looking at trees that showed ripe fruit in the last half of the year
quercus.lfr <- quercus.fr [quercus.fr$yday>=180,]
summary(quercus.lfr)


#aggregating quercus.lfp so it shows me the date of first ripe fruit presence for  every plant number and species 
fruit.ripe<- aggregate(yday ~ PlantNumber + Species + Year, data=quercus.lfr, FUN=min, na.rm=T)
summary(fruit.ripe)
head(fruit.ripe)
# making a box plot of all of the species of oak earliest date of ripe fruit present showing in that quercus.lrf data frame
ggplot(data=fruit.ripe) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and oak as needed
ggplot(data=fruit.ripe[fruit.ripe$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day ripe fruit was present per species
#not per individual. So what is the average day per species that leaves showed fall color
meanfruit.ripe <- aggregate(yday ~ Species + Year, data=fruit.ripe, FUN=mean, na.rm=T)
summary(meanfruit.ripe)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
meanfruit.ripe[meanfruit.ripe$Species %in% c("Quercus macrocarpa"),]

# messing aroung with some different plots
ggplot(data=meanfruit.ripe) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))


ggplot(data=meanfruit.ripe[meanfruit.ripe$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))


ggplot(data=meanfruit.ripe) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Ripe Fruit", x="Day of Year")



#########
#subsetting out for individual phenophases fruit drop
quercus.fd <- quercus.all[quercus.all$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.drop.observed")]
quercus.fd <- quercus.fd[!is.na(quercus.fd$PlantNumber),]
summary(quercus.fd)
head(quercus.fd)

#finding the minimimum and maximum range and mean of the dates ripe fruits were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fd$Date.Observed)
max(quercus.fd$Date.Observed)
range(quercus.fd$Date.Observed)
mean(quercus.fd$Date.Observed,na.rm=T)

#Now make my Yday
quercus.fd$yday <- lubridate::yday(quercus.fd$Date.Observed)
summary(quercus.fd)

#Also looking at year as well not as important but nice to have
#quercus.lc$year <- lubridate::year(quercus.lc$Date.Observed)
#summary(quercus.lc)

#only looking at trees that showed ripe fruit in the last half of the year
quercus.lfd <- quercus.fd [quercus.fr$yday>=180,]
summary(quercus.lfd)


#aggregating quercus.lfp so it shows me the date of first ripe fruit presence for  every plant number and species 
fruit.drop<- aggregate(yday ~ PlantNumber + Species + Year, data=quercus.lfd, FUN=min, na.rm=T)
summary(fruit.drop)
head(fruit.drop)
# making a box plot of all of the species of oak earliest date of ripe fruit present showing in that quercus.lrf data frame
ggplot(data=fruit.drop) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and oak as needed
ggplot(data=fruit.drop[fruit.drop$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day ripe fruit was present per species
#not per individual. So what is the average day per species that leaves showed fall color
meanfruit.drop <- aggregate(yday ~ Species + Year, data=fruit.drop, FUN=mean, na.rm=T)
summary(meanfruit.drop)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
meanfruit.drop[meanfruit.drop$Species %in% c("Quercus macrocarpa"),]

# messing aroung with some different plots
ggplot(data=meanfruit.drop) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=meanfruit.drop[meanfruit.drop$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) 

ggplot(data=meanfruit.drop) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Fruit Drop", x="Day of Year")

####Getting broader information across years for phenophases
quercus.lc <- quercus.all[quercus.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.observed")]
quercus.lc <- quercus.lc[!is.na(quercus.lc$PlantNumber),]
summary(quercus.lc)
head(quercus.lc)

#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.lc$Date.Observed)
max(quercus.lc$Date.Observed)
range(quercus.lc$Date.Observed)
mean(quercus.lc$Date.Observed,na.rm=T)

#Now make my Yday
quercus.lc$yday <- lubridate::yday(quercus.lc$Date.Observed)
summary(quercus.lc)

#Also looking at year as well not as important but nice to have
#quercus.lc$year <- lubridate::year(quercus.lc$Date.Observed)
#summary(quercus.lc)

#only looking at trees that showed fall color in the last half of the year
quercus.lf <- quercus.lc [quercus.lc$yday>=180,]
summary(quercus.lf)


#aggregating quercus.lf so it shows me the date of first leaf color for  every plant number and species 
first.tree <- aggregate(yday ~ PlantNumber + Species + Year, data=quercus.lf, FUN=min, na.rm=T)
summary(first.tree)
head(first.tree)
# making a box plot of all of the species of oak earliest date of leaf color showing in that quercus.lf data frame
ggplot(data=first.tree) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and oak as needed
ggplot(data=first.tree[first.tree$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves showed fall color per species
#not per individual. So what is the average day per species that leaves showed fall color
meanfirst.tree <- aggregate(yday ~ Species + Year, data=first.tree, FUN=mean, na.rm=T)
summary(meanfirst.tree)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
meanfirst.tree[meanfirst.tree$Species %in% c("Quercus macrocarpa"),]

# messing aroung with some different plots
ggplot(data=meanfirst.tree) +
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme_bw()+
  labs(title="Average Day of First Leaf Color", x="Day of Year")



ggplot(data=meanfirst.tree[meanfirst.tree$Species %in% c("Quercus macrocarpa", "Quercus montana", "Quercus bicolor", "Quercus alba", "Quercus rubra"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) 

##Getting mean date of falling leaves just a single year
#2020
quercus.lfl <- quercus20[quercus20$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed")]
quercus.lfl <- quercus.lfl[!is.na(quercus.lfl$PlantNumber),]
summary(quercus.lfl)
head(quercus.lfl)

#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.lfl$Date.Observed)
max(quercus.lfl$Date.Observed)
range(quercus.lfl$Date.Observed)
mean(quercus.lfl$Date.Observed,na.rm=T)

#2019
quercus.lfl <- quercus19[quercus19$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed")]
quercus.lfl <- quercus.lfl[!is.na(quercus.lfl$PlantNumber),]
summary(quercus.lfl)
head(quercus.lfl)

#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.lfl$Date.Observed)
max(quercus.lfl$Date.Observed)
range(quercus.lfl$Date.Observed)
mean(quercus.lfl$Date.Observed,na.rm=T)

#2018
quercus.lfl <- quercus18[quercus18$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed")]
quercus.lfl <- quercus.lfl[!is.na(quercus.lfl$PlantNumber),]
summary(quercus.lfl)
head(quercus.lfl)

#finding the minimimum and maximum range and mean of the dates falling leaves were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.lfl$Date.Observed)
max(quercus.lfl$Date.Observed)
range(quercus.lfl$Date.Observed)
mean(quercus.lfl$Date.Observed,na.rm=T)

#Doing the above for Ripe Fruit
#2020
quercus.fr <- quercus20[quercus20$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.ripe.observed")]
quercus.fr <- quercus.fr[!is.na(quercus.fr$PlantNumber),]
summary(quercus.fr)
head(quercus.fr)

#finding the minimimum and maximum range and mean of the dates ripe fruits were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fr$Date.Observed)
max(quercus.fr$Date.Observed)
range(quercus.fr$Date.Observed)
mean(quercus.fr$Date.Observed,na.rm=T)

#2019
quercus.fr <- quercus19[quercus19$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.ripe.observed")]
quercus.fr <- quercus.fr[!is.na(quercus.fr$PlantNumber),]
summary(quercus.fr)
head(quercus.fr)

#finding the minimimum and maximum range and mean of the dates ripe fruits were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fr$Date.Observed)
max(quercus.fr$Date.Observed)
range(quercus.fr$Date.Observed)
mean(quercus.fr$Date.Observed,na.rm=T)

#2018
quercus.fr <- quercus18[quercus18$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.ripe.observed")]
quercus.fr <- quercus.fr[!is.na(quercus.fr$PlantNumber),]
summary(quercus.fr)
head(quercus.fr)

#finding the minimimum and maximum range and mean of the dates ripe fruits were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fr$Date.Observed)
max(quercus.fr$Date.Observed)
range(quercus.fr$Date.Observed)
mean(quercus.fr$Date.Observed,na.rm=T)

#Doing the above but for fruit drop
#2020
quercus.fd <- quercus20[quercus20$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.drop.observed")]
quercus.fd <- quercus.fd[!is.na(quercus.fd$PlantNumber),]
summary(quercus.fd)
head(quercus.fd)

#finding the minimimum and maximum range and mean of the dates ripe fruits were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fd$Date.Observed)
max(quercus.fd$Date.Observed)
range(quercus.fd$Date.Observed)
mean(quercus.fd$Date.Observed,na.rm=T)

#2019
quercus.fd <- quercus19[quercus19$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.drop.observed")]
quercus.fd <- quercus.fd[!is.na(quercus.fd$PlantNumber),]
summary(quercus.fd)
head(quercus.fd)

#finding the minimimum and maximum range and mean of the dates ripe fruits were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fd$Date.Observed)
max(quercus.fd$Date.Observed)
range(quercus.fd$Date.Observed)
mean(quercus.fd$Date.Observed,na.rm=T)

#2018
quercus.fd <- quercus18[quercus18$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.drop.observed")]
quercus.fd <- quercus.fd[!is.na(quercus.fd$PlantNumber),]
summary(quercus.fd)
head(quercus.fd)

#finding the minimimum and maximum range and mean of the dates ripe fruits were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fd$Date.Observed)
max(quercus.fd$Date.Observed)
range(quercus.fd$Date.Observed)
mean(quercus.fd$Date.Observed,na.rm=T)


#####
###
######Getting graphs of intensity for fruit and leaf phenophases#####
###
#####
#Breaking buds
quercus.bbi <- quercus.891[quercus.all$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.breaking.buds.intensity", "leaf.breaking.buds.observed")]
quercus.bbi <- quercus.bbi[!is.na(quercus.bbi$PlantNumber),]
summary(quercus.bbi)
head(quercus.bbi)

#finding the minimimum and maximum range and mean of the dates Leaf Breaking Bud was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.bbi$Date.Observed)
max(quercus.bbi$Date.Observed)
range(quercus.bbi$Date.Observed)
mean(quercus.bbi$Date.Observed,na.rm=T)

#Now make my Yday
quercus.bbi$yday <- lubridate::yday(quercus.bbi$Date.Observed)
summary(quercus.bbi)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed ripe fruit in the first half of the year
quercus.bbi <- quercus.bbi [quercus.bbi$yday<=180,]
summary(quercus.bbi)


#aggregating quercus.bbi so it shows me the date of first Leaf Breaking Bud for  every plant number and species 
ifirst.bud <- aggregate(yday ~ PlantNumber + Species + Year+ leaf.breaking.buds.intensity, data=quercus.bbi, FUN=min, na.rm=T)
summary(ifirst.bud)
head(ifirst.bud)

#aggregating the data so it only shows us the average of the first day there were leaf breaking buds per species
#not per individual. So what is the average day per species that first leaf breaking buds appeared
meanifirst.bud <- aggregate(yday ~ Species + Year+ leaf.breaking.buds.intensity, data=ifirst.bud, FUN=mean, na.rm=T)
summary(meanifirst.bud)


##mean Leaf Breaking Bud intensity per year per indiviual 
ggplot(data=ifirst.bud) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=leaf.breaking.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaf Breaking Bud Intensity", x="Day of Year")

##mean Leaf Breaking Bud intensity per year per species 
ggplot(data=meanifirst.bud) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=leaf.breaking.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaf Breaking Bud", x="Day of Year")

ggplot(data=meanifirst.bud) +
  geom_histogram(alpha=1.5, aes(x=yday, fill=leaf.breaking.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaf Breaking Bud", x="Day of Year")

png(file.path(path.figs,"Quercus_Breaking_Leaf_Bud_desnsity.png"), height=4, width=6, units="in", res=320)
ggplot(data=meanifirst.bud) +
  geom_density(alpha=1.5, aes(x=yday, fill=leaf.breaking.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaf Breaking Bud", x="Day of Year")
dev.off()


###########
#fruit ripe
quercus.fri <- quercus.all[quercus.all$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.ripe.intensity", "fruit.ripe.observed")]
quercus.fri <- quercus.fri[!is.na(quercus.fri$PlantNumber),]
summary(quercus.fri)
head(quercus.fri)

#finding the minimimum and maximum range and mean of the dates ripe fruit was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fri$Date.Observed)
max(quercus.fri$Date.Observed)
range(quercus.fri$Date.Observed)
mean(quercus.fri$Date.Observed,na.rm=T)

#Now make my Yday
quercus.fri$yday <- lubridate::yday(quercus.fri$Date.Observed)
summary(quercus.fri)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed ripe fruit in the last half of the year
quercus.lfri <- quercus.fri [quercus.fri$yday>=180,]
summary(quercus.lfri)


#aggregating quercus.lfri so it shows me the date of first ripe fruit  for  every plant number and species 
ifirst.fruit <- aggregate(yday ~ PlantNumber + Species + Year+ fruit.ripe.intensity, data=quercus.lfri, FUN=min, na.rm=T)
summary(ifirst.fruit)
head(ifirst.fruit)

#aggregating the data so it only shows us the average of the first day there were ripe fruit per species
#not per individual. So what is the average day per species that first ripe fruit appeared
meanifirst.fruit <- aggregate(yday ~ Species + Year+ fruit.ripe.intensity, data=ifirst.fruit, FUN=mean, na.rm=T)
summary(meanifirst.friuit)


##mean fruit ripe instensit per year per indiviual 
ggplot(data=ifirst.fruit) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=fruit.ripe.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Fruit Ripe Intensity", x="Day of Year")

##mean fruit ripe instensit per year per species 
ggplot(data=meanifirst.fruit) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=fruit.ripe.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Fruit Ripe Intensity", x="Day of Year")
#####
##
######Doing the above but for fruit drop#########
##
####
quercus.fdi <- quercus.all[quercus.all$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "fruit.drop.intensity", "fruit.drop.observed")]
quercus.fdi <- quercus.fdi[!is.na(quercus.fdi$PlantNumber),]
summary(quercus.fdi)
head(quercus.fdi)

#finding the minimimum and maximum range and mean of the dates fruit drop was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fdi$Date.Observed)
max(quercus.fdi$Date.Observed)
range(quercus.fdi$Date.Observed)
mean(quercus.fdi$Date.Observed,na.rm=T)

#Now make my Yday
quercus.fdi$yday <- lubridate::yday(quercus.fdi$Date.Observed)
summary(quercus.fdi)

#Also looking at year as well not as important but nice to have
#quercus.fdi$year <- lubridate::year(quercus.fdi$Date.Observed)
#summary(quercus.fdi)

#only looking at trees that showed fruit drop in the last half of the year
quercus.lfdi <- quercus.fdi [quercus.fdi$yday>=180,]
summary(quercus.lfdi)


#aggregating quercus.lfdi so it shows me the date of first fruit drop  for  every plant number and species 
ifruit.drop <- aggregate(yday ~ PlantNumber + Species + Year+ fruit.drop.intensity, data=quercus.lfdi, FUN=min, na.rm=T)
summary(ifruit.drop)
head(ifruit.drop)

#aggregating the data so it only shows us the average of the first day there was fruit drop per species
#not per individual. So what is the average day per species that fruit drop first appeared
meanifruit.drop <- aggregate(yday ~ Species + Year+ fruit.drop.intensity, data=ifruit.drop, FUN=mean, na.rm=T)
summary(meanifruit.drop)


##mean fruit ripe instensit per year per indiviual 
ggplot(data=ifruit.drop) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=fruit.drop.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Fruit Drop Intensity", x="Day of Year")

##mean fruit ripe instensit per year per species 
ggplot(data=meanifruit.drop) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=fruit.drop.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Fruit Drop Intensity", x="Day of Year")
#######
#Doing the above but for Leaves Present
quercus.lpi <- quercus.891[quercus.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.intensity", "leaf.present.observed")]
quercus.lpi <- quercus.lpi[!is.na(quercus.lpi$PlantNumber),]
summary(quercus.lpi)
head(quercus.lpi)

#finding the minimimum and maximum range and mean of the dates Leaf Breaking Bud was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.lpi$Date.Observed)
max(quercus.lpi$Date.Observed)
range(quercus.lpi$Date.Observed)
mean(quercus.lpi$Date.Observed,na.rm=T)

#Now make my Yday
quercus.lpi$yday <- lubridate::yday(quercus.lpi$Date.Observed)
summary(quercus.lpi)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed ripe fruit in the first half of the year
quercus.lpi <- quercus.lpi [quercus.lpi$yday<=180,]
summary(quercus.lpi)


#aggregating quercus.bbi so it shows me the date of first Leaf Breaking Bud for  every plant number and species 
ileaf.present <- aggregate(yday ~ PlantNumber + Species + Year+ leaf.present.intensity, data=quercus.lpi, FUN=min, na.rm=T)
summary(ileaf.present)
head(ileaf.present)

#aggregating the data so it only shows us the average of the first day there were leaf breaking buds per species
#not per individual. So what is the average day per species that first leaf breaking buds appeared
meanileaf.present <- aggregate(yday ~ Species + Year+ leaf.present.intensity, data=ileaf.present, FUN=mean, na.rm=T)
summary(meanileaf.present)


##mean Leaf Breaking Bud intensity per year per indiviual 
ggplot(data=ileaf.present) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=leaf.present.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaf Present Intensity", x="Day of Year")

##mean Leaf Breaking Bud intensity per year per species 
ggplot(data=meanileaf.present) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=leaf.present.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaves Present", x="Day of Year")

ggplot(data=meanifirst.bud) +
  geom_histogram(alpha=1.5, aes(x=yday, fill=leaf.present.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaves Present", x="Day of Year")

png(file.path(path.figs,"Quercus_Leaf_Present_density.png"), height=4, width=6, units="in", res=320)
ggplot(data=meanileaf.present) +
  geom_density(alpha=1.5, aes(x=yday, fill=leaf.present.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaves Present", x="Day of Year")
dev.off()

#######
#Doing the above but for Leaves Increasing in size
quercus.lii <- quercus.891[quercus.all$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.increasing.intensity", "leaf.increasing.observed")]
quercus.lii <- quercus.lii[!is.na(quercus.lii$PlantNumber),]
summary(quercus.lii)
head(quercus.lii)

#finding the minimimum and maximum range and mean of the dates Leaf Breaking Bud was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.lii$Date.Observed)
max(quercus.lii$Date.Observed)
range(quercus.lii$Date.Observed)
mean(quercus.lii$Date.Observed,na.rm=T)

#Now make my Yday
quercus.lii$yday <- lubridate::yday(quercus.lii$Date.Observed)
summary(quercus.lii)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed ripe fruit in the first half of the year
quercus.lii <- quercus.lii [quercus.lii$yday<=180,]
summary(quercus.lii)


#aggregating quercus.bbi so it shows me the date of first Leaf Breaking Bud for  every plant number and species 
ileaf.increasing <- aggregate(yday ~ PlantNumber + Species + Year+ leaf.increasing.intensity, data=quercus.lii, FUN=min, na.rm=T)
summary(ileaf.increasing)
head(ileaf.increasing)

#aggregating the data so it only shows us the average of the first day there were leaf breaking buds per species
#not per individual. So what is the average day per species that first leaf breaking buds appeared
meanileaf.increasing <- aggregate(yday ~ Species + Year+ leaf.increasing.intensity, data=ileaf.increasing, FUN=mean, na.rm=T)
summary(meanileaf.increasing)


##mean Leaf Breaking Bud intensity per year per indiviual 
ggplot(data=ileaf.increasing) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=leaf.increasing.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaf Increasing in Size Intensity", x="Day of Year")

##mean Leaf Breaking Bud intensity per year per species 
ggplot(data=meanileaf.increasing) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=leaf.increasing.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaves Increasing in Size", x="Day of Year")

ggplot(data=meanileaf.increasing) +
  geom_histogram(alpha=1.5, aes(x=yday, fill=leaf.increasing.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaves Increasing in Size", x="Day of Year")

png(file.path(path.figs,"Quercus_Leaf_Increasing_in_Size_density.png"), height=4, width=6, units="in", res=320)
ggplot(data=meanileaf.increasing) +
  geom_density(alpha=1.5, aes(x=yday, fill=leaf.increasing.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Leaves Increasing in Size", x="Day of Year")
dev.off()

#######
#Doing the above but for Flowers or flowering buds
quercus.fbi <- quercus.891[quercus.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.intensity", "flower.buds.observed")]
quercus.fbi <- quercus.fbi[!is.na(quercus.fbi$PlantNumber),]
summary(quercus.fbi)
head(quercus.fbi)

#finding the minimimum and maximum range and mean of the dates Leaf Breaking Bud was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.fbi$Date.Observed)
max(quercus.fbi$Date.Observed)
range(quercus.fbi$Date.Observed)
mean(quercus.fbi$Date.Observed,na.rm=T)

#Now make my Yday
quercus.fbi$yday <- lubridate::yday(quercus.fbi$Date.Observed)
summary(quercus.fbi)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed ripe fruit in the first half of the year
quercus.fbi <- quercus.fbi [quercus.fbi$yday<=180,]
summary(quercus.fbi)


#aggregating quercus.bbi so it shows me the date of first Leaf Breaking Bud for  every plant number and species 
iflower.buds <- aggregate(yday ~ PlantNumber + Species + Year+ flower.buds.intensity, data=quercus.fbi, FUN=min, na.rm=T)
summary(iflower.buds)
head(iflower.buds)

#aggregating the data so it only shows us the average of the first day there were leaf breaking buds per species
#not per individual. So what is the average day per species that first leaf breaking buds appeared
meaniflower.buds <- aggregate(yday ~ Species + Year+ flower.buds.intensity, data=iflower.buds, FUN=mean, na.rm=T)
summary(meaniflower.buds)


##mean Leaf Breaking Bud intensity per year per indiviual 
ggplot(data=iflower.buds) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=flower.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Flowers or Flowering Buds Intensity", x="Day of Year")

##mean Leaf Breaking Bud intensity per year per species 
ggplot(data=meaniflower.buds) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=flower.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Flowers or Flowering buds", x="Day of Year")

ggplot(data=meaniflower.buds) +
  geom_histogram(alpha=1.5, aes(x=yday, fill=flower.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Flowers or Flowering buds", x="Day of Year")

png(file.path(path.figs,"Quercus_Flower_or_Flowering_buds_density.png"), height=4, width=6, units="in", res=320)
ggplot(data=meaniflower.buds) +
  geom_density(alpha=1.5, aes(x=yday, fill=flower.buds.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Flowers or Flowering buds", x="Day of Year")
dev.off()


#######
#Doing the above but for Open Flowers
quercus.foi <- quercus.891[quercus.all$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.intensity", "flower.open.observed")]
quercus.foi <- quercus.foi[!is.na(quercus.foi$PlantNumber),]
summary(quercus.foi)
head(quercus.foi)

#finding the minimimum and maximum range and mean of the dates Leaf Breaking Bud was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.foi$Date.Observed)
max(quercus.foi$Date.Observed)
range(quercus.foi$Date.Observed)
mean(quercus.foi$Date.Observed,na.rm=T)

#Now make my Yday
quercus.foi$yday <- lubridate::yday(quercus.foi$Date.Observed)
summary(quercus.foi)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed ripe fruit in the first half of the year
quercus.foi <- quercus.foi [quercus.foi$yday<=180,]
summary(quercus.foi)


#aggregating quercus.bbi so it shows me the date of first Leaf Breaking Bud for  every plant number and species 
iflower.open <- aggregate(yday ~ PlantNumber + Species + Year+ flower.open.intensity, data=quercus.foi, FUN=min, na.rm=T)
summary(iflower.open)
head(iflower.open)

#aggregating the data so it only shows us the average of the first day there were leaf breaking buds per species
#not per individual. So what is the average day per species that first leaf breaking buds appeared
meaniflower.open <- aggregate(yday ~ Species + Year+ flower.open.intensity, data=iflower.open, FUN=mean, na.rm=T)
summary(meaniflower.open)


##mean Leaf Breaking Bud intensity per year per indiviual 
ggplot(data=iflower.open) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=flower.open.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Open Flowers Intensity", x="Day of Year")

##mean Leaf Breaking Bud intensity per year per species 
ggplot(data=meaniflower.open) +
  geom_boxplot(alpha=1.5, aes(x=yday, fill=flower.open.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Open Flowers", x="Day of Year")

ggplot(data=meaniflower.open) +
  geom_histogram(alpha=1.5, aes(x=yday, fill=flower.open.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Open Flowers", x="Day of Year")

png(file.path(path.figs,"Quercus_Open_Flower_density.png"), height=4, width=6, units="in", res=320)
ggplot(data=meaniflower.open) +
  geom_density(alpha=1.5, aes(x=yday, fill=flower.open.intensity,)) +
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  #scale_color_manual(name="Year", values=c("0%"="red", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Mean Period of Open Flowers", x="Day of Year")
dev.off()
