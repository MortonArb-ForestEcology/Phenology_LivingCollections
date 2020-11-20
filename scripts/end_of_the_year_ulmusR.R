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

# Downloading 2020 data
ulmus20 <- clean.google(collection="Ulmus", dat.yr=2020)
ulmus20$Collection <- as.factor("Ulmus")
ulmus20$Year <- lubridate::year(ulmus20$Date.Observed)
summary(ulmus20)

# Putting 2020 into the a data frame to make working with it easier
ulmus.all <- rbind(ulmus20)
summary(ulmus.all) # makign sure this worked; check date & year columns to make sure they make sense

head(ulmus.all)

# start with just getting ulmus americana
summary(ulmus$Species=="Ulmus americana")
umam <- ulmus[ulmus$Species=="Ulmus americana",]
summary(umam)

#Checking to make sure it is just Ulmus americana
head(umam)

#creating a data frame of just leaves present observed
umam.lp <- umam[umam$leaf.present.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "leaf.present.observed")]
summary(umam.lp)
dim(umam.lp)

# When pulling one tree, don't forget double = to get T/F
ulmus.solo <- umam.lp[umam.lp$PlantNumber=="1053-28*2",]
summary(ulmus.solo)
ulmus.solo
dim(ulmus.solo)

#finding the minimimum and maximum of the dates leaves pesent was observed on our tree.
min(ulmus.solo$Date.Observed)
max(ulmus.solo$Date.Observed)
range(ulmus.solo$Date.Observed)
usolo.dates <- range(ulmus.solo$Date.Observed)

summary(usolo.dates)

# How to pull multiple trees for the same species
ulmus.duo <- umam.lp[umam.lp$PlantNumber %in% c("2390-26*2", "604-25*1") & umam.lp$Observer=="Reidy",]
summary(ulmus.duo)

#Plotting presence of fruit accoring to date in Ulmus americana
ggplot(data=umam, aes(x=Date.Observed)) +
  geom_histogram(aes(fill=fruit.present.observed), binwidth=7) +
  ggtitle("Ulmus americana fruit present")
dev.off()

##Getting phenophase data for all trees or a select few trees
#maybe this?
ulmus.fr <- subset(ulmus.all [,c("PlantNumber","leaf.color.observed")], simplify = TRUE, drop = TRUE)
summary (ulmus.fr)

#I can also try this
ulmus.lc <- ulmus.all[ulmus.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.observed")]
ulmus.lc <- ulmus.lc[!is.na(ulmus.lc$PlantNumber),]
summary(ulmus.lc)
head(ulmus.lc)


#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(ulmus.lc$Date.Observed)
max(ulmus.lc$Date.Observed)
range(ulmus.lc$Date.Observed)
mean(ulmus.lc$Date.Observed,na.rm=T)

#Now make my Yday
ulmus.lc$yday <- lubridate::yday(ulmus.lc$Date.Observed)
summary(ulmus.lc)

#Also looking at year as well not as important but nice to have
#ulmus.lc$year <- lubridate::year(ulmus.lc$Date.Observed)
#summary(ulmus.lc)

#only looking at trees that showed fall color in the last half of the year
ulmus.lf <- ulmus.lc [ulmus.lc$yday>=180,]
summary(ulmus.lf)


#aggregating quercus.lf so it shows me the date of first leaf color for  every plant number and species 
ufirst.tree <- aggregate(yday ~ PlantNumber + Species + Year, data=ulmus.lf, FUN=min, na.rm=T)
summary(ufirst.tree)
head(ufirst.tree)
# making a box plot of all of the species of oak earliest date of leaf color showing in that ulmus.lf data frame
ggplot(data=ufirst.tree) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and oak as needed
ggplot(data=ufirst.tree[ufirst.tree$Species %in% c("Ulmus americana", "Ulmus parvifolia", "Ulmus changii", "Ulmus serotina", "Ulmus alta"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day leaves showed fall color per species
#not per individual. So what is the average day per species that leaves showed fall color
umeanfirst.tree <- aggregate(yday ~ Species + Year, data=ufirst.tree, FUN=mean, na.rm=T)
summary(umeanfirst.tree)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
umeanfirst.tree[umeanfirst.tree$Species %in% c("Ulmus americana"),]

# messing aroung with some different plots
ggplot(data=umeanfirst.tree) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=umeanfirst.tree[umeanfirst.tree$Species %in% c("Ulmus americana", "Ulmus parvifolia", "Ulmus changii", "Ulmus serotina", "Ulmus alta"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) 

############
#Getting other phenophases--flower buds
ulmus.fb <- ulmus.all[ulmus.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed")]
ulmus.fb <- ulmus.fb[!is.na(ulmus.fb$PlantNumber),]
summary(ulmus.fb)
head(ulmus.fb)


#finding the minimimum and maximum range and mean of the dates flower buds were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(ulmus.fb$Date.Observed)
max(ulmus.fb$Date.Observed)
range(ulmus.fb$Date.Observed)
mean(ulmus.fb$Date.Observed,na.rm=T)

#Now make my Yday
ulmus.fb$yday <- lubridate::yday(ulmus.fb$Date.Observed)
summary(ulmus.fb)

#Ulmus can show flower buds in fall and spring so commenting this out 
#only looking at trees that showed fall color in the last half of the year
#ulmus.lf <- ulmus.lc [ulmus.lc$yday>=180,]
#summary(ulmus.lf)


#aggregating ulmus.fb so it shows me the date of first flowerbud for  every plant number and species 
ufirst.bud <- aggregate(yday ~ PlantNumber + Species + Year, data=ulmus.fb, FUN=min, na.rm=T)
summary(ufirst.bud)
head(ufirst.bud)
# making a box plot of all of the species of elm earliest date of buds in that ulmus.fb data frame
ggplot(data=ufirst.bud) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and out as needed
ggplot(data=ufirst.tree[ufirst.bud$Species %in% c("Ulmus americana", "Ulmus parvifolia", "Ulmus changii", "Ulmus serotina", "Ulmus alta"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day buds appeared per species
#not per individual. So what is the average day per species that flower buds appeared
umeanfirst.bud <- aggregate(yday ~ Species + Year, data=ufirst.bud, FUN=mean, na.rm=T)
summary(umeanfirst.bud)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
umeanfirst.bud[umeanfirst.bud$Species %in% c("Ulmus americana"),]


# messing aroung with some different plots
ggplot(data=umeanfirst.bud) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=umeanfirst.bud[umeanfirst.bud$Species %in% c("Ulmus americana", "Ulmus parvifolia", "Ulmus changii", "Ulmus serotina", "Ulmus alta"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) 

############
#Getting other phenophases--falling leaves
ulmus.fl <- ulmus.all[ulmus.all$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed")]
ulmus.fl <- ulmus.fl[!is.na(ulmus.fl$PlantNumber),]
summary(ulmus.fl)
head(ulmus.fl)


#finding the minimimum and maximum range and mean of the dates leaves falling were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(ulmus.fl$Date.Observed)
max(ulmus.fl$Date.Observed)
range(ulmus.fl$Date.Observed)
mean(ulmus.fl$Date.Observed,na.rm=T)

#Now make my Yday
ulmus.fl$yday <- lubridate::yday(ulmus.fl$Date.Observed)
summary(ulmus.fl)

#only looking at trees that showed leaves falling in the last half of the year
ulmus.fl <- ulmus.fl [ulmus.fl$yday>=180,]
summary(ulmus.fl)


#aggregating quercus.lf so it shows me the date of first falling leaves for  every plant number and species 
uleaves.fall <- aggregate(yday ~ PlantNumber + Species + Year, data=ulmus.fl, FUN=min, na.rm=T)
summary(uleaves.fall)
head(uleaves.fall)
# making a box plot of all of the species of elm earliest date of falling leaves in that ulmus.fb data frame
ggplot(data=uleaves.fall) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year)))

#Generating the same box plot but only for a few select species, species can be swapped in and out as needed
ggplot(data=uleaves.fall[uleaves.fall$Species %in% c("Ulmus americana", "Ulmus parvifolia", "Ulmus changii", "Ulmus serotina", "Ulmus alta"),]) +
  #the facet grid can be changedd to compare year~year(year to year) 
  facet_grid(Year~.) +
  geom_boxplot(aes(x=Species, y=yday, fill=as.factor(Year)))


#aggregating the data so it only shows us the average of the first day falling leaves per species
#not per individual. So what is the average day per species that flower buds appeared
uleaves.fall <- aggregate(yday ~ Species + Year, data=uleaves.fall, FUN=mean, na.rm=T)
summary(uleaves.fall)

#Doing the same thing as above but at a species level the %in% part makes it take the specific thing in the data frame
uleaves.fall[uleaves.fall$Species %in% c("Ulmus americana"),]


# messing aroung with some different plots
ggplot(data=uleaves.fall) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) +
  theme(axis.text.x=element_text(size = 7, angle = 45, hjust = 1))



ggplot(data=uleaves.fall[uleaves.fall$Species %in% c("Ulmus americana", "Ulmus parvifolia", "Ulmus changii", "Ulmus serotina", "Ulmus alta"),]) +
  geom_point(aes(x=Species, y=yday, fill=as.factor(Year), color=as.factor(Year))) 



