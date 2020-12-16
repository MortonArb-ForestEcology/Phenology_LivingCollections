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

if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")



# Downloading 2020 data
quercus20 <- clean.google(collection="Quercus", dat.yr=2020)
quercus20$Collection <- as.factor("Quercus")
quercus20$Year <- lubridate::year(quercus20$Date.Observed)
summary(quercus20)

# Putting 2020 into the a data frame to make working with it easier
quercus <- rbind(quercus20)
summary(quercus) # makign sure this worked; check date & year columns to make sure they make sense

head(quercus)

# start with just getting QUMA
summary(quercus$Species=="Quercus macrocarpa")
quma <- quercus[quercus$Species=="Quercus macrocarpa",]
summary(quma)

#Checking to make sure it is just Quercus Macrocarpa
head(quma)

#creating a data frame of just leaves present observed
quma.lp <- quma[quma$leaf.present.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "leaf.present.observed")]
summary(quma.lp)
dim(quma.lp)

# When pulling one tree, don't forget double = to get T/F
quercus.solo <- quma.lp[quma.lp$PlantNumber=="2390-26*2",]
summary(quercus.solo)
quercus.solo
dim(quercus.solo)

#finding the minimimum and maximum of the dates leaves pesent was observed on our tree.
min(quercus.solo$Date.Observed)
max(quercus.solo$Date.Observed)
range(quercus.solo$Date.Observed)
solo.dates <- range(quercus.solo$Date.Observed)

summary(solo.dates)

# How to pull multiple trees for the same species
# quercus.duo <- quma.lp[quma.lp$PlantNumber=="2390-26*2" | quma.lp$PlantNumber=="132-2015*1",]
quercus.duo <- quma.lp[quma.lp$PlantNumber %in% c("2390-26*2", "132-2015*1") & quma.lp$Observer=="Reidy",]
summary(quercus.duo)

#Plotting presence of fruit accoring to date in Quercus Marcocarpa
ggplot(data=quma, aes(x=Date.Observed)) +
  geom_histogram(aes(fill=fruit.present.observed), binwidth=7) +
  ggtitle("Quercus Macrocarpa fruit present")
dev.off()


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

# Putting 2020, 2019 and 2018 into the same data frame to make working with it easier (in the long run; might be hard at first)
quercus.all <- rbind(quercus18, quercus19, quercus)
summary(quercus.all)


#maybe this?
quercus.fr <- subset(quercus.all [,c("PlantNumber","leaf.color.observed")], simplify = TRUE, drop = TRUE)
summary (quercus.fr)

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
