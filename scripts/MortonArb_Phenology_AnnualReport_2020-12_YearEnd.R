# A new script with some combined collections analyses/graphs
library(ggplot2)
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")

acer20 <- clean.google(collection="Acer", dat.yr=2020)
acer20$Collection <- as.factor("Acer")
acer20$Year <- lubridate::year(acer20$Date.Observed)
summary(acer20)

acer19 <- clean.google(collection="Acer", dat.yr=2019)
acer19$Collection <- as.factor("Acer")
acer19$Year <- lubridate::year(acer19$Date.Observed)
summary(acer19)

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

ulmus20 <- clean.google(collection="Ulmus", dat.yr=2020)
ulmus20$Collection <- as.factor("Ulmus")
ulmus20$Year <- lubridate::year(ulmus20$Date.Observed)
summary(ulmus20)


dat.all <- rbind(quercus20, quercus19, acer20, ulmus20, acer19, quercus18)
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
summary(dat.all)

###########
###########
#Getting a graph of first color observations
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


#only looking at trees that showed fall color in the last half of the year
dat.lf <- dat.lc [dat.lc$yday>=180,]
summary(dat.lf)

#aggregating quercus.lf so it shows me the date of first leaf color for  every plant number and species 
leaf.color <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lf, FUN=min, na.rm=T)
summary(leaf.color)
head(leaf.color)

#Graphing
ggplot(data=leaf.color) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue")) +
  scale_color_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue")) +
  theme_bw()+
  labs(title="Average Day of First Leaf Color", x="Day of Year")

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
  facet_grid(Collection ~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue")) +
  scale_color_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue")) +
  theme_bw()+
  labs(title="Average Day of First Falling Leaves", x="Day of Year")

