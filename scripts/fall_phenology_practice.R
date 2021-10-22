library(ggplot2)
#path.figs <- "G://My Drive/LivingCollections_Phenology/Reports/2021_01_MidYear_Report/figures_spring_2021"
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

acer20 <- clean.google(collection="Acer", dat.yr=2020)
acer20$Collection <- as.factor("Acer")
acer20$Year <- lubridate::year(acer20$Date.Observed)
summary(acer20)


acer19 <- clean.google(collection="Acer", dat.yr=2019)
acer19$Collection <- as.factor("Acer")
acer19$Year <- lubridate::year(acer19$Date.Observed)
summary(acer19)

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

#leaving out Ulmus because they were not observed in spring 2021
#ulmus20 <- clean.google(collection="Ulmus", dat.yr=2020)
#ulmus20$Collection <- as.factor("Ulmus")
#ulmus20$Year <- lubridate::year(ulmus20$Date.Observed)
#summary(ulmus20)



dat.all <- rbind(quercus18, quercus19, quercus20, quercus21, acer19, acer20, acer21)
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
summary(dat.all)

###########
###########
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
  scale_fill_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue", "2021"="green")) +
  scale_color_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue", "2021"="green")) +
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
  scale_fill_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue", "2021"="green")) +
  scale_color_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue", "2021"="green")) +
  theme_bw()+
  labs(title="Average Day of First Falling Leaves", x="Day of Year")

######
##getting a picture of intensity
######
#biding only oak data
dat.qu <- rbind(quercus18, quercus19, quercus20, quercus21)
summary(dat.qu)

#setting yday
dat.qu$yday <- lubridate::yday(dat.qu$Date.Observed)
summary(dat.qu)

#subsetting out for fruit intensity
dat.fr <- dat.qu[dat.qu$fruit.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.intensity")]
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
dat.fr <- dat.fr [dat.fr$yday>=180,]
dat.fr <- dat.fr [dat.fr$yday<=Sys.Date(),]
summary(dat.fr)

#removing "0 and NA's
#dat.fr <-dat.fr[(dat.fr$fruit.present.intensity!="NA"),]
#summary(dat.fr)

ggplot(data=dat.fr) +
  geom_histogram(alpha=1.5, binwidth = 15, aes(x=yday, fill=fruit.present.intensity, NA.rm = TRUE))+
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
  #scale_color_manual(name="Year", values=cc("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Fruit Present Intensity", x="Day of Year")


############################
######Looking at leaf color intense across all collections
###########################


#subsetting out for fruit intensity
dat.lci <- dat.all[dat.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.intensity")]
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
dat.lci <- dat.lci [dat.lci$yday>=180,]
dat.lci <- dat.lci [dat.lci$yday<=Sys.Date(),]
summary(dat.lci)

#removing "0 and NA's
#dat.lci <-dat.lci[(dat.lci$leaf.color.intensity!="NA"),]
#summary(dat.lci)

ggplot(data=dat.lci) +
  geom_histogram(alpha=1.5, binwidth = 15, aes(x=yday, fill=leaf.color.intensity, NA.rm = TRUE))+
  facet_grid(~Year)+
  #scale_fill_manual(name="Year", values=c("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
  #scale_color_manual(name="Year", values=cc("101-1,000"="red", "1,001-10,000"="orange", "11-100"="yellow", "3-10"="green", ">10,000"="blue", "0"="NA", "NA"="NA")) +
  theme_bw()+
  theme(axis.text.y=element_blank())+
  labs(title="Fruit Present Intensity", x="Day of Year")
