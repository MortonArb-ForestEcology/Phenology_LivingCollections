# A new script with some combined collections analyses/graphs for the end of the year report
library(ggplot2)
path.figs <- "G://My Drive/LivingCollections_Phenology/Reports/2021_01_MidYear_Report/figures_spring_2021"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")
#year 2021
acer22 <- clean.google(collection="Acer", dat.yr=2022)
acer22$Collection <- as.factor("Acer")
acer22$Year <- lubridate::year(acer22$Date.Observed)
summary(acer22)

acer21 <- clean.google(collection="Acer", dat.yr=2021)
acer21$Collection <- as.factor("Acer")
acer21$Year <- lubridate::year(acer21$Date.Observed)
summary(acer21)

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

quercus19 <- clean.google(collection="Quercus", dat.yr=2019)
quercus19$Collection <- as.factor("Quercus")
quercus19$Year <- lubridate::year(quercus19$Date.Observed)
summary(quercus19)

quercus18 <- clean.google(collection="Quercus", dat.yr=2018)
quercus18$Collection <- as.factor("Quercus")
quercus18$Year <- lubridate::year(quercus18$Date.Observed)
summary(quercus18)

#leaving ulmus out because no spring data was colleted in 2020 or 2021

dat.all <- rbind(quercus18, quercus19, quercus21, quercus22, acer19, acer21, acer22)
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
summary(dat.all)

###########
###########
#Getting a graph of breaking leaf bud observations
###########
###########
dat.lb <- dat.all[dat.all$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.breaking.buds.observed", "Collection")]
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

dat.lb <- dat.lb [dat.lb$yday>=62,]
summary(dat.lb)

#aggregating quercus.lf so it shows me the date of first breaking leaf buds for  every plant number and species 
breaking.buds <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lb, FUN=min, na.rm=T)
summary(breaking.buds)
head(breaking.buds)

#Graphing
#png(file.path(path.figs,"All_Leaf_Breaking_Buds.png"), height=4, width=6, units="in", res=320)
ggplot(data=breaking.buds) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  theme_bw()+
  labs(title="Average Day of First Breaking Lead Buds", x="Day of Year")
dev.off()
###########
###########
#Getting a graph of leaves present observations
###########
###########
dat.lp <- dat.all[dat.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed", "Collection")]
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
dat.lp <- dat.lp [dat.lp$yday<=150,]
dat.lp <- dat.lp [dat.lp$yday>=62,]
summary(dat.lp)

#aggregating quercus.lf so it shows me the date of first leaf present for  every plant number and species 
leaves.present <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lp, FUN=min, na.rm=T)
summary(leaves.present)
head(leaves.present)

#Graphing
#png(file.path(path.figs,"All_Leaf_Present.png"), height=4, width=6, units="in", res=320)
ggplot(data=leaves.present) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  theme_bw()+
  labs(title="Average Day of Leaves Present", x="Day of Year")
dev.off()

###########
###########
#Getting a graph of leaves increasing in size observations
###########
###########
dat.li <- dat.all[dat.all$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.increasing.observed", "Collection")]
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
dat.li <- dat.li [dat.li$yday<=150,]
dat.li <- dat.li [dat.li$yday>=62,]
summary(dat.li)

#aggregating quercus.lf so it shows me the date of first leaf increasing in size for  every plant number and species 
leaves.increasing <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.li, FUN=min, na.rm=T)
summary(leaves.increasing)
head(leaves.increasing)

#Graphing
#png(file.path(path.figs,"All_Leaf_Increasing.png"), height=4, width=6, units="in", res=320)
ggplot(data=leaves.increasing) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  theme_bw()+
  labs(title="Average Day of Leaves Increasing in Size Observed", x="Day of Year")
dev.off()

###########
###########
#Getting a graph of flower buds observations
##########
###########
dat.fb <- dat.all[dat.all$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed", "Collection")]
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
dat.fb <- dat.fb [dat.fb$yday<=150,]
dat.fb <- dat.fb [dat.fb$yday>=62,]

summary(dat.fb)

#aggregating quercus.lf so it shows me the date of first flower buds for  every plant number and species 
flower.buds <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fb, FUN=min, na.rm=T)
summary(flower.buds)
head(flower.buds)

#Graphing
#png(file.path(path.figs,"All_Flowers_or_Flower_Buds.png"), height=4, width=6, units="in", res=320)
ggplot(data=flower.buds) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  theme_bw()+
  labs(title="Average Day of Flower Buds or Flowers Observed", x="Day of Year")
dev.off()

###########
###########
#Getting a graph of open flowers observations
###########
###########
dat.fo <- dat.all[dat.all$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed", "Collection")]
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
dat.fo <- dat.fo [dat.fo$yday<=150,]
dat.fo <- dat.fo [dat.fo$yday>=62,]
summary(dat.fo)

#aggregating quercus.lf so it shows me the date of open flowers for  every plant number and species 
flower.open <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fo, FUN=min, na.rm=T)
summary(flower.open)
head(flower.open)

#Graphing
#png(file.path(path.figs,"All_Flowers_Open.png"), height=4, width=6, units="in", res=320)
ggplot(data=flower.open) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  theme_bw()+
  labs(title="Average Day of Open Flower Observed", x="Day of Year")
dev.off()

###########
###########
#Getting a graph of pollen observations
###########
###########
dat.fp <- dat.all[dat.all$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.pollen.observed", "Collection")]
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
dat.fp <- dat.fp [dat.fp$yday<=150,]
dat.fp <- dat.fp [dat.fp$yday>=62,]
summary(dat.fp)

#aggregating quercus.lf so it shows me the date of first pollen for  every plant number and species 
flower.pollen <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fp, FUN=min, na.rm=T)
summary(flower.pollen)
head(flower.pollen)

#Graphing
#png(file.path(path.figs,"All_Flowers_Pollen.png"), height=4, width=6, units="in", res=320)
ggplot(data=flower.pollen) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  scale_color_manual(name="Year", values=c("2018"="maroon4", "2019"="#009E73", "2021"="gray", "2022"="#0072B2")) +
  theme_bw()+
  labs(title="Average Day of Flower Pollen Observed", x="Day of Year")
dev.off()

