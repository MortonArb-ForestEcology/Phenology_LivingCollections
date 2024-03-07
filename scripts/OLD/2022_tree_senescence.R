# A new script with to attempt to get 50% senescence metric for leaf color in 2022
# -----------------------------------
library(ggplot2)
#------------------------------------
source("clean_google_form.R")
# Reading phenology data for 2022
acer22 <- clean.google(collection="Acer", dat.yr=2022)
acer22$Collection <- as.factor("Acer")
acer22$Year <- lubridate::year(acer22$Date.Observed)
summary(acer22)

quercus22 <- clean.google(collection="Quercus", dat.yr=2022)
quercus22$Collection <- as.factor("Quercus")
quercus22$Year <- lubridate::year(quercus22$Date.Observed)
summary(quercus22)

ulmus22 <- clean.google(collection="Ulmus", dat.yr=2022)
ulmus22$Collection <- as.factor("Ulmus")
ulmus22$Year <- lubridate::year(ulmus22$Date.Observed)
summary(ulmus22)

tilia22 <- clean.google(collection="Tilia", dat.yr=2022)
tilia22$Collection <- as.factor("Tilia")
tilia22$Year <- lubridate::year(tilia22$Date.Observed)
summary(tilia22)

###binding into a df 
dat.22 <- rbind(quercus22,acer22, ulmus22, tilia22)
#yday, but it can be changed if needed 
dat.22$yday <- lubridate::yday(dat.22$Date.Observed)
summary(dat.22)


#only looking at trees that showed fall color from 8/1 on --> 8/1 is day 213, but I'm going to soft-code it
dat.22 <- dat.22 [dat.22$yday>=lubridate::yday("2022-08-01"),]
summary(dat.22)

#Separating out the columns I want. I might be able to skip this step, however,I wanted to make sure I was only grabbing trees that recorded "Yes" for leaves.
## CR: I *think* this is okay, but if a tree were to be like ginkgo and just drop ALL of it's leaves at once without showing color or having that recorded, it'd give us a bad reading.
dat.se <- dat.22[dat.22$leaf.present.observed=="Yes", ]
summary(dat.se)

##Creating a df containing only leaves that expressed the critera of senescence 
## CR: Note that here you're actulaly not referencing the object you created in line 40.  Not a problem (see my note), but just something to be aware of
# checking our categories 
unique(dat.se$leaf.present.intensity);unique(dat.se$leaf.color.intensity) 

dat.22o <- dat.se[(!is.na(dat.se$leaf.present.intensity) & dat.se$leaf.present.intensity %in% c("0%", "<5%","5-24%", "25-49%"))| (!is.na(dat.se$leaf.color.intensity) & dat.se$leaf.color.intensity %in% c(">95%", "75-94%", "50-74%")),]
head(dat.22o)
summary(dat.22o)


#aggregating out NA'S and superfluous columns 
leaf.sen <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.22o, FUN=min, na.rm=T)
summary(leaf.sen)
head(leaf.sen)
dim(leaf.sen) # 501 trees!

# Creating a folder for where we want to save everything for this
path.out <- "~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations/Collaboration-Primack_FallSenescence"
dir.create(path.out, recursive = T)

#writing .csv commented out for now
write.csv(leaf.sen, file.path(path.out, "Tree_senescence_TMA_2022.CSV"), row.names=F)

dates.breaks <- c("2022-08-15", "2022-09-01", "2022-09-15", "2022-10-01", "2022-10-15", "2022-11-01", "2022-11-15", "2022-12-01")
yday.breaks <- lubridate::yday(dates.breaks)
breaks.labs <- c("Aug 15", "Sept 1", "Sept 15", "Oct 1", "Oct 15", "Nov 1", "Nov 15", "Dec 1")

#png(file.path(path.out, "Senescence_2022_Individuals_byCollection.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen) +
  facet_grid(Collection~.,scales="free_y" ) + 
  geom_point(alpha=0.5, aes(x=yday, y=PlantNumber, fill=as.factor(yday), color=as.factor(yday))) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme(axis.title.y=element_blank(),
       axis.text.y=element_blank(),
       axis.ticks.y=element_blank(),
       panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs(title="Points", x="Day of Year")
dev.off()


#png(file.path(path.out, "Senescence_2022_byCollection-Histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen) +
  facet_grid(Collection~.,scales="free_y") + 
  geom_histogram(aes(x=yday, fill=Species), binwidth=7) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs( x="Day of Year")
dev.off()


# Checking out our outliers
leaf.sen[leaf.sen$yday<=227,]


# Doing a spot check on a couple of our outliers and it looks like there are some issues with random "no" observations early --> go back and go with your filter of saying it has to have leaves.  If we wanted to get fancy we can remove things like that that are just a clear 1-off, but that'd be hard
dat.22[dat.22$PlantNumber=="3-2008*1",c("Timestamp", "Observer", "Date.Observed", "Species", "PlantNumber", "leaf.present.observed", "leaf.present.intensity", "leaf.color.observed", "leaf.color.intensity")]

# Thsi is a weird example too becuase on 9/26 there's a Q. variabilis with the accession number that's otherwise a quercus deamii --> we need to figure that out!  This no logner appears when we filter out the "nos" but it's weird

# Checking our data for PlantNumbers with multiple species attached 
check.plants <- aggregate(yday ~ PlantNumber, data=leaf.sen, FUN=length)
check.plants[check.plants$yday>1,]

# 2 numbers with multiple species listed: 437-2004*2, 476-42*1
head(dat.22[dat.22$PlantNumber=="437-2004*3",]) # Merskey/Houston
head(dat.22[dat.22$PlantNumber=="476-42*1",]) # Teesdale/Wagner

# # NOTE: probably worth doing this earlier on to make sure we don't have this more broadly.  Running this check on anything that is already aggregated to 1 value per PlantNumber per species will reveal it
dat.check <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.22, FUN=median, na.rm=T)
summary(dat.check)
plant.check2 <- aggregate(yday ~ PlantNumber, data=dat.check, FUN=length)
plant.check2[plant.check2$yday>1,]

######################
####Doing the same for subsequent years
######################
#####2021
acer21 <- clean.google(collection="Acer", dat.yr=2021)
acer21$Collection <- as.factor("Acer")
acer21$Year <- lubridate::year(acer21$Date.Observed)
summary(acer21)

quercus21 <- clean.google(collection="Quercus", dat.yr=2021)
quercus21$Collection <- as.factor("Quercus")
quercus21$Year <- lubridate::year(quercus21$Date.Observed)
summary(quercus21)

ulmus21 <- clean.google(collection="Ulmus", dat.yr=2021)
ulmus21$Collection <- as.factor("Ulmus")
ulmus21$Year <- lubridate::year(ulmus21$Date.Observed)
summary(ulmus21)

dat.21 <- rbind(quercus21,acer21, ulmus21)
#yday, but it can be changed if needed 
dat.21$yday <- lubridate::yday(dat.21$Date.Observed)
summary(dat.21)


#only looking at trees that showed fall color from 8/1 on --> 8/1 is day 213, but I'm going to soft-code it
dat.21 <- dat.21 [dat.21$yday>=lubridate::yday("2021-08-01"),]
summary(dat.21)

#Separating out the columns I want. I might be able to skip this step, however,I wanted to make sure I was only grabbing trees that recorded "Yes" for leaves.
## CR: I *think* this is okay, but if a tree were to be like ginkgo and just drop ALL of it's leaves at once without showing color or having that recorded, it'd give us a bad reading.
dat.se21 <- dat.21[dat.21$leaf.present.observed=="Yes", ]
summary(dat.se21)

##Creating a df containing only leaves that expressed the critera of senescence 
## CR: Note that here you're actulaly not referencing the object you created in line 40.  Not a problem (see my note), but just something to be aware of
# checking our categories 
unique(dat.se21$leaf.present.intensity);unique(dat.se21$leaf.color.intensity) 

dat.21o <- dat.se21[(!is.na(dat.se21$leaf.present.intensity) & dat.se21$leaf.present.intensity %in% c("0%", "<5%","5-24%", "25-49%"))| (!is.na(dat.se21$leaf.color.intensity) & dat.se21$leaf.color.intensity %in% c(">95%", "75-94%", "50-74%")),]
head(dat.21o)
summary(dat.21o)


#aggregating out NA'S and superfluous columns 
leaf.sen21 <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.21o, FUN=min, na.rm=T)
summary(leaf.sen21)
head(leaf.sen21)
dim(leaf.sen21) # 420 trees!

# Creating a folder for where we want to save everything for this
path.out <- "~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations/Collaboration-Primack_FallSenescence"
#dir.create(path.out, recursive = T)

#writing .csv commented out for now
write.csv(leaf.sen21, file.path(path.out, "Tree_senescence_TMA_2021.CSV"), row.names=F)

dates.breaks <- c("2021-08-15", "2021-09-01", "2021-09-15", "2021-10-01", "2021-10-15", "2021-11-01", "2021-11-15", "2021-12-01")
yday.breaks <- lubridate::yday(dates.breaks)
breaks.labs <- c("Aug 15", "Sept 1", "Sept 15", "Oct 1", "Oct 15", "Nov 1", "Nov 15", "Dec 1")

png(file.path(path.out, "Senescence_2021_Individuals_byCollection.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen21) +
  facet_grid(Collection~.,scales="free_y" ) + 
  geom_point(alpha=0.5, aes(x=yday, y=PlantNumber, fill=as.factor(yday), color=as.factor(yday))) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs(title="Points", x="Day of Year")
dev.off()


png(file.path(path.out, "Senescence_2021_byCollection-Histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen21) +
  facet_grid(Collection~.,scales="free_y") + 
  geom_histogram(aes(x=yday, fill=Species), binwidth=7) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs( x="Day of Year")
dev.off()


# Checking out our outliers
leaf.sen21[leaf.sen$yday<=227,]


# Doing a spot check on a couple of our outliers and it looks like there are some issues with random "no" observations early --> go back and go with your filter of saying it has to have leaves.  If we wanted to get fancy we can remove things like that that are just a clear 1-off, but that'd be hard
dat.21[dat.21$PlantNumber=="15-2008*1",c("Timestamp", "Observer", "Date.Observed", "Species", "PlantNumber", "leaf.present.observed", "leaf.present.intensity", "leaf.color.observed", "leaf.color.intensity")]

# U.gaussenni is weird

# Checking our data for PlantNumbers with multiple species attached 
check.plants <- aggregate(yday ~ PlantNumber, data=leaf.sen, FUN=length)
check.plants[check.plants$yday>1,]

# 2 numbers with multiple species listed: 437-2004*2, 476-42*1
head(dat.21[dat.21$PlantNumber=="15-2008*1",]) # Zwemke/Johnson

# # NOTE: probably worth doing this earlier on to make sure we don't have this more broadly.  Running this check on anything that is already aggregated to 1 value per PlantNumber per species will reveal it
dat.check <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.21, FUN=median, na.rm=T)
summary(dat.check)
plant.check2 <- aggregate(yday ~ PlantNumber, data=dat.check, FUN=length)
plant.check2[plant.check2$yday>1,]




###### 2020 #####
#######################
acer20 <- clean.google(collection="Acer", dat.yr=2020)
acer20$Collection <- as.factor("Acer")
acer20$Year <- lubridate::year(acer20$Date.Observed)
summary(acer20)

quercus20 <- clean.google(collection="Quercus", dat.yr=2020)
quercus20$Collection <- as.factor("Quercus")
quercus20$Year <- lubridate::year(quercus20$Date.Observed)
summary(quercus20)

ulmus20 <- clean.google(collection="Ulmus", dat.yr=2020)
ulmus20$Collection <- as.factor("Ulmus")
ulmus20$Year <- lubridate::year(ulmus20$Date.Observed)
summary(ulmus20)

dat.20 <- rbind(quercus20,acer20, ulmus20)
#yday, but it can be changed if needed 
dat.20$yday <- lubridate::yday(dat.20$Date.Observed)
summary(dat.20)


#only looking at trees that showed fall color from 8/1 on --> 8/1 is day 203, but I'm going to soft-code it
dat.20 <- dat.20 [dat.20$yday>=lubridate::yday("2020-08-01"),]
summary(dat.20)

#Separating out the columns I want. I might be able to skip this step, however,I wanted to make sure I was only grabbing trees that recorded "Yes" for leaves.
## CR: I *think* this is okay, but if a tree were to be like ginkgo and just drop ALL of it's leaves at once without showing color or having that recorded, it'd give us a bad reading.
dat.se20 <- dat.20[dat.20$leaf.present.observed=="Yes", ]
summary(dat.se20)

##Creating a df containing only leaves that expressed the critera of senescence 
## CR: Note that here you're actulaly not referencing the object you created in line 40.  Not a problem (see my note), but just something to be aware of
# checking our categories 
unique(dat.se20$leaf.present.intensity);unique(dat.se20$leaf.color.intensity) 

dat.20o <- dat.se20[(!is.na(dat.se20$leaf.present.intensity) & dat.se20$leaf.present.intensity %in% c("0%", "<5%","5-24%", "25-49%"))| (!is.na(dat.se20$leaf.color.intensity) & dat.se20$leaf.color.intensity %in% c(">95%", "75-94%", "50-74%")),]
head(dat.20o)
summary(dat.20o)


#aggregating out NA'S and superfluous columns 
leaf.sen20 <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.20o, FUN=min, na.rm=T)
summary(leaf.sen20)
head(leaf.sen20)
dim(leaf.sen20) # 406 trees!

#writing .csv commented out for now
write.csv(leaf.sen20, file.path(path.out, "Tree_senescence_TMA_2020.CSV"), row.names=F)

dates.breaks <- c("2020-08-15", "2020-09-01", "2020-09-15", "2020-10-01", "2020-10-15", "2020-11-01", "2020-11-15", "2020-12-01")
yday.breaks <- lubridate::yday(dates.breaks)
breaks.labs <- c("Aug 15", "Sept 1", "Sept 15", "Oct 1", "Oct 15", "Nov 1", "Nov 15", "Dec 1")

png(file.path(path.out, "Senescence_2020_Individuals_byCollection.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen20) +
  facet_grid(Collection~.,scales="free_y" ) + 
  geom_point(alpha=0.5, aes(x=yday, y=PlantNumber, fill=as.factor(yday), color=as.factor(yday))) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs(title="Points", x="Day of Year")
dev.off()


png(file.path(path.out, "Senescence_2020_byCollection-Histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen20) +
  facet_grid(Collection~.,scales="free_y") + 
  geom_histogram(aes(x=yday, fill=Species), binwidth=7) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs( x="Day of Year")
dev.off()


# Checking out our outliers
leaf.sen20[leaf.sen$yday<=227,]


# Doing a spot check on a couple of our outliers and it looks like there are some issues with random "no" observations early --> go back and go with your filter of saying it has to have leaves.  If we wanted to get fancy we can remove things like that that are just a clear 1-off, but that'd be hard
#dat.20[dat.20$PlantNumber=="15-2008*1",c("Timestamp", "Observer", "Date.Observed", "Species", "PlantNumber", "leaf.present.observed", "leaf.present.intensity", "leaf.color.observed", "leaf.color.intensity")]

# Checking our data for PlantNumbers with multiple species attached 
check.plants <- aggregate(yday ~ PlantNumber, data=leaf.sen20, FUN=length)
check.plants[check.plants$yday>1,]

# 2 numbers with multiple species listed: 437-2004*2, 476-42*1
#head(dat.21[dat.21$PlantNumber=="15-2008*1",]) # Zwemke/Johnson

# # NOTE: probably worth doing this earlier on to make sure we don't have this more broadly.  Running this check on anything that is already aggregated to 1 value per PlantNumber per species will reveal it
dat.check <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.21, FUN=median, na.rm=T)
summary(dat.check)
plant.check2 <- aggregate(yday ~ PlantNumber, data=dat.check, FUN=length)
plant.check2[plant.check2$yday>1,]

#### 2019 ###
acer19 <- clean.google(collection="Acer", dat.yr=2019)
acer19$Collection <- as.factor("Acer")
acer19$Year <- lubridate::year(acer19$Date.Observed)
summary(acer19)

quercus19 <- clean.google(collection="Quercus", dat.yr=2019)
quercus19$Collection <- as.factor("Quercus")
quercus19$Year <- lubridate::year(quercus19$Date.Observed)
summary(quercus19)


dat.19 <- rbind(quercus19,acer19)
#yday, but it can be changed if needed 
dat.19$yday <- lubridate::yday(dat.19$Date.Observed)
summary(dat.19)


#only looking at trees that showed fall color from 8/1 on --> 8/1 is day 213, but I'm going to soft-code it
dat.19 <- dat.19 [dat.19$yday>=lubridate::yday("2019-08-01"),]
summary(dat.19)

#Separating out the columns I want. I might be able to skip this step, however,I wanted to make sure I was only grabbing trees that recorded "Yes" for leaves.
## CR: I *think* this is okay, but if a tree were to be like ginkgo and just drop ALL of it's leaves at once without showing color or having that recorded, it'd give us a bad reading.
dat.se19 <- dat.19[dat.19$leaf.present.observed=="Yes", ]
summary(dat.se19)

##Creating a df containing only leaves that expressed the critera of senescence 
## CR: Note that here you're actulaly not referencing the object you created in line 40.  Not a problem (see my note), but just something to be aware of
# checking our categories 
unique(dat.se19$leaf.present.intensity);unique(dat.se19$leaf.color.intensity) 

dat.19o <- dat.se19[(!is.na(dat.se19$leaf.present.intensity) & dat.se19$leaf.present.intensity %in% c("0%", "<5%","5-24%", "25-49%"))| (!is.na(dat.se19$leaf.color.intensity) & dat.se19$leaf.color.intensity %in% c(">95%", "75-94%", "50-74%")),]
head(dat.19o)
summary(dat.19o)


#aggregating out NA'S and superfluous columns 
leaf.sen19 <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.19o, FUN=min, na.rm=T)
summary(leaf.sen19)
head(leaf.sen19)
dim(leaf.sen19) # 324 trees!

#writing .csv commented out for now
write.csv(leaf.sen19, file.path(path.out, "Tree_senescence_TMA_2019.CSV"), row.names=F)

dates.breaks <- c("2019-08-15", "2019-09-01", "2019-09-15", "2019-10-01", "2019-10-15", "2019-11-01", "2019-11-15", "2019-12-01")
yday.breaks <- lubridate::yday(dates.breaks)
breaks.labs <- c("Aug 15", "Sept 1", "Sept 15", "Oct 1", "Oct 15", "Nov 1", "Nov 15", "Dec 1")

png(file.path(path.out, "Senescence_2019_Individuals_byCollection.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen19) +
  facet_grid(Collection~.,scales="free_y" ) + 
  geom_point(alpha=0.5, aes(x=yday, y=PlantNumber, fill=as.factor(yday), color=as.factor(yday))) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs(title="Points", x="Day of Year")
dev.off()


png(file.path(path.out, "Senescence_2019_byCollection-Histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen19) +
  facet_grid(Collection~.,scales="free_y") + 
  geom_histogram(aes(x=yday, fill=Species), binwidth=7) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs( x="Day of Year")
dev.off()


# Checking out our outliers
leaf.sen19[leaf.sen$yday<=227,]


# Doing a spot check on a couple of our outliers and it looks like there are some issues with random "no" observations early --> go back and go with your filter of saying it has to have leaves.  If we wanted to get fancy we can remove things like that that are just a clear 1-off, but that'd be hard
#dat.21[dat.21$PlantNumber=="15-2008*1",c("Timestamp", "Observer", "Date.Observed", "Species", "PlantNumber", "leaf.present.observed", "leaf.present.intensity", "leaf.color.observed", "leaf.color.intensity")]

# Checking our data for PlantNumbers with multiple species attached 
check.plants <- aggregate(yday ~ PlantNumber, data=leaf.sen, FUN=length)
check.plants[check.plants$yday>1,]

# 2 numbers with multiple species listed: 437-2004*2, 476-42*1
#head(dat.21[dat.21$PlantNumber=="15-2008*1",]) # Zwemke/Johnson

# # NOTE: probably worth doing this earlier on to make sure we don't have this more broadly.  Running this check on anything that is already aggregated to 1 value per PlantNumber per species will reveal it
dat.check <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.19, FUN=median, na.rm=T)
summary(dat.check)
plant.check2 <- aggregate(yday ~ PlantNumber, data=dat.check, FUN=length)
plant.check2[plant.check2$yday>1,]

##### 2018
quercus18 <- clean.google(collection="Quercus", dat.yr=2018)
quercus18$Collection <- as.factor("Quercus")
quercus18$Year <- lubridate::year(quercus18$Date.Observed)
summary(quercus18)


dat.18 <- rbind(quercus18)
#yday, but it can be changed if needed 
dat.18$yday <- lubridate::yday(dat.18$Date.Observed)
summary(dat.18)


#only looking at trees that showed fall color from 8/1 on --> 8/1 is day 213, but I'm going to soft-code it
dat.18 <- dat.18 [dat.18$yday>=lubridate::yday("2018-08-01"),]
summary(dat.18)

#Separating out the columns I want. I might be able to skip this step, however,I wanted to make sure I was only grabbing trees that recorded "Yes" for leaves.
## CR: I *think* this is okay, but if a tree were to be like ginkgo and just drop ALL of it's leaves at once without showing color or having that recorded, it'd give us a bad reading.
dat.se18 <- dat.18[dat.18$leaf.present.observed=="Yes", ]
summary(dat.se18)

##Creating a df containing only leaves that expressed the critera of senescence 
## CR: Note that here you're actulaly not referencing the object you created in line 40.  Not a problem (see my note), but just something to be aware of
# checking our categories 
unique(dat.se18$leaf.present.intensity);unique(dat.se18$leaf.color.intensity) 

dat.18o <- dat.se18[(!is.na(dat.se18$leaf.present.intensity) & dat.se18$leaf.present.intensity %in% c("0%", "<5%","5-24%", "25-49%"))| (!is.na(dat.se18$leaf.color.intensity) & dat.se18$leaf.color.intensity %in% c(">95%", "75-94%", "50-74%")),]
head(dat.18o)
summary(dat.18o)


#aggregating out NA'S and superfluous columns 
leaf.sen18 <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.18o, FUN=min, na.rm=T)
summary(leaf.sen18)
head(leaf.sen18)
dim(leaf.sen18) # 324 trees!

#writing .csv commented out for now
write.csv(leaf.sen18, file.path(path.out, "Tree_senescence_TMA_2018.CSV"), row.names=F)

dates.breaks <- c("2018-08-15", "2018-09-01", "2018-09-15", "2018-10-01", "2018-10-15", "2018-11-01", "2018-11-15", "2018-12-01")
yday.breaks <- lubridate::yday(dates.breaks)
breaks.labs <- c("Aug 15", "Sept 1", "Sept 15", "Oct 1", "Oct 15", "Nov 1", "Nov 15", "Dec 1")

png(file.path(path.out, "Senescence_2018_Individuals_byCollection.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen18) +
  facet_grid(Collection~.,scales="free_y" ) + 
  geom_point(alpha=0.5, aes(x=yday, y=PlantNumber, fill=as.factor(yday), color=as.factor(yday))) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs(title="Points", x="Day of Year")
dev.off()


png(file.path(path.out, "Senescence_2018_byCollection-Histogram.png"), height=8, width=8, units="in", res=220)
ggplot(data=leaf.sen18) +
  facet_grid(Collection~.,scales="free_y") + 
  geom_histogram(aes(x=yday, fill=Species), binwidth=7) +
  scale_x_continuous(breaks=yday.breaks, labels=breaks.labs) +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid=element_blank())+
  theme(legend.position = "none")+
  labs( x="Day of Year")
dev.off()


# Checking out our outliers
leaf.sen18[leaf.sen18$yday<=227,]


# Doing a spot check on a couple of our outliers and it looks like there are some issues with random "no" observations early --> go back and go with your filter of saying it has to have leaves.  If we wanted to get fancy we can remove things like that that are just a clear 1-off, but that'd be hard
#dat.21[dat.21$PlantNumber=="15-2008*1",c("Timestamp", "Observer", "Date.Observed", "Species", "PlantNumber", "leaf.present.observed", "leaf.present.intensity", "leaf.color.observed", "leaf.color.intensity")]

# Checking our data for PlantNumbers with multiple species attached 
check.plants <- aggregate(yday ~ PlantNumber, data=leaf.sen18, FUN=length)
check.plants[check.plants$yday>1,]

# 2 numbers with multiple species listed: 437-2004*2, 476-42*1
#head(dat.21[dat.21$PlantNumber=="15-2008*1",]) # Zwemke/Johnson

# # NOTE: probably worth doing this earlier on to make sure we don't have this more broadly.  Running this check on anything that is already aggregated to 1 value per PlantNumber per species will reveal it
dat.check <- aggregate(yday ~ PlantNumber + Species + Collection, data=dat.18, FUN=median, na.rm=T)
summary(dat.check)
plant.check2 <- aggregate(yday ~ PlantNumber, data=dat.check, FUN=length)
plant.check2[plant.check2$yday>1,]
