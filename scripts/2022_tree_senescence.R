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
################################

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

####2020