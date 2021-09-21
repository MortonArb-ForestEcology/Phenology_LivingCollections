library(ggplot2)
library(googlesheets4)
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

source("clean_google_form.R")

#getting all phenology data by collection by year
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

dat.all <- rbind(quercus18, quercus19, quercus20, quercus21, acer19, acer20, acer21)
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
summary(dat.all)

#Checking to see range of dates
range(dat.all$Date.Observed)
dat.all[lubridate::year(dat.all$Date.Observed)<lubridate::year(Sys.Date()),1:6]
dat.all[lubridate::year(dat.all$Date.Observed)>lubridate::year(Sys.Date()),1:6]
dat.all[dat.all$Date.Observed>Sys.Date(),1:6]

##
###subset out for just observers
##
dat.ob <- subset(dat.all, select = c(2:3, 28:29))
summary(dat.ob)

#dat.ob <- dat.all[dat.all$Observer, c("Date.Observed", "Year", "Observer", "Collection")]
#summary(dat.ob)

#dat.ob <- dat.ob[!is.na(dat.ob$Observer),]
#summary(dat.ob)
#head(dat.ob)
#View(dat.ob)

## makeing a yday

dat.ob$yday<- lubridate::yday(dat.ob$Date.Observed)
summary(dat.ob)

##
#getting a graph of #of observations per observer
##
ggplot(data=dat.ob) +
  facet_grid(Year~ .) + # This is the code that will stack everything
  geom_bar(alpha=0.5, aes(x=Observer, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue", "2021"="green")) +
  scale_color_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue", "2021"="green")) +
  theme_bw()+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  labs(title="# of observations per observer", x="Observer name")

