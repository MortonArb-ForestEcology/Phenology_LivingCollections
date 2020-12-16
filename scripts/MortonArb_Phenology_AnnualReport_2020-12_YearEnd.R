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

#Getting the data for all Acer
# Downloading 2019 data
quercus20 <- clean.google(collection="Quercus", dat.yr=2020)
quercus20$Collection <- as.factor("Quercus")
quercus20$Year <- lubridate::year(quercus20$Date.Observed)
summary(quercus20)

quercus19 <- clean.google(collection="Quercus", dat.yr=2019)
quercus19$Collection <- as.factor("Quercus")
quercus19$Year <- lubridate::year(quercus19$Date.Observed)
summary(quercus19)

ulmus20 <- clean.google(collection="Ulmus", dat.yr=2020)
ulmus20$Collection <- as.factor("Ulmus")
ulmus20$Year <- lubridate::year(ulmus20$Date.Observed)
summary(ulmus20)


dat.all <- rbind(quercus20, quercus19, acer20, ulmus20)
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
summary(dat.all)

###########
###########
# Do whatever you do to get the first color observations
###########
###########


ggplot(data=dat.all[dat.all$leaf.color.observed=="Yes",]) +
  facet_grid(Collection ~ .) + # This is the code that will stack everything
  geom_density(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue")) +
  scale_color_manual(name="Year", values=c("2018"="red", "2019"="orange", "2020"="blue")) +
  theme_bw()+
  labs(title="Average Day of First Fruit Drop in the Acer Collection", x="Day of Year")
