# Additional stats for the mid-year report
library(ggplot2)
library(lubridate)
library(dplyr)

source("../../NPN_Data_Utils/R/npn_get_obs.R")
source("../../NPN_Data_Utils/R/npn_enter_observation.R")
source("clean_google_form.R")
# npn_creds <- readLines("../data/NPN/.NPN_creds")
user_id <- strsplit(npn_creds[1], ":")[[1]][2]
user_pw <- strsplit(npn_creds[2], ":")[[1]][2]
# Loop through collections
sites.push <- c("Oak", "Maple", "Elm")
yrs.push <- c(lubridate::year(Sys.Date()))

stat.arb <- read.csv("../data/NPN/NPN_TheMortonArboretum_Stations_All.csv")

dat.npn <- rnpn::npn_download_status_data(station_ids=unique(stat.arb$station_id), years=2021, request_source="C. Rollinson, Morton Arb")
dat.npn$observation_date <- as.Date(dat.npn$observation_date)

summary(dat.npn)

# Children's Garden Data
dat.cg <- dat.npn[dat.npn$site_id == 37197,]

# Meadow Lake Data
dat.ml <- dat.npn[dat.npn$site_id == 37196,]

View(dat.cg)

# Weekly Challenge Graphs:
# Week 1. # obs pers list per week (or through time)
# Week 2. # phenophases observed in each list; bonus points: break down through time

#Subsetting out columns in Childen's Garden to make this easier to work with. 
dat.cgs <- subset(dat.cg, select=c(11,14:18))
summary(dat.cgs)
#View(dat.cgs)

#finding the minimimum and maximum range and mean of the dates observations were taken in the Childrens Garden.
#Note the na.rm=T which is removing N/A values
min(dat.cgs$observation_date)
max(dat.cgs$observation_date)
range(dat.cgs$observation_date)
mean(dat.cgs$observation_date,na.rm=T)

#Seperating observation date into weeks using lubridate
dat.cgs$week <- lubridate::week(dat.cgs$observation_date)
summary(dat.cgs)


#I know I need to make it so the weeks are renamed 1,2 etc instead of week 22, 23, etc Trying to do that here
#dat.cgs2 <- mutate(week=cut.Date(order_by(week),breaks = "1 week", labels = FALSE))
#summary(dat.cgs2)

#Or maybe
#dat.cgs2 <- dat.cgs[dat.cgs$week(week=cut.Date(order_by(week),breaks = "1 week", labels = FALSE))]
#arrange(order_by(week))

ggplot(data=dat.cgs,aes(x=week,fill=as.factor(week), color=as.factor(week)))+geom_bar()+
theme_bw()+
  labs(title="Observations of the Childrens Garden by Week", x="Week")
 

################################
###############
#Doing this for Meadow Lake
###############
################################
#Subsetting out columns in Childen's Garden to make this easier to work with. 
dat.mls <- subset(dat.ml, select=c(11,14:18))
summary(dat.mls)
#View(dat.cgs)

#finding the minimimum and maximum range and mean of the dates observations were taken in the Childrens Garden.
#Note the na.rm=T which is removing N/A values
min(dat.mls$observation_date)
max(dat.mls$observation_date)
range(dat.mls$observation_date)
mean(dat.mls$observation_date,na.rm=T)

#Seperating observation date into weeks using lubridate
dat.mls$week <- lubridate::week(dat.mls$observation_date)
summary(dat.mls)


#I know I need to make it so the weeks are renamed 1,2 etc instead of week 22, 23, etc Trying to do that here
#dat.mls2 <- mutate(week=cut.Date(order_by(week),breaks = "1 week", labels = FALSE))
#summary(dat.mls2)

#Or maybe
#dat.mls2 <- dat.mls[dat.mls$week(week=cut.Date(order_by(week),breaks = "1 week", labels = FALSE))]
#arrange(order_by(week))

ggplot(data=dat.mls,aes(x=week,fill=as.factor(week), color=as.factor(week)))+geom_bar()+
  theme_bw()+
  labs(title="Observations of the Childrens Garden by Week", x="Week")



