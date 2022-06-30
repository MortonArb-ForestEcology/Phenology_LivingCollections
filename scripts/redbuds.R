library(readr)
library(tidyverse)
library(ggplot2)
#reading jusy TMA data
dat.red <- read_csv("~/Desktop/d-sets/Redbud_bloom_obs.csv")
summary(dat.red)
#checking the standard deviation on the yday
sd(dat.red$yday)

#checking standard error on yday
SE<- sd(dat.red$yday)/sqrt(length(dat.red$yday))

#adding date
dat.red$year <- lubridate::year(dat.red$Date)
head(dat.red)

#Just a little graphic display
ggplot(dat.red, aes(y=yday, x=year, color=year)) +
  scale_color_gradientn(colors = rainbow(10)) +
  geom_point() + 
  geom_smooth(method=lm)

 ggplot(dat.red, alpha=0.5,aes(group=year, x=year, y=yday, color=year)) +
  scale_color_gradientn(colors = rainbow(10)) +
  geom_boxplot() 
 

ggplot(dat.red, aes(y=yday, x=year, color=year)) +
  scale_color_gradientn(colors = rainbow(10)) +
  geom_crossbar() + 
  geom_smooth(method=lm)

#Readinb in "budburst" and NPN data

dat.otred <- read.csv("~/Desktop/d-sets/NPNBB.csv")

summary(dat.otred)
#setting yday
dat.otred$yday <- lubridate::yday(dat.otred$Date.Observed)
summary(dat.otred)

#checking the standard deviation on the yday
sd(dat.otred$yday)

#adding date
dat.otred$year <- lubridate::year(dat.otred$Date.Observed)
head(dat.otred)

#gettting observations after 2013
dat.otred <- dat.otred [dat.otred$year>"2013",]
summary(dat.otred)
#graphing
ggplot(dat.otred, aes(y=yday, x=year, color=year)) +
  scale_color_gradientn(colors=rainbow(9)) +
  facet_grid(Data.Set~.)+
  geom_point() + 
  geom_smooth(method=lm)



ggplot(dat.otred, alpha=0.5,aes(group=year, x=year, y=yday,)) +
  scale_fill_gradientn(colors = rainbow(12)) +
  facet_grid(Data.Set~.)+
  geom_boxplot() + 
  geom_smooth(method=lm)
