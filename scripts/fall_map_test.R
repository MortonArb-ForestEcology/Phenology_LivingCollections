
library('devtools')
# devtools::install_github("usa-npn/rnpn")

library(googlesheets4); library(car); library(lubridate)
library(ggplot2)
library(dplyr)
library(gifski)
library(gganimate)

#
# Making some directories to help with organization
## NOTE: This assumes you opened R by double-clicking this script in your github folder.  Your working directory (in the bar of the "Console" tab) should be [SOMETHIGN]/Collections-Habitat/scripts
# If it's not, you'll need to set your working directory to be here
# Once you do that, we can use the same file paths without having to worry about differences in where your github folder is vs. mine
path.figs <- "G://My Drive/LivingCollections_Phenology/Reports/2021_01_MidYear_Report/figures_spring_2021"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")



# Downloading 2021 data
quercus21 <- clean.google(collection="Quercus", dat.yr=2021)
quercus21$Collection <- as.factor("Quercus")
quercus21$Year <- lubridate::year(quercus21$Date.Observed)
summary(quercus21)

quercus.lci <- quercus21[quercus.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.intensity", "leaf.color.observed")]
quercus.lci <- quercus.lci[!is.na(quercus.lci$PlantNumber),]
summary(quercus.lci)
head(quercus.lci)

#finding the minimimum and maximum range and mean of the dates leaf color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(quercus.lci$Date.Observed)
max(quercus.lci$Date.Observed)
range(quercus.lci$Date.Observed)
mean(quercus.lci$Date.Observed,na.rm=T)

#Now make my Yday
quercus.lci$yday <- lubridate::yday(quercus.lci$Date.Observed)
summary(quercus.lci)

#Also looking at year as well not as important but nice to have
#quercus.fri$year <- lubridate::year(quercus.fri$Date.Observed)
#summary(quercus.fri)

#only looking at trees that showed leaf color in the las half of the year
quercus.lci <- quercus.lci [quercus.lci$yday>=180,]
summary(quercus.lci)

#checking
range(quercus.lci$Date.Observed)

# getting my Plant Numbers with Lat and long by accessing a google sheet with that information readily  available
locdat <- read.csv("G:/My Drive/LivingCollections_Phenology/Observing Lists/Quercus/ObservingLists_Quercus.csv")
loc.dat <- data.frame(locdat)
head(loc.dat)

#creating  new columns in quercus.lci that contain the latitude & longitude for all matching plant numbers between
#quercus.lci and loc.dat
quercus.lci$BgLatitude <-loc.dat$BgLatitude[ match(quercus.lci$PlantNumber,loc.dat$PlantNumber)]
quercus.lci$BgLongitude <-loc.dat$BgLongitude[ match(quercus.lci$PlantNumber,loc.dat$PlantNumber)]

head(quercus.lci)

#aggregating
ileaf.color <- aggregate(yday ~ PlantNumber + Species + BgLongitude + BgLatitude + Year + leaf.color.observed + leaf.color.intensity, data=quercus.lci, na.rm=T)
summary(ileaf.color)
head(ileaf.color)

which.state <- "Illinois"
county.info <- map_data("county", region=which_state)
pheno.map<- ggplot(data = county_info, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "gray19") +
  coord_quickmap() +
  theme_void()

print(pheno.map)

#setting the limits of the map to the limits of the lat and long where data was collected
min_long <- min(ileaf.color$BgLongitude)
max_long <- max(ileaf.color$BgLongitude)
min_lat <- min(ileaf.color$BgLatitude)
max_lat <- max(ileaf.color$BgLatitude)
num_days <- max(ileaf.color$yday) - min(ileaf.color$yday) + 1


### attempting to map and animate
anim_save(file.path(path.figs,"Fall_Color_Presence.gif"), height=4, width=6, units="in", res=320 )
map_with_data <- pheno.map +
  geom_point(data = ileaf.color, aes(x = BgLongitude, y = BgLatitude, group=yday, color=leaf.color.observed)) +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat)) +
  transition_time(yday) +
  ggtitle('yday: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}') +
  labs(title="Fall Color Presence",)+
  shadow_mark()
animate(map_with_data, nframes = num_days, fps = 10)

###doing this without shadowmark
map_with_data <- pheno.map +
  geom_point(data = ileaf.color, aes(x = BgLongitude, y = BgLatitude, group=yday, color=leaf.color.observed)) +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat)) +
  transition_time(yday) +
  ggtitle('yday: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}') +
  labs(title="Fall Color Presence",)
  
animate(map_with_data, nframes = num_days, fps = 10)

###that was weird, but maybe it will look better with intensity
anim_save(file.path(path.figs,"Fall_Color_Intensity.gif"), height=4, width=6, units="in", res=320 )
map_with_data <- pheno.map +
  geom_point(data = ileaf.color, aes(x = BgLongitude, y = BgLatitude, group=yday, color=leaf.color.intensity)) +
  scale_color_manual(name="intensity", values=c("0%"="gray19", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat)) +
  transition_time(yday) +
  ggtitle('yday: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}') +
  shadow_mark() +
  labs(title="Fall Color intensity",)
animate(map_with_data, nframes = num_days, fps = 5)

###trying without shadow mark

map_with_data <- pheno.map +
  geom_point(data = ileaf.color, aes(x = BgLongitude, y = BgLatitude, group=yday, color=leaf.color.intensity)) +
  scale_color_manual(name="intensity", values=c("0%"="white", "<5%"="orange", "5-24%"="yellow", "25-49%"="green", "50-74%"="blue", "75-94%"="blue3", ">95%"="violet")) +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat)) +
  transition_time(yday) +
  ggtitle('yday: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}') +
  labs(title="Fall Color intensity",)
animate(map_with_data, nframes = num_days, fps = 5)





################### graph grave yard #################
#pheno.map +
   #geom_point(data = ileaf.color, aes(x = BgLongitude, y = BgLatitude, group=PlantNumber, color=leaf.color.intensity)) +
  #coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat)) +
    # transition_time(yday) +
  #ggtitle('day of year: {frame_time}',
  #subtitle = 'Frame {frame} of {nframes}') +
  #animate(data.map, nframes = num_yday, fps = 15)


#data.map <- pheno.map +
 # geom_point(data = quercus.lci, aes(x = BgLatitude, y = BgLatitude, group=PlantNumber, color=leaf.color.intensity)) +
  #coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat)) +
  #transition_time(yday) +
  #ggtitle('day of year: {frame_time}',
          #subtitle = 'Frame {frame} of {nframes}') +
#animate(data.map, nframes = num_yday, fps = 15)

   
   #pheno.map +
  # geom_point(data = ileaf.color, aes(x = BgLongitude, y = BgLatitude, group=PlantNumber, color=leaf.color.intensity)) +
     #coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat)) +
   #  transition_time(yday) +
    # ggtitle('day of year: {frame_time}',
     #        subtitle = 'Frame {frame} of {nframes}') +
     #animate(data.map, nframes = num_yday, fps = 15)
   