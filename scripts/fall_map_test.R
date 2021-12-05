# install.packages("devtools")
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

quercus.lci <- quercus21[quercus.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.intensity", "leaf.present.observed")]
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

locdat <- read.csv("G:/My Drive/LivingCollections_Phenology/Observing Lists/Quercus/ObservingLists_Quercus.csv")
loc.dat <- data.frame(locdat)
head(loc.dat)

which.state <- "Illinois"
county.info <- map_data("county", region=which_state)
pheno.map <- ggplot(data = county.info, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "white", fill = "grey") +
  coord_quickmap() +
  theme_void()

print(pheno.map)


min_long <- min(loc.dat$BgLongitude)
max_long <- max(loc.dat$BgLongitude)
min_lat <- min(loc.dat$BgLatitude)
max_lat <- max(loc.dat$BgLatitude)
num_days <- max(quercus.lci$yday) - min(quercus.lci$yday) + 1

map_with_data <- pheno.map +
  geom_point(data = quercus.lci, aes(x = long, y = lat, group=PlantNumber, color=leaf.color.intensity)) +
  coord_quickmap(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat)) +
  transition_time(yday) +
  ggtitle('year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}') +
animate(map_with_data, nframes = num_yday, fps = 15)
