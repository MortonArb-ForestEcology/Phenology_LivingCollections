# ---------------------------------
# Script to get Shiven started on working with some of the data we want him to make pretty.

# Starter Dataset: Recent past weather data: NOAA COOP Station ID 
#    - The Morton Arboretum current Station

# Much of this script is pirated from the "Phenology_Forecasting" repostiroy, specifically script, "M1_MeteorologyData_Download.R" script.  Apologies if context is missing
#
# Workflow
# 0. Set up file paths, load libraries, etc.
# 1. Download the latest weather data.  Can skip this if you already have data to work with
# 2. Read in the data you have on hand and play around with it.
# downloaded some random packages to see if they make it work
library(rgdal)
library(ggmap)
library(raster)
library(dplyr)
library(leaflet)
library(rgeos)
library(tidyr)
library(xts)
library(dygraphs)
library(sp)
library(tigris)

# ---------------------------------
# 0. Set up some general file paths
# ---------------------------------
library(ggplot2)

dir.met <- "../data_raw/meteorology" # Setting up a folder where I want to store the latest met data
dir.create(dir.met, recursive=T, showWarnings = F) # This will create the folder if it doesn't already exist
# ---------------------------------


# ---------------------------------
# 1. Download NOAA COOP Station data
# Website I used to find information: https://cran.r-project.org/web/packages/rnoaa/rnoaa.pdf
library(rnoaa)
# Use FedData Package to download station data and put it here
library("FedData")
ID="USC00115097" # The Arb's weather station ID
vars.want <- c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD") # Variables we want to pull from the internet
path.ghcn="../data_raw/meteorology/GHCN_extracted/"
dir.raw="../data_raw/meteorology/GHCN_raw/"

# Creating the folders in case they don't already exist
dir.create(dir.raw, recursive=T, showWarnings = F)
dir.create(path.ghcn, recursive=T, showWarnings = F)

# If you haven't already, clone the "Phenology_Forecasting" repository.  Make sure it's in the same place as the "Phenology_LivingCollections" repository.  Doing this means that we don't have to maintain two copies of the same function
# Repository URL: https://github.com/MortonArb-ForestEcology/Phenology_Forecasting
# Note: this script might have some packages you need to install, so open it up and see what Rstudio tells you to install
source("../../Phenology_Forecasting/scripts/met_download_GHCN.R")
source("../../Phenology_Forecasting/scripts/met_gapfill.R")
dir("../../")
download.ghcn(ID=ID, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T) # This downloads the data and puts it in the path.ghcn
# -------------------------------------

# -------------------------------------
# 2. Read in the data you have on hand and play around with it.
# -------------------------------------
dat.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
dat.ghcn$DATE <- as.Date(dat.ghcn$DATE) # Converts this to a date format so it knows about time
summary(dat.ghcn) # 

# CHALLENGE: Make 1 graph from the weather data.  Start with what you want to show.  Once you've done it, save it and post it to slack.  Also, save how you did it and commit it to Github!
# I created a graph that displays the min and max temperatures for the first year since the data was too crowded for the entire dataset
ggplot(dat.ghcn[1:365, ]) + geom_point(aes(x= YDAY, y=TMAX, color="TMAX")) + #maximum temp values: supposed to be red but are blue
  geom_point(aes(x=YDAY, y=TMIN, color="TMIN")) + #minimum temp values: supposed to be blue but are red
  labs(title="Temp Over the Year", x="Day of the Year", y="Temp Range (in Celsius)", scale_color_manual(name="Temp Type", values=c("red", "blue"))) #labels: struggled to change legend labels