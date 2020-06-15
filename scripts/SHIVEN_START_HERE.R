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
# dir("../../")
download.ghcn(ID=ID, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T) # This downloads the data and puts it in the path.ghcn
# -------------------------------------

# -------------------------------------
# 2. Read in the data you have on hand and play around with it.
# -------------------------------------
dat.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
dat.ghcn$DATE <- as.Date(dat.ghcn$DATE) # Converts this to a date format so it knows about time
summary(dat.ghcn) # 
tail(dat.ghcn)

# CHALLENGE: Make 1 graph from the weather data.  Start with what you want to show.  Once you've done it, save it and post it to slack.  Also, save how you did it and commit it to Github!
# I created a graph that displays the min and max temperatures for the first year since the data was too crowded for the entire dataset
ggplot(dat.ghcn[1:365, ]) + geom_point(aes(x= YDAY, y=TMAX, color="TMAX")) + #maximum temp values: supposed to be red but are blue
  geom_point(aes(x=YDAY, y=TMIN, color="TMIN")) + #minimum temp values: supposed to be blue but are red
  labs(title="Temp Over the Year", x="Day of the Year", y="Temp Range (in Celsius)", scale_color_manual(name="Temp Type", values=c("red", "blue"))) #labels: struggled to change legend labels

head(dat.ghcn)

# Making another graph that displays the yearly temp mins and maxes in a plot
# Started off by creating functions to calculate the mins and maxes of each year and putting them in a data frame
Tmax_year <- function (YEAR) {
  yearly_max <- max(dat.ghcn$TMAX[dat.ghcn$YEAR==YEAR], na.rm=FALSE)
  return(yearly_max)
}
Tmin_year <- function (YEAR) {
  yearly_min <- min(dat.ghcn$TMIN[dat.ghcn$YEAR==YEAR], na.rm=FALSE)
  return(yearly_min)
}

#data frame with all the yearly extremes but did not include 2007 or 2020 because those years were incomplete
# yearly_Textremes <- data.frame("Year" = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019), 
#                             "YearlyTempMax" = c(Tmax_year(2008), Tmax_year(2009), Tmax_year(2010), Tmax_year(2011), 
                            #                       Tmax_year(1737, 2102), Tmax_year(2103, 2467), Tmax_year(2468, 2832), Tmax_year(2833, 3197),
                            #                       Tmax_year(3198, 3563), Tmax_year(3564, 3928), Tmax_year(3929, 4293), Tmax_year(4294, 4658)),
                            # "YearlyTempMin" = c(Tmin_year(276, 641), Tmin_year(642, 1006), Tmin_year(1007, 1371), Tmin_year(1372, 1736), 
                            #                       Tmin_year(1737, 2102), Tmin_year(2103, 2467), Tmin_year(2468, 2832), Tmin_year(2833, 3197), 
                            #                       Tmin_year(3198, 3563), Tmin_year(3564, 3928), Tmin_year(3929, 4293), Tmin_year(4294, 4658)))

                            
yearly_Textremes <- data.frame(Year=(min(dat.ghcn$YEAR)+1):(max(dat.ghcn$YEAR)-1),
                               YearlyTempMax=NA,
                               YearlyTempMin=NA)
for(i in 1:nrow(yearly_Textremes)){
  yearly_Textremes$YearlyTempMax[i] <- Tmax_year(yearly_Textremes$Year[i])
  yearly_Textremes$YearlyTempMin[i] <- Tmin_year(yearly_Textremes$Year[i])
}
yearly_Textremes

# Bar graph displaying mins and maxes of each year
ggplot(yearly_Textremes) + geom_bar(stat="identity", aes(x=Year, y=YearlyTempMax), fill="red") + 
  geom_bar(stat="identity", aes(x=Year, y=YearlyTempMin), fill="blue") +
  labs(title="Temp Extremes Over the Years", x= "Year", y= "Temperature Extreme") +
  theme_minimal()

# Creating a seasonal variable to new data frame
Season_Calculation <- function (row) {
  if ((dat.ghcn$MONTH[row, ] == 6 & dat.ghcn$DAY[row, ] >= 21) | dat.ghcn$MONTH[row, ] == 7 | dat.ghcn$MONTH[row, ] == 8 | (dat.ghcn$MONTH[row, ] == 9 & dat.ghcn$DAY[row, ] <= 20)) {
    Season = "Summer"
  } else { if ((dat.ghcn$MONTH[row, ] == 9 & dat.ghcn$DAY[row, ] >= 21) | dat.ghcn$MONTH[row, ] == 10 | dat.ghcn$MONTH[row, ] == 11 | (dat.ghcn$MONTH[row, ] == 12 & dat.ghcn$DAY[row, ] <= 20)) {
      Season = "Autumn"
      } else { if ((dat.ghcn$MONTH[row, ] == 12 & dat.ghcn$DAY[row, ] >= 21) | dat.ghcn$MONTH[row, ] == 1 | dat.ghcn$MONTH[row, ] == 2 | (dat.ghcn$MONTH[row, ] == 3 & dat.ghcn$DAY[row, ] <= 20)) {
        Season = "Winter"
        } else {
          Season = "Spring"
        }
      }
  }
  return(Season)
}

#Spring is the only season that is displayed
dat.ghcn[1:4815, ]
dat.ghcn <- cbind(dat.ghcn, Season_Calculation(1:4815, ))
ggplot(dat.ghcn) + geom_point(aes(x = TMIN, y = TMAX, col = dat.ghcn$SEASON))

# Just testing some stuff out
ggplot(yearly_Textremes, aes(x=Year, y=YearlyTempMin, size=YearlyTempMax)) + geom_point()
ggplot(dat.ghcn) + geom_point(aes(x = YEAR, y = PRCP))