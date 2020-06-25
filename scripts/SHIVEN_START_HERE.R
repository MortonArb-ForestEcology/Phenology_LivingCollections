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
# Season_Calculation <- function (row) {
dat.ghcn <- data.frame(dat.ghcn, Season = NA)
head(dat.ghcn)
  
for (i in 1:nrow(dat.ghcn)) {
  if ((dat.ghcn$MONTH[i] == 6 & dat.ghcn$DAY[i] >= 21) | dat.ghcn$MONTH[i] == 7 | dat.ghcn$MONTH[i] == 8 | (dat.ghcn$MONTH[i] == 9 & dat.ghcn$DAY[i] <= 20)) {
    dat.ghcn$Season[i] = "Summer"
  } else { if ((dat.ghcn$MONTH[i] == 9 & dat.ghcn$DAY[i] >= 21) | dat.ghcn$MONTH[i] == 10 | dat.ghcn$MONTH[i] == 11 | (dat.ghcn$MONTH[i] == 12 & dat.ghcn$DAY[i] <= 20)) {
      dat.ghcn$Season[i] = "Autumn"
      } else { if ((dat.ghcn$MONTH[i] == 12 & dat.ghcn$DAY[i] >= 21) | dat.ghcn$MONTH[i] == 1 | dat.ghcn$MONTH[i] == 2 | (dat.ghcn$MONTH[i] == 3 & dat.ghcn$DAY[i] <= 20)) {
        dat.ghcn$Season[i] = "Winter"
        } else {
          dat.ghcn$Season[i] = "Spring"
        }
      }
    }
  }
# }

#Spring is the only season that is displayed: fixed
head(dat.ghcn)
ggplot(dat.ghcn) + geom_point(aes(x = TMIN, y = TMAX, col = Season))


# Just testing some stuff out
ggplot(data = subset(yearly_Textremes, Year >= 2010), aes(x=Year, y=YearlyTempMin, size=YearlyTempMax)) + geom_point()
ggplot(dat.ghcn) + geom_point(aes(x = YEAR, y = PRCP, col=TMAX))


# Attempts to Make a Line Graph: realized geom_line vs geom_path does not matter in time series graph
# I can't figure out how to maually change the tick marks: figured it out
# Tried to create a new column so the mins and maxes of the years would connect but did not work
yearly_Textremes$grp = NA
head(yearly_Textremes)
for (i in 1:nrow(yearly_Textremes)) {
  yearly_Textremes$grp[i] = i
}
head(yearly_Textremes)

#can't figure out how to portray the differences betweeen the mins and maxes each year
ggplot(data = yearly_Textremes) + geom_line(aes(x = Year, y = YearlyTempMax, col = "YearlyTempMax")) + #lines connecting yearly temp maxes: red
  geom_point(aes(x = Year, y = YearlyTempMax), group = yearly_Textremes$grp) + #yearly temp max: black dots
  geom_line(aes(x = Year, y = YearlyTempMin, col = "YearlyTempMin")) +  #line connecting yearly temp mins: navy
  geom_point (aes(x = Year, y = YearlyTempMin)) + #yearly temp min: black dots
  geom_line(aes(x = Year, y = mean(YearlyTempMin), col = "AvgYearlyTempMin"), linetype = 2) + #average min temp: blue dashed lin
  geom_line(aes(x = Year, y = mean(YearlyTempMax), col= "AvgYearlyTempMax"), linetype = 2) + #average max temp: red dashed line
  labs(title = "Yearly Temp Extremes", x = "Year", y = "Temp Extreme (in Celsius)", 
       scale_color_manual(name = "Temp Type", values = c("red", "navy", "navy", "red"))) + #coloring for these lines is not working
  scale_x_continuous(breaks = seq(2008, 2020, 2)) + theme_minimal() +
  geom_rect(aes(xmin=Year-0.25, xmax=Year+0.25, ymin=YearlyTempMin, ymax=YearlyTempMax, fill = YearlyTempMax-YearlyTempMin))
  
  
#new line graph of 2019 temp extremes: looks like a lot, is there any way to make it look cleaner?
ggplot(data = subset(dat.ghcn, YEAR == 2019)) + geom_line(aes(x = YDAY, y = TMAX, col = "TMAX")) + #red line of temp maxes throughout the year
  geom_point(aes(x = YDAY, y = TMAX)) + #black points of temp maxes throughout the year
  geom_line(aes(x = YDAY, y = TMIN, col = "TMIN")) + #blue line of temp mins throughout the year
  geom_point(aes(x = YDAY, y = TMIN)) + #black points of temp mins throughout the year
  geom_smooth(data = dat.ghcn, aes(x = YDAY, y = TMAX, col = "TMAX")) + #red parabolic curve to fit temp max data
  geom_smooth(data = dat.ghcn, aes(x = YDAY, y = TMIN, col = "TMIN")) + #blue parabolic curve to fit temp min data
  labs(title = "2019 Daily Temp Extremes", x = "Day of Year", y = "Temp Extreme", #label names
       scale_color_manual(name = "Temp Type", values = c("red", "blue"))) + #formats lines properly
  scale_x_continuous(breaks = seq(0, 365, 30)) + scale_y_continuous(breaks = seq(-40, 40, 10)) + #fixed scaling to make more sense
  theme_minimal()


#Made an interactive graph: is there anyway to make it load faster?
PrecipGraph <- ggplot(data = subset(dat.ghcn, PRCP > 2 & YEAR == 2008)) + geom_point(aes(x = YDAY, y = PRCP, col = PRCP))
library(plotly)
ggplotly(PrecipGraph)


#Interactive Line graph of TMAX averages of seasons of each year with different lines for each year 
# SpringSum = 0
# WinterSum = 0
# SummerSum = 0
# AutumnSum = 0
# SpringCount = 0
# SummerCount = 0
# WinterCount = 0
# AutumnCount = 0
# 
# for (i in 1:nrow(dat.ghcn)) {
#   if (dat.ghcn$Season[i] == "Spring") {
#     SpringSum = SpringSum + dat.ghcn$TMAX[i]
#     SpringCount = SpringCount + 1
#   } else if (dat.ghcn$Season[i] == "Summer") {
#     SummerSum = SummerSum + dat.ghcn$TMAX[i]
#     SummerCount = SummerCount + 1
#     } else if (dat.ghcn$Season[i] == "Autumn") {
#       AutumnSum = AutumnSum + dat.ghcn$TMAX[i]
#       AutumnCount = AutumnCount + 1
#       } else {
#         WinterSum = WinterSum + dat.ghcn$TMAX[i]
#         WinterCount = WinterCount + 1
#       }
#   SpringAvg = SpringSum/SpringCount
#   SummerAvg = SummerSum/SummerCount
#   AutumnAvg = AutumnSum/AutumnCount
#   WinterAvg = WinterSum/WinterCount
# }
# 
# 
# 
# #I didn't make the Seasons numerics because then I wouldn't be able to make a line graph, how could group be implemented here?
# SeasonalAverages = data.frame(Season = c("Spring", "Summer", "Autumn", "Winter"), SeasonAvg = c(SpringAvg, SummerAvg, AutumnAvg, WinterAvg))
# SeasonalAverages

#individual meeting changes
dat.season <- aggregate(cbind(TMAX, TMIN, PRCP) ~ Season,
                        data=dat.ghcn,
                        FUN=mean)
dat.season$Season <- factor(dat.season$Season, levels = c("Winter", "Spring", "Summer", "Autumn"))
dat.season

dat.season.yr <- aggregate(cbind(TMAX, TMIN, PRCP) ~ Season + YEAR,
                           data=dat.ghcn,
                           FUN=mean)
dat.season.yr$Season <- factor(dat.season.yr$Season, levels = c("Winter", "Spring", "Summer", "Autumn"))
summary(dat.season.yr)
head(dat.season.yr)

# make graph that you intended to make earlier: Interactive Line graph of TMAX averages of seasons total and of each year with different lines for each year 
# not working because season is a categorical variable
TotalSeasonGraph <- ggplot (data = dat.season) + geom_point(aes(x = Season, y = TMAX))
ggplotly(TotalSeasonGraph)
YearlySeasonGraph <- ggplot (data = dat.season.yr, aes(label = TMIN, PRCP)) + geom_point (aes(x = YEAR, y = TMAX, col = Season))
ggplotly(YearlySeasonGraph) #not working: fixed, figured out how to add different

# #line was not showing up
# SeasonAvgGraph <- ggplot(data = SeasonalAverages) + geom_line(aes(x = Season, y = SeasonAvg)) +
#   geom_point(aes(x = Season, y = SeasonAvg)) + theme_minimal() +
#   labs(title = "Seasonal Averages during 2007-2020", x = "Season", y = "Season Average (in Celsius")
# SeasonAvgGraph
# ggplotly(SeasonAvgGraph)


#practice with the aggregate function
#mean temp extremes for each month: 2 different ways of doing it
agg_mean <- aggregate(dat.ghcn[5:6], by=list(dat.ghcn$MONTH), FUN=mean)
  agg_mean %>% rename(agg_mean$Group.1=agg_mean$MONTH)
  agg_mean
agg_mean2 <- aggregate(cbind(PRCP,SNOW,SNWD) ~ MONTH,
                       data = dat.ghcn, FUN=mean) 
  agg_mean2


#practice with the merge function: not working properly as the months is repeated 12 times with same values
merge(x=agg_mean2, y=agg_mean[,2:3])
  
#practice with the stack function
