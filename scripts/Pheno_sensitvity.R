# ---------------------------------
# 0. Set up some general file paths
# ---------------------------------
---------------------------------
library(ggplot2)
library(gghighlight)
library(readr)
library(lubridate)
library(tidyverse)
library(gganimate)
library(dplyr)
library(transformr)
# path.google <- "G:/My Drive" # Windows
path.google <- "/Volumes/GoogleDrive/My Drive" # Mac


path.out <- file.path(path.google, "LivingCollections_Phenology/Phenology Forecasting")
path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2023_02_EndOfYear_Report/figures_2023_end")
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

dir.met <- "../data_raw/meteorology"
dir.create(dir.met, recursive=T, showWarnings = F)
# ---------------------------------


# ---------------------------------
# 1. NOAA COOP Station data
# Use FedData Package to download station data and put it here
# ---------------------------------
ID="USC00115097"
vars.want <- c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD")
path.ghcn=c("../data_raw/meteorology/GHCN_extracted/")
dir.raw="../data_raw/meteorology/GHCN_raw/"

source("phenology weather/met_download_GHCN.R"); 
source("phenology weather/met_gapfill.R")

download.ghcn(ID=ID, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T, method="https")
# -------------------------------------


# -------------------------------------
# Calculating some cumulative statistics
# -------------------------------------
# Create a function to calculate values we're interested in for prediction
calc.indices <- function(dat){
  # Assumes upper case column names of: TMAX, TMIN, PRCP, YDAY
  # For chilling days, only start after the solstice (June 20th)
  dat$TMEAN <- apply(dat[,c("TMAX", "TMIN")], 1, mean)
  dat$GDD0 <- ifelse(dat$TMEAN>0, dat$TMEAN-0, 0)
  dat$GDD2 <- ifelse(dat$TMEAN>2, dat$TMEAN-2, 0)
  dat$GDD5 <- ifelse(dat$TMEAN>5, dat$TMEAN-5, 0)
  dat$CDD0 <- ifelse(dat$YDAY>172 & dat$TMEAN<0, 0-dat$TMEAN, 0)
  dat$CDD2 <- ifelse(dat$YDAY>172 & dat$TMEAN< -2, -2-dat$TMEAN, 0)
  dat$DaysNoRain <- NA
  
  dat[, c("GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum", "PRCP.cum", "GDD2.cum")] <- cumsum(dat[, c("GDD0", "GDD5", "CDD0", "CDD2", "PRCP","GDD2")])
  
  dat[, c("NORAIN.cum")] <- cumsum(ifelse(dat[,"PRCP"]>0, 1, 0))
  
  # Calculating days since rain just in case
  dat$DaysNoRain[1] <- ifelse(dat$PRCP[1]>0, 0, 1)
  for(i in 2:nrow(dat)){
    dat$DaysNoRain[i] <- ifelse(dat$PRCP[i]>0, dat$DaysNoRain[i-1]+1, 0)
  }
  return(dat)
}

# For the "historical" GHCN data
dat.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)
summary(dat.ghcn)

yr.min <- 2007
yr.max <- lubridate::year(Sys.Date())
dat.ghcn2 <- data.frame()
for(YR in yr.min:yr.max){
  rows.yr <- which(dat.ghcn$YEAR==YR)
  # dat.yr <- dat.ghcn[rows.yr,]
  dat.tmp <- calc.indices(dat=dat.ghcn[rows.yr,])
  dat.ghcn2 <- rbind(dat.ghcn2, dat.tmp)
}
summary(dat.ghcn2)
head(dat.ghcn2)
# dat.ghcn2[dat.ghcn2$DATE=="2020-04-09",]

#creating a data frame of just the last 5 years of weather data
dat.ghcn3 <- dat.ghcn2[dat.ghcn2$YEAR>=2019,]

#making sure the date being shown only shows spring dates
dat.ghcn33 <- dat.ghcn3[dat.ghcn3$YDAY<=359,]
summary(dat.ghcn33)
head(dat.ghcn33)

#Subsetting out uncecessary columns for the phenology report
dat.ghcn4 <- dat.ghcn33[ ,c("YEAR","MONTH", "TMAX", "TMIN","PRCP", "DATE", "YDAY", "TMEAN", "PRCP.cum", "GDD5.cum", "GDD2.cum")]
summary(dat.ghcn4)
head(dat.ghcn4)
