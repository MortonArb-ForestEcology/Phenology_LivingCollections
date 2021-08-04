# Additional stats for the mid-year report

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


# Weekly Challenge Graphs:
# Week 1. # obs pers list per week (or through time)
# Week 2. # phenophases observed in each list; bonus points: break down through time
