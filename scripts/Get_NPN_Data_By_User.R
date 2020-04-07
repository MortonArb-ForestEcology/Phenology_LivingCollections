#----------------------------------------------------------
# Getting all observations for specific people that are associated with The Morton Arboretum, but aren't observing on site
#----------------------------------------------------------
# -----------------------------------
# -----------------------------------
# 1. Get People in our Network 
#    -- Currently downloaded from Nature's Notebook:
#       1. My Observation Deck --> edit users --> 
#          downloads: group Roster
# 2. Get at home sites for our people: 
#    getAllStations (GetStations Port) -- station = site
#    key inputs: 
#      - state_code -- can filter to IL
#      - person_id -- get our users
#    key outputs:
#      - station_id; latitude; longitude
#
# 3. Get observations of our people's sites
#    getObservations (use npn_get_obs.R)
#    key inputs:
#      - station_id
#    key outputs
#      - (whatever has been output in the past)
# -----------------------------------
#
# -----------------------------------
# NOTES!!
# -----------------------------------
# 1. This uses functions from the Morton Forest Ecology repo: NPN_Data_Utils: https://github.com/MortonArb-ForestEcology/NPN_Data_Utils
#    1.a. Default assumption will be that the NPN repository will be housed in the same place as this script
# 2. The observer list is stored in this github repository for simplicity.  If you do not have it: 
#    2.1. Copy it from our Google Drive Folder: LivingCollections_Phenology/Observing_NPN 
#    2.2. Put it in the Github Repository: Phenology_LivingCollections/data/NPN/
# -----------------------------------
#----------------------------------------------------------
library(ggplot2)



# -----------------------------------
# 1. Get People in our Network 
# -----------------------------------
observers <- read.csv("../data/NPN/groupRoster_The_Morton_Arboretum.csv")
summary(observers)
# -----------------------------------

# -----------------------------------
# 2. Get sites for our people: 
# -----------------------------------
source("../../NPN_Data_Utils/R/npn_get_stations.R")

dat.station <- data.frame()
pb <- txtProgressBar(min=0, max=nrow(observers), style=3)
for(i in 1:nrow(observers)){
  setTxtProgressBar(pb, i)
  dat.tmp <- npn.getStations(state_code="IL", person_id=observers$Person_ID[i], network_ids=NULL)
  
  dat.station <- rbind(dat.station, dat.tmp)
}
dat.station <- dat.station[!is.na(dat.station$station_id),]
summary(dat.station)
# -----------------------------------

# -----------------------------------
# 3. Get observations of our people's sites
# -----------------------------------
source("../../NPN_Data_Utils/R/npn_get_obs.R")

# Note this step can be quite slow!
# Note: Currently no observaitons beyond May 2018 :-(
obs <- npn.getObs(station_id=dat.station$station_id, start_date = "2018-01-01")
obs[obs==-9999] <- NA
obs$phenophase_status <- car::recode(obs$phenophase_status, "'0'='no'; '1'='yes'; '-1'='uncertain'")
obs$phenophase_status <- factor(obs$phenophase_status, levels=c("no", "yes", "uncertain"))
summary(obs)
# -----------------------------------
