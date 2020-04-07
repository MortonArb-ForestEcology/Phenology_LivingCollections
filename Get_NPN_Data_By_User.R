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
observers <- read.csv("")
# -----------------------------------

# -----------------------------------
# 2. Get sites for our people: 
# -----------------------------------
# -----------------------------------

# -----------------------------------
# 3. Get observations of our people's sites
# -----------------------------------
# -----------------------------------
