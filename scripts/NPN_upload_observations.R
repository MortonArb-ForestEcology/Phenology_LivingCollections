# ------------------------------------------
# Push our data to NPN
# Author:Christy Rollinson
# ------------------------------------------
# 1. download our observations from Google
# 2. Format to match NPN data set
# 3. Compare to list of observations from NPN
# 4. subset to new observations
# 5. push by individual observation set
# ------------------------------------------

# ------------------------------------------
# May need to move this up!
# Pull Morton Arb data (network ID = 720) & get rid of anything already there
# ------------------------------------------
source("../../NPN_Data_Utils/R/npn_get_obs.R")
dat.npn <- npn.getObs(station_id = 26202)
summary(dat.npn)
# ------------------------------------------


# ------------------------------------------
# 1. download our observations from Google
# ------------------------------------------
source("clean_google_form.R")
dat.test <- clean.google(dat.yr = 2020)
summary(dat.test)

names(dat.test)
# ------------------------------------------

# ------------------------------------------
# 2. Format to match NPN data set
# Station IDs
# Oak Collection
# Maple Collection
# Schulenberg Prairie = 
# ------------------------------------------
# read in our column crosswalk
xwalk <- read.csv("../data/NPN/NPN_Phenophase_Crosswalk.csv")
xwalk

# merge in our NPN data
station = 26202 # Oak Collection
file.inds <- "../data/NPN/NPN_MortonArb_OakCollection_Individuals.csv"
if(file.exists(file.inds)){
  arb.inds <- read.csv(file.inds)
} else {
  source("../../NPN_Data_Utils/R/npn_getIndAtStation.R")
  arb.inds <- npn.getIndAtStation(station_ids = station)
  arb.inds$station_id <- station
  write.csv(arb.inds, "../data/NPN/NPN_MortonArb_OakCollection_Individuals.csv", row.names=F)
}
arb.inds
arb.inds$PlantNumber <- arb.inds$individual_name

dat.test <- dat.test[dat.test$PlantNumber %in% arb.inds$individual_name, ]
dim(dat.test)

names(dat.test)[names(dat.test) %in% paste0(xwalk$MortonArb.Description, ".observed")]
# paste0(xwalk$MortonArb.Description, ".observed")[!paste0(xwalk$MortonArb.Description, ".observed") %in% names(dat.test)]
for(COL in names(dat.test)){
  if(COL %in% c("Timestamp", "Date.Observed")) next
  dat.test[,COL] <- as.character(dat.test[,COL])
}


dat.status <- stack(dat.test[,paste0(xwalk$MortonArb.Description, ".observed")])
names(dat.status) <- c("status", "phenophase")
dat.status$phenophase <- gsub(".observed", "", dat.status$phenophase)
dat.status[,c("Observer", "Date.Observed", "Species", "PlantNumber", "Notes")] <- dat.test[,c("Observer", "Date.Observed", "Species", "PlantNumber", "Notes")]
head(dat.status)

dat.intensity <- stack(dat.test[,names(dat.test)[names(dat.test) %in% paste0(xwalk$MortonArb.Description, ".intensity")]])
names(dat.intensity) <- c("intensity", "phenophase")
dat.intensity$phenophase <- gsub(".intensity", "", dat.intensity$phenophase)
dat.intensity[,c("Observer", "Date.Observed", "PlantNumber")] <- dat.test[,c("Observer", "Date.Observed", "PlantNumber")]
summary(dat.intensity)

# Merge the status and intensity together
dat.long <- merge(dat.status, dat.intensity, all=T)
dat.long <- merge(dat.long, arb.inds, all.x=T)
dat.long <- merge(dat.long, xwalk, by.x="phenophase", by.y="MortonArb.Description")
dat.long$phenophase_status <- car::recode(dat.long$status, "'No'='0'; 'Yes'='1'; 'Unsure'='-1'; 'No Observation'=NA")
dat.long$intensity[dat.long$intensity %in% c("None", "0", "0%")] <- NA
dat.long$intensity_id <- car::recode(dat.long$intensity, 
                                     "'<3'='32';
                                      '3-10'='32'; 
                                      '11-100'='39';
                                      '101-1,000'='40';
                                      '1,001-10,000'='41';
                                      '>10,000'='42'; 
                                      '<5%'='25'; 
                                      '5-24%'='26';
                                      '25-49%'='27';
                                      '50-74%'='28';
                                      '75-94%'='29';
                                      '>95%'='30';
                                      'Little'='44';
                                      'Some'='45';
                                      'Lots'='46'")
summary(dat.long)

for(COL in names(dat.long)){
  if(COL %in% c("Date.Observed")) next
  dat.long[,COL] <- as.factor(dat.long[,COL])
}
summary(dat.long)
dim(dat.long)

# Do some recoding to make things line up with NPN
dat.arb <- data.frame(phenophase_id=dat.long$NPN.Code,
                      individual_id=dat.long$individual_id,
                      observation_date=dat.long$Date.Observed,
                      observation_extent=dat.long$phenophase_status,
                      observation_comment=paste0(gsub(" " , "_", dat.long$Notes), "__Uploaded_via_R_by_CR"),
                      observation_value_id=dat.long$intensity_id)
dat.arb <- dat.arb[!is.na(dat.arb$observation_extent),]
summary(dat.arb)
# ------------------------------------------



# ------------------------------------------
# Loop through and upload
# ------------------------------------------
dat.now <- dat.arb[dat.arb$individual_id==dat.arb$individual_id[1] & dat.arb$observation_date==dat.arb$observation_date[1],]

newdata=dat.now
# ------------------------------------------
