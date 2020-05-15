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
# Read in NPN-level metadata
# ------------------------------------------

# Using get All stations to get data Christy has access to
# Arb network ID = 720
source("../../NPN_Data_Utils/R/npn_get_stations.R")
stat.arb <- npn.getStations(network_ids=720)
stat.arb
write.csv(stat.arb, "../data/NPN/NPN_TheMortonArboretum_Stations_All.csv", row.names=F)

# merge in our NPN data
file.inds <- "../data/NPN/NPN_MortonArb_Individuals_All.csv"
UPDATE=T # Can choose if we want to update or not
if(file.exists(file.inds) & !UPDATE){
  arb.inds <- read.csv(file.inds)
} else {
  source("../../NPN_Data_Utils/R/npn_getIndAtStation.R")
  
  arb.inds <- data.frame()
  for(i in 1:nrow(stat.arb)){
    coll.inds <- npn.getIndAtStation(station_ids = stat.arb$station_id[i])
    coll.inds$station_id <- stat.arb$station_id[i]
    coll.inds$station_name <- stat.arb$station_name[i]
    
    arb.inds <- rbind(arb.inds, coll.inds)
  }
  arb.inds <- arb.inds[!is.na(arb.inds$individual_id),]
  write.csv(arb.inds, "../data/NPN/NPN_MortonArb_Individuals_All.csv", row.names=F)
}
for(i in 1:ncol(arb.inds)){
  arb.inds[,i] <- as.factor(arb.inds[,i])
}

summary(arb.inds)
arb.inds$PlantNumber <- arb.inds$individual_name

# ------------------------------------------

# ------------------------------------------
# May need to move this up!
# Pull Morton Arb data (network ID = 720) & get rid of anything already there
# ------------------------------------------
# read in our column crosswalk
xwalk <- read.csv("../data/NPN/NPN_Phenophase_Crosswalk.csv")
xwalk

# Read in Arb station codes
stat.arb <- read.csv("../data/NPN/NPN_TheMortonArboretum_Stations_All.csv")

# read in arb.individuals 
arb.inds <- read.csv("../data/NPN/NPN_MortonArb_Individuals_All.csv", stringsAsFactors = T)
arb.inds$PlantNumber=arb.inds$individual_name
summary(arb.inds)

# ------------------------------------------


# ------------------------------------------
# 1. download our observations from Google
# ------------------------------------------
source("../../NPN_Data_Utils/R/npn_get_obs.R")
source("../../NPN_Data_Utils/R/npn_enter_observation.R")
source("clean_google_form.R")
npn_creds <- readLines("../data/NPN/.NPN_creds")
user_id <- strsplit(npn_creds[1], ":")[[1]][2]
user_pw <- strsplit(npn_creds[2], ":")[[1]][2]
# Loop through collections
sites.push <- c("Oak", "Maple", "Elm")
yrs.push <- c(2018:2020)
overwrite=T

for(SITE in sites.push){
  GENUS <- car::recode(SITE, "'Oak'='Quercus'; 'Maple'='Acer'; 'Elm'='Ulmus'")
  station_id <- stat.arb$station_id[grep(SITE, stat.arb$station_name)]
  
  if(SITE=="Maple") yrs.push <- yrs.push[yrs.push>=2019]
  if(SITE=="ELM") yrs.push <- yrs.push[yrs.push>=2020]
  
  for(YR in yrs.push){
    dat.now <- clean.google(collection = GENUS, dat.yr = YR)
    summary(dat.now)
    
    dat.now <- dat.now[dat.now$PlantNumber %in% arb.inds$individual_name, ]
    dim(dat.now)
    
    # Check to see if data has already been pushed
    # NOTE: NOT IMPLEMENTED YET!!!
    if(overwrite==F){
      # Download data for the station starting at Jan 1 for the year
      dat.npn <- npn.getObs(station_id = station_id, start_date=paste0(YR, "-01-01"), end_date=paste0(YR, "-12-31"))
      summary(dat.npn)
      
      # Check to remove data from dat.now where the dates line up
      # (this is the part that needs to be implemented)
      
    }
    if(nrow(dat.now)==0) next
    
    names(dat.now)[names(dat.now) %in% paste0(xwalk$MortonArb.Description, ".observed")]
    # paste0(xwalk$MortonArb.Description, ".observed")[!paste0(xwalk$MortonArb.Description, ".observed") %in% names(dat.now)]
    for(COL in names(dat.now)){
      if(COL %in% c("Timestamp", "Date.Observed")) next
      dat.now[,COL] <- as.character(dat.now[,COL])
    }
    
    
    dat.status <- stack(dat.now[,paste0(xwalk$MortonArb.Description, ".observed")])
    names(dat.status) <- c("status", "phenophase")
    dat.status$phenophase <- gsub(".observed", "", dat.status$phenophase)
    dat.status[,c("Observer", "Date.Observed", "Species", "PlantNumber", "Notes")] <- dat.now[,c("Observer", "Date.Observed", "Species", "PlantNumber", "Notes")]
    head(dat.status)
    
    dat.intensity <- stack(dat.now[,names(dat.now)[names(dat.now) %in% paste0(xwalk$MortonArb.Description, ".intensity")]])
    names(dat.intensity) <- c("intensity", "phenophase")
    dat.intensity$phenophase <- gsub(".intensity", "", dat.intensity$phenophase)
    dat.intensity[,c("Observer", "Date.Observed", "PlantNumber")] <- dat.now[,c("Observer", "Date.Observed", "PlantNumber")]
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
    
    # for(COL in names(dat.long)){
    #   if(COL %in% c("Date.Observed")) next
    #   dat.long[,COL] <- as.factor(dat.long[,COL])
    # }
    # head(dat.long)
    # summary(dat.long[dat.long$individual_id==dat.long$individual_id[1],])
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
    dim(dat.arb)
    
    # hist(dat.npn$observation_id)
    
    # Push data individual by individual and date by date
    print(paste0("Pushing data for ", SITE, ", ", YR, " (", nrow(dat.arb), " data points for ", nrow(dat.now), " observation sets)"))
    pb <- txtProgressBar(min=0, max=nrow(dat.now), style=3)
    pb.ind=0
    for(IND in unique(dat.arb$individual_id)){
      dat.ind <- dat.arb[dat.arb$individual_id==IND,]
      for(OBS in unique(paste(dat.ind$observation_date))){
        dat.obs <- dat.ind[dat.ind$observation_date==OBS,]
        
        resp <- npn.putObs(newdata=dat.obs, user_id=user_id, user_pw = user_pw)
        resp <- httr::content(resp, as="parsed")
        xml.chil <- xml2::xml_children(resp)
        if(xml2::xml_attrs(xml.chil[[1]])[["response_code"]]=="1"){
          # print("Observation Successfully Uploaded")
        } else {
          warning(paste0("Observation Not Uploaded: individual ", IND, " (", OBS, ")"))
          # Eventually change this so it gives more info
          # subchil <- xml2::xml_children(xml.chil[[1]])
          # subchil[["response_messages"]]
          
        }
        pb.ind=pb.ind+1
        setTxtProgressBar(pb, pb.ind)
      } # End observation loop
    } # End individual loop
    
    
  } # end YR loop
  
} # End collections loop


names(dat.now)
# ------------------------------------------

# ------------------------------------------
# 2. Format to match NPN data set
# Station IDs
# Oak Collection
# Maple Collection
# Schulenberg Prairie = 
# ------------------------------------------


# ------------------------------------------



# ------------------------------------------
# Loop through and upload
# ------------------------------------------
dat.now <- dat.arb[dat.arb$individual_id==dat.arb$individual_id[1] & dat.arb$observation_date==dat.arb$observation_date[1],]

newdata=dat.now
# ------------------------------------------
