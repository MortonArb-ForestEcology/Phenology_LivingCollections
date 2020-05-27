# Pulling NPN data for the Arboretum to check/use
stat.arb <- read.csv("../data/NPN/NPN_TheMortonArboretum_Stations_All.csv")
stat.arb

dat.npn <- rnpn::npn_download_status_data(network_ids=720, years=2018:2020, request_source="C. Rollinson, Morton Arb")
dat.npn[dat.npn==-9999] <- NA
dat.npn$observation_date <- as.Date(dat.npn$observation_date)
dat.npn$update_datetime <- as.Date(dat.npn$update_datetime)
dat.npn$genus <- as.factor(dat.npn$genus)
dat.npn$species <- as.factor(dat.npn$species)
dat.npn$phenophase_description <- as.factor(dat.npn$phenophase_description)
dat.npn$intensity_value <- as.factor(dat.npn$intensity_value)
summary(dat.npn)

summary(dat.npn[!is.na(dat.npn$intensity_category_id),])

