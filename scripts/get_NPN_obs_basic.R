# Read in Arb station codes
# Data frame if we want to pull individual sites
stat.arb <- read.csv("../data/NPN/NPN_TheMortonArboretum_Stations_All.csv")

id.arb <- 720 # Our USA-NPN network ID

dat.npn <- rnpn::npn_download_status_data(network_ids=id.arb, years=2022, request_source="C. Rollinson, Morton Arb")
summary(dat.npn)
nrow(dat.npn) #Number of observations (counts each phenophase separately)
