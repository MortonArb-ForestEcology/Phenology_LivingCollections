# Annual report (rough draft) for NPN Local Leaders final project
# Load some useful packages; (if you don't have these installed, you'll need to do so)
library(ggplot2); library(googlesheets); 

# For the npn_get_obs function:
library(raster); library(httr); library(jsonlite)

# ----------------------------
# 1. Grabbing our raw data
# ----------------------------
# # NOTE: The first time you run things, you're going to have to grant google sheets permission to 
#         access your Google Drive account. An Authentication token (.httr-oauth) will be stored 
#         in the working directory.  You should only need to do this once as long as you use the same
#         working directory.  Make sure this token does not get pushed to github etc.
# "Register" the sheet we want to work with (load the metadata so we can interact with it)
pheno.lc <- gs_title("Observations_2017_LC_Oaks_TreeShrub")
pheno.lc

# get the data from a particular sheet
dat.lc <- data.frame(gs_read(pheno.lc, ws="Observations_LC_Oaks_2017"))

# Setting things to factors
for(i in 1:ncol(dat.lc)){
  if(class(dat.lc[,i])=="character") dat.lc[,i] <- as.factor(dat.lc[,i])
}
summary(dat.lc)

# Lets get rid of our random Acer
dat.lc <- dat.lc[dat.lc$genus!="Acer",]
summary(dat.lc)
# ----------------------------

# ----------------------------
# Generate some summary numbers for our phenology report
# ----------------------------
# Get some summary stats on our number of trees and species
nrow(dat.lc) # Number of observations (not counting individual phenophases)
range(dat.lc$date_observed) # Date range (note: incomplete at moment)
length(unique(dat.lc$id)) # Number of individuals observed (assuming no typos)
length(unique(dat.lc$species)) # Number of species observed (assuming no typos)
length(unique(dat.lc[dat.lc$data_entered_NPN=="Y", "species"])) # Species in NPN database
length(unique(dat.lc[dat.lc$data_entered_NPN=="Y", "id"])) # Species in NPN database


# Find the earliest bud burst and first color by individual tree
budburst <- aggregate(dat.lc[dat.lc$leaf_breaking_bud_observed=="Y", "date_observed"], 
                      by=dat.lc[dat.lc$leaf_breaking_bud_observed=="Y", c("genus", "species", "id")], 
                      FUN=min)
names(budburst)[which(names(budburst)=="x")] <- "date_observed"
budburst$type <- "first"
summary(budburst)

budburst2 <- aggregate(dat.lc[dat.lc$leaf_breaking_bud_observed=="Y", "date_observed"], 
                       by=dat.lc[dat.lc$leaf_breaking_bud_observed=="Y", c("genus", "species", "id")], 
                       FUN=mean)
names(budburst2)[which(names(budburst2)=="x")] <- "date_observed"
budburst2$type <- "mean"
summary(budburst2)

budburst <- rbind(budburst, budburst2)

png("Budburst_First_2017.png", height=4, width=7, units="in", res=120)
ggplot(data=budburst[budburst$type=="first",]) +
  ggtitle("Date of First Budburst") +
  geom_histogram(aes(x=date_observed, fill=species), binwidth=7) +
  theme_bw() +
  # scale_fill_discrete(colors=brewer.pal(length(unique(budburst$species)),"Set3")) +
  theme(legend.position="bottom")
dev.off()


png("Budburst_2017.png", height=4, width=7, units="in", res=120)
ggplot(data=budburst) +
  ggtitle("Date of Budburst") +
  facet_grid(type~.) +
  geom_histogram(aes(x=date_observed, fill=species), binwidth=7) +
  theme_bw() +
  # scale_fill_discrete(colors=brewer.pal(length(unique(budburst$species)),"Set3")) +
  theme(legend.position="bottom")
dev.off()

# Comparing first and mean budburst date
mean(budburst[budburst$type=="first", "date_observed"]); sd(budburst[budburst$type=="first", "date_observed"])
mean(budburst[budburst$type=="mean", "date_observed"]); sd(budburst[budburst$type=="mean", "date_observed"])
# ----------------------------

# ----------------------------
# Pull Nature's Notebook data to compare our data to
# ----------------------------
# Path to my NPN data portal function(s); 
#  - these can be found here: https://github.com/MortonArb-ForestEcology/NPN_Data_Utils
path.npn <- "~/Desktop/Research/NPN_Data_Utils/R/"
source(file.path(path.npn, "npn_get_obs.R")) #Get raw individual-level NPN data

summary(dat.lc[dat.lc$data_entered_NPN=="Y", "species"])

summary(as.factor(midwest$state))
midwest <- c("IL", "IN", "MI", "OH", "WI", "MN", "IA", "MO")

npn.oaks <- npn.getObs(species=c("Quercus alba", "Quercus macrocarpa", "Quercus rubra", "Quercus velutina"), 
                       state=midwest,
                       start_date="2017-01-01", end_date="2017-11-30", 
                       request_src="NPN_LocalLeaders2017")
npn.oaks$phenophase_status <- as.factor(npn.oaks$phenophase_status)
summary(npn.oaks)

# Change -9999 to NA


# Subset just to get breaking buds
npn.bb <- npn.oaks[npn.oaks$phenophase_description=="Breaking leaf buds",]
summary(npn.bb)

# -------
# Getting the fraction of the individuals showing budburst by week
# -------
# 1. Aggregate the observation date to a week
date.seq <- seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by=7)

budburst.master <- data.frame(week=date.seq, species=rep(unique(npn.bb$species), each=length(date.seq)))
summary(budburst.master)

# Creating Columns of fraction showing budburst for each nature's notebook and Morton Arb
for(i in 1:nrow(budburst.master)){
  spp.now <- budburst.master$species[i]
  wk <- budburst.master$week[i]
  
  npn.sub <- npn.bb[npn.bb$species==spp.now & npn.bb$observation_date>=wk & npn.bb$observation_date<=wk+6,]
  if(nrow(npn.sub)>0){
    budburst.master[i, "NPN"] <- length(which(npn.sub$phenophase_status==1))/nrow(npn.sub)
  } 

  ma.sub <- dat.lc[paste(dat.lc$species)==paste(spp.now) & dat.lc$date_observed>=wk & dat.lc$date_observed<=wk+6,]
  if(nrow(ma.sub)>0){
    budburst.master[i, "MortonArb"] <- length(which(ma.sub$leaf_breaking_bud_observed=="Y"))/nrow(ma.sub)
  }
}
summary(budburst.master)

png("Budburst_MA_vs_NN_2017.png", height=6, width=10, units="in", res=220)
ggplot(data=budburst.master[budburst.master$week>= as.Date("2017-03-01") & 
                              budburst.master$week <= as.Date("2017-07-01") &
                              budburst.master$species!="velutina",]) +
  facet_grid(species~.) +
  geom_line(aes(x=week, y=NPN, color="NN Midwest"), size=2) +
  geom_line(aes(x=week, y=MortonArb, color="Morton Arb"), size=1.5) +
  scale_color_manual(name="Dataset", values=c("red2", "black")) +
  # coord_cartesian(ylim = c(as.Date("2017-03-01"), as.Date("2017-07-01"))) +
  labs(y="Fraction Showing Budburst") +
  theme_bw() +
  theme(legend.position="top")
dev.off()
# -------

# ----------------------------
