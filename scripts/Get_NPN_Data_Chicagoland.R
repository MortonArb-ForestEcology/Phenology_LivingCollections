# Getting NPN data for the whole Chicago Region

#----------------------------------------------------------
# Getting all observations for specific people that are associated with The Morton Arboretum, but aren't observing on site
#----------------------------------------------------------
# -----------------------------------
# -----------------------------------
# 1. Load a shapefile for IL counties to just do the 7-county region
# 2. Get observations for all phenology observations in our region
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
# -----------------------------------
#----------------------------------------------------------
library(ggplot2)
path.google <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology"

# -----------------------------------
# 1. Get county boundaries to jsut do the 7-county region
# -----------------------------------
library(rgdal)
chicago.co <- c("COOK", "LAKE", "DUPAGE", "WILL", "MCHENRY", "KANE", "KENDALL")

county <- readOGR("../data/spatial/IL_BNDY_County/IL_BNDY_County_Py.shp")
summary(county)

chicagoland <- county[county$COUNTY_NAM %in% chicago.co,]
plot(chicagoland)
# -----------------------------------

# -----------------------------------
# 2. Get observations of our people's sites
# -----------------------------------
source("../../NPN_Data_Utils/R/npn_get_obs.R")

# Read in what data we already have
obs.all <- read.csv("../data/NPN/NPN_Chicago_Observations_all.csv")

# If our data is potentially out of date, update it
if(max(obs.all$observation_date)>Sys.Date()-1){
  # Note this step can be quite slow!
  # Note: if you get errors, what you're trying to do is too large
  # 1. Go by County
  # 2. narrow it down to plants? Deciduous, Evergreen, Forb
  obs.new <- data.frame()
  # WILL, KENDALL -- No Observations
  # COOK -- needs to be split
  for(CO in unique(chicagoland$COUNTY_NAM)){
    # Need to split some counties in 2 because there's just too much
    if(CO=="COOK"){
      cook.sp <- chicagoland[chicagoland$COUNTY_NAM == CO,]
      ext.cook <- raster::extent(cook.sp)
      lat.brks <- seq(ext.cook[3], ext.cook[4], length.out=5)
      
      cook.split <- list()
      for(i in 1:(length(lat.brks)-1)){
        cook.split[[i]] <- raster::crop(cook.sp, raster::extent(ext.cook[1:2], lat.brks[i], lat.brks[i+1]))
      }
      
      
      obs.co <- data.frame() 
      for(i in 1:length(cook.split)){
        dat.tmp <- npn.getObs(region=cook.split[[i]], start_date = max(obs.all$observation_date))
        if(nrow(dat.tmp)==0) next
        obs.co <- rbind(obs.co, dat.tmp)
      }
      
    } else {
      obs.co <- npn.getObs(region=chicagoland[chicagoland$COUNTY_NAM == CO,], start_date = max(obs.all$observation_date))
      # summary(obs.co)
      
    }
    if(nrow(obs.co)==0) next
    obs.co$County <- as.factor(CO)
    obs.new <- rbind(obs.new, obs.co)
    
  }
  
  
  obs.new[obs.new==-9999] <- NA
  obs.new$phenophase_status <- car::recode(obs.new$phenophase_status, "'0'='no'; '1'='yes'; '-1'='uncertain'")
  obs.new$phenophase_status <- factor(obs.new$phenophase_status, levels=c("no", "yes", "uncertain"))
  summary(obs.new)
  
  # Make sure we're not adding duplicates
  obs.new <- obs.new[!obs.new$observation_id %in% obs.all$observation_id,]
  
  obs.all <- rbind(obs.all, obs.new)
  
  write.csv(obs.all, "../data/NPN/NPN_Chicago_Observations_all.csv", row.names=F)
}
# -----------------------------------


# -----------------------------------
# Doing some graphing
# -----------------------------------
library(ggplot2)

obs.sites <- aggregate(obs.all[,c("latitude", "longitude")],
                       by=obs.all[,c("site_id", "kingdom", "County")],
                       FUN=mean)
summary(obs.sites)


png(file.path(path.google, "Observing_NPN/NPN_ChicagoLand_Sites.png"), height=6, width=6, units="in", res=180)
ggplot(data=obs.sites)+
  coord_equal() +
  geom_polygon(data=chicagoland, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_jitter(aes(x=longitude, y=latitude, color=kingdom), size=3, height=0.01, width=0.01, alpha=0.5) + 
  theme_minimal() +
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.position="top")
dev.off()

summary(obs.all)
obs.spp <- aggregate(obs.all[,c("latitude", "longitude")],
                     by=obs.all[,c("site_id", "kingdom", "County", "common_name")],
                     FUN=mean)
summary(obs.spp)

oak <- grep("oak", obs.spp$common_name)
maple <- grep("maple", obs.spp$common_name)
elm <- grep("elm", obs.spp$common_name)

png(file.path(path.google, "Observing_NPN/NPN_ChicagoLand_OaksMaplesElms.png"), height=6, width=6, units="in", res=180)
ggplot(data=obs.spp[c(oak, maple, elm),])+
  coord_equal() +
  geom_polygon(data=chicagoland, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_jitter(aes(x=longitude, y=latitude, color=common_name), size=3, height=0.01, width=0.01, alpha=0.5) + 
  theme_minimal() +
  theme(axis.text=element_blank(),
        axis.title=element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())
dev.off()
# -----------------------------------


# -----------------------------------
# Looking at phenology status
# -----------------------------------
obs.plant <- obs.all[obs.all$kingdom=="Plantae",]
summary(obs.plant)

obs.plant$phenophase_category <- as.factor(ifelse(grepl("leaf", obs.plant$phenophase_description) | grepl("leave", obs.plant$phenophase_description), "leaf", "flower"))

pheno.spring <- c("Breaking leaf buds", "Increasing leaf size", "Flowers or flower buds", "Open flowers", "Pollen release (flowers)")



png(file.path(path.google, "Observing_NPN/NPN_ChicagoLand_Phenology_Status_QuercusAcer.png"), height=6, width=8, units="in", res=180)
ggplot(data=obs.plant[obs.plant$genus %in% c("Acer", "Quercus") & obs.plant$phenophase_description %in% pheno.spring,]) +
  facet_grid(genus + species ~ phenophase_category + phenophase_description, scales="free") +
  geom_point(aes(x=observation_date, y=individual_id, color=phenophase_status), pch="|", size=5) +
  scale_color_manual(name="status", values=c("gray50", "green4", "lightblue")) +
  labs(title="Chicago Oak and Maple Phenology", x="Date") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_text(angle=-30, hjust=0),
        strip.text.y=element_text(face="italic", margin=unit(c(0,0,0.5,0), "lines")),
        legend.position="bottom")
dev.off()


summary(obs.plant)
obs.last <- aggregate(observation_date~ County + site_id + latitude + longitude + genus + species + common_name + individual_id + phenophase_category + phenophase_description, 
                      data=obs.plant, FUN=max)
summary(obs.last)

for(IND in unique(obs.last$individual_id)){
  for(PHENO in unique(obs.last$phenophase_description[obs.last$individual_id==IND])){
    date.obs <- obs.last$observation_date[obs.last$individual_id==IND & obs.last$phenophase_description==PHENO]
    obs.last$phenophase_status[obs.last$individual_id==IND & obs.last$phenophase_description==PHENO] <- paste(obs.plant$phenophase_status[obs.plant$observation_date==date.obs & obs.plant$individual_id==IND & obs.plant$phenophase_description==PHENO])
  }
}
obs.last$phenophase_status <- factor(obs.last$phenophase_status, levels=c("no", "yes", "uncertain"))
summary(obs.last)


png(file.path(path.google, "Observing_NPN/NPN_ChicagoLand_Phenology_Status_QuercusAcer_Map_latest.png"), height=6, width=8, units="in", res=180)
ggplot(data=obs.last[obs.last$genus %in% c("Acer", "Quercus") & obs.last$phenophase_description %in% pheno.spring,]) + 
  facet_grid(genus + species ~ phenophase_category + phenophase_description) +
  coord_equal() +
  geom_polygon(data=chicagoland, aes(x=long, y=lat, group=group), fill=NA, color="black") +
  geom_jitter(aes(x=longitude, y=latitude, color=phenophase_status), size=3, height=0.03, width=0.03, alpha=0.5) +
  scale_x_continuous(expand=c(0.25,0.25)) +
  labs(title="Chicago Oak and Maple Phenology (latest observation)") +
  scale_color_manual(name="status", values=c("gray50", "green4", "lightblue")) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text=element_blank(),
        axis.title=element_blank(),
        strip.text.y=element_text(face="italic", margin=unit(c(0,0,0.5,0), "lines")),
        legend.position="right",
        # panel.spacing.x = unit(4, "lines"),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank())
dev.off()
  
# -----------------------------------
