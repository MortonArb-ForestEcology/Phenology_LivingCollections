# -------------------------------------------------------------
# Checking weekly phenology observation data
# -------------------------------------------------------------


# -------------------------------------------------------------
# Set file paths, load libraries etc.
# -------------------------------------------------------------
library(googlesheets)
library(raster); library(rgdal); library(rgeos) # spatial analysis packages
library(ggplot2); library(grid) # graphing packages
library(lubridate)

# Source my cleaning function
source("clean_google_form.R")

dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
path.figs <- "/Volumes/GoogleDrive/My Drive/Conferences_Presentations/ESA 2019 Louisville/Phenology"
if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)
# setwd(dir.base)

path.dat <- file.path(dir.base, "Observing Lists/2018_Quercus")
maps.out <- file.path(path.dat)
# path.gis <- "/Volumes/GIS/Collections" # Path on a Mac
# path.gis <- "Y:/Collections" # Path on a PC
# -------------------------------------------------------------


# -------------------------------------------------------------
# Access & format the observations
# -------------------------------------------------------------
# get the data from a particular sheet
quercus <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Quercus", dat.yr=lubridate::year(Sys.Date()))
quercus$Collection <- as.factor("Quercus")
quercus$Year <- lubridate::year(quercus$Date.Observed)
summary(quercus)

acer <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Acer", dat.yr=lubridate::year(Sys.Date()))
acer$Collection <- as.factor("Acer")
acer$Year <- lubridate::year(acer$Date.Observed)
summary(acer)

dat.2019 <- rbind(quercus, acer)
summary(dat.2019)

quercus.2018 <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Quercus", dat.yr=2018)
quercus.2018$Collection <- as.factor("Quercus")
# quercus.2018
quercus.2018$Year <- lubridate::year(quercus.2018$Date.Observed)
names(quercus.2018) <- names(quercus)
summary(quercus.2018)

# quercus.all <- rbind(quercus.2018, quercus)
# quercus.all$Year <- lubridate::year(quercus.all$Date.Observed)
# summary(quercus.all)

dat.all <- rbind(dat.2019, quercus.2018)
dat.all$fruit.drop.intensity <- as.factor(dat.all$fruit.drop.intensity)
dat.all <- dat.all[dat.all$Observer!="TEST", ]
summary(dat.all)

phenophase.obs <- names(dat.all)[grep(".observed", names(dat.all))] 
for(PHENO in phenophase.obs){
  dat.all[is.na(dat.all[,PHENO]),PHENO] <- "Did not look for"
  dat.all[,PHENO] <- factor(dat.all[,PHENO], levels=c("No", "Yes", "?", "Did not look for"))
}
summary(dat.all)

dat.all$Observer <- factor(dat.all$Observer, levels=c(sort(paste(unique(dat.all$Observer))[!unique(dat.all$Observer) %in% c("Rollinson", "Reidy")]), "Rollinson", "Reidy"))
#----------------------------


#----------------------------
# Summarizing the first events
#----------------------------
# getting some some date of first events
first.event <- aggregate(dat.all[dat.all$leaf.present.observed=="Yes", "Date.Observed"],
                         by=dat.all[dat.all$leaf.present.observed=="Yes",c("Year", "Collection", "Species", "PlantNumber")],
                         FUN=min, na.rm=T)
first.event$x <- lubridate::yday(first.event$x)
names(first.event)[names(first.event)=="x"] <- "leaf.present.observed"
summary(first.event)

# ggplot(data=first.event) +
#   facet_wrap(~Year) +
#   geom_histogram(aes(x=x, fill=Collection)) 

for(PHENO in phenophase.obs){
  if(nrow(dat.all[dat.all[,PHENO]=="Yes",])==0) next
  
  # Need to store it as a temporary data frame because some trees won't have particular phenophases
  dat.tmp <- aggregate(dat.all[dat.all[,PHENO]=="Yes", "Date.Observed"],
                       by=dat.all[dat.all[,PHENO]=="Yes",c("Year", "Collection", "Species", "PlantNumber")],
                       FUN=min, na.rm=T)
  dat.tmp$x <- lubridate::yday(dat.tmp$x)
  names(dat.tmp)[names(dat.tmp)=="x"] <- PHENO
  
  first.event <- merge(first.event, dat.tmp, all=T)
  
}
first.event$Collection <- as.factor(first.event$Collection)
summary(first.event)

yrs.mark <- data.frame(Label=c("March 1", "Apr 1", "May 1", "Jun 1", "Jul 1"), 
                       Date=c("2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01", "2019-07-01"))
yrs.mark$mark.yday <- lubridate::yday(yrs.mark$Date)
# yrs.mark$mark.yday[1] <- yrs.mark$mark.yday[1]-365


png(file.path(path.figs, "Budburst_Comparison_Quercus_Acer_2018-2019.png"), height=10.5, width=9, units="in", res=120)
ggplot(data=first.event) +
  facet_grid(Collection~., scales="fixed") +
  geom_density(aes(x=leaf.buds.observed, fill=as.factor(Year)), adjust=1.5, alpha=0.5) +
  scale_y_continuous(name="Density", expand=c(0,0), breaks=seq(0.02, 0.12, by=0.02)) +
  scale_x_continuous(name="Day of First Budburst", expand=c(0,0), breaks=yrs.mark$mark.yday, label=yrs.mark$Label) +
  scale_fill_manual(name="Year", values=c("blue2", "green4")) +
  theme(panel.background=element_rect(fill=NA, color="black", size=0.5),
        panel.grid = element_blank(),
        # panel.margin=unit(0, "lines"),
        axis.text=element_text(size=rel(3)),
        axis.title=element_text(size=rel(3.5), face="bold"),
        legend.position=c(0.8, 0.85),
        legend.title = element_text(face="bold", size=rel(3)),
        legend.text=element_text(size=rel(3)),
        legend.key.size=unit(3, "lines"),
        strip.text=element_text(face="bold", size=rel(3.5)))
dev.off()

ggplot(data=first.event) +
  facet_grid(Collection~.) +
  geom_density(aes(x=leaf.present.observed, fill=as.factor(Year)), adjust=1.5, alpha=0.5)

ggplot(data=first.event) +
  facet_grid(Collection~.) +
  geom_density(aes(x=leaf.increasing.observed, fill=as.factor(Year)), adjust=1.5, alpha=0.5)

#----------------------------


#----------------------------
# Past & future Bloom Times
#----------------------------
path.met <- "/Volumes/GoogleDrive/My Drive/Arboretum Met Data/GHCN-Daily"
# path.met <- "datasets" 

# Read in the older dataset
met.old <- read.csv(file.path(path.met, "MortonArb_GHCND-USC00119221_1895-2007.csv"))
met.old$DATE <- as.Date(met.old$DATE) # Convert this column to a special datatype that's recognized as a date
summary(met.old)

# Read in the newer dataset
met.new <- read.csv(file.path(path.met, "MortonArb_GHCND-USC00115097_2007-04-01_2019-05-15.csv"))
met.new$DATE <- as.Date(met.new$DATE) # Convert this column to a special datatype that's recognized as a date
summary(met.new)

# Check to make sure we're in metric (earlier I had a mixture of units and that was bad)
range(met.old$TMAX, na.rm=T)
range(met.new$TMAX, na.rm=T)

# Combine the old and the new datasets into a new data frame.  We don't want all columns, so just take the ones we care about
met.all <- rbind(met.old[,c("STATION", "DATE", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")],
                 met.new[,c("STATION", "DATE", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")])
met.all$YEAR <- lubridate::year(met.all$DATE)
met.all$MONTH <- lubridate::month(met.all$DATE)
met.all$DAY <- lubridate::day(met.all$DATE)
met.all$YDAY <- lubridate::yday(met.all$DATE)
met.all <- met.all[met.all$YEAR>1895 & met.all$YEAR<2019,]
met.all$TMEAN <- (met.all$TMAX + met.all$TMIN)/2
summary(met.all)

# Adding in growing degree-days with base temp of 5
met.all$GDD5 <- ifelse(met.all$TMEAN>5, met.all$TMEAN-5, 0)
met.all$GDD5.cum <- NA
summary(met.all)

# Calculate the running growing degree days for each day/year
for(YR in unique(met.all$YEAR)){
  dat.tmp <- met.all[met.all$YEAR==YR, ]
  
  if(min(dat.tmp$DATE)>as.Date(paste0(YR, "-01-01"))) next
  
  gdd.cum=0
  d.miss = 0
  for(i in 1:nrow(dat.tmp)){
    if(is.na(dat.tmp$GDD5[i]) & d.miss<=3){
      d.miss <- d.miss+1 # Let us miss up to 3 consecutive days
      gdd.cum <- gdd.cum+0
    } else {
      d.miss = 0 # reset to 0
      gdd.cum <- gdd.cum+dat.tmp$GDD5[i] 
    }
    
    dat.tmp[i,"GDD5.cum"] <- gdd.cum
  }
  met.all[met.all$YEAR==YR, "GDD5.cum"] <- dat.tmp$GDD5.cum
}
summary(met.all)


model.list <- c("GFDL-ESM2M", "GFDL-ESM2G", "bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM", "CanESM2", "CNRM-CM5", "CSIRO-Mk3-6-0", "HadGEM2-ES", "HadGEM2-CC", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC5", "MIROC-ESM", "MIROC-ESM-CHEM", "MRI-CGCM3", "NorESM1-M")
scenario.list <- c("rcp45", "rcp85")

sec2day <- 60*60*24
dat.gcm <- data.frame()
pb <- txtProgressBar(min=0, max=length(model.list)*length(scenario.list), style=3)
pb.ind <- 0
for(model in model.list){
  dat.mod <- data.frame()
  for(scenario in scenario.list){
    pb.ind <- pb.ind+1
    setTxtProgressBar(pb, pb.ind)
    
    outfolder = file.path(path.met, "../GCM_future_Projections/MACAv2_raw", model, scenario)
    if(!dir.exists(outfolder)) next
    
    flist <- dir(outfolder, ".nc")
    for(i in 1:length(flist)){
      yr.now <- as.numeric(stringr::str_split(flist[i], "[.]")[[1]][5])
      is.leap <- lubridate::leap_year(yr.now)
      
      ncT <- ncdf4::nc_open(file.path(outfolder, flist[i]))
      summary(ncT$var)
      dat.tmp <- data.frame(model=model, scenario=scenario, YEAR=yr.now, 
                            TMAX=ncdf4::ncvar_get(ncT, "air_temperature_max")-273.15,
                            TMIN=ncdf4::ncvar_get(ncT, "air_temperature_min")-273.15,
                            PRCP=ncdf4::ncvar_get(ncT, "precipitation_flux")*sec2day)
      dat.tmp$YDAY <- 1:nrow(dat.tmp)
      
      if(is.leap){
        dat.tmp[dat.tmp$YDAY>(31+28),"YDAY"] <- dat.tmp[dat.tmp$YDAY>(31+28),"YDAY"]+1
      }
      # summary(dat.tmp)
      dat.tmp$TMEAN <- apply(dat.tmp[,c("TMAX", "TMIN")], 1, mean)
      dat.tmp$GDD5 <- ifelse(dat.tmp$TMEAN>5, dat.tmp$TMEAN-5, 0)
      dat.tmp$GDD5.cum <- NA
      dat.tmp$GDD5.cum[1] <- dat.tmp$GDD5[1]
      for(j in 2:nrow(dat.tmp)){
        dat.tmp[j,"GDD5.cum"] <- dat.tmp[j-1,"GDD5.cum"] + dat.tmp$GDD5[j]
      }
      
      
      ncdf4::nc_close(ncT)
      
      dat.mod <- rbind(dat.mod, dat.tmp)
    } # End i file loop
  } # End scenario loop
  dat.gcm <- rbind(dat.gcm, dat.mod)
} # End model loop
dat.gcm$DATE <- as.Date(paste(dat.gcm$YEAR, dat.gcm$YDAY, sep="-"), format="%Y-%j")
summary(dat.gcm)
summary(dat.gcm[dat.gcm$YEAR==2008,])



dat.bloom <- as.data.frame(readxl::read_excel("/Volumes/GoogleDrive/My Drive/Misc/ClimateChange_MortonArb/datasets/BRAHMS-FULL-BLOOM-EXTRACT_2019-04-15.xlsx", sheet="Sheet1"))
dat.bloom <- dat.bloom[,1:8]
dat.bloom$Date <- as.Date(dat.bloom$Date)
dat.bloom$year <- lubridate::year(dat.bloom$Date)
dat.bloom$yday <- lubridate::yday(dat.bloom$Date)
dat.bloom$PlantID <- as.factor(dat.bloom$PlantID)
dat.bloom$FullName <- as.factor(dat.bloom$FullName)
dat.bloom$Collection <- as.factor(dat.bloom$Collection)
dat.bloom$FlowerStage <- as.factor(dat.bloom$Collection)
dat.bloom <- dat.bloom[!is.na(dat.bloom$Date),]
dat.bloom <- dat.bloom[grep("Cercis canadensis", dat.bloom$FullName),]
summary(dat.bloom)

# Merge in cumulative growing degree-days from the met data
dat.bloom$GDD5.cum <- NA
for(DAT in unique(paste(dat.bloom$Date))){
  if(length(met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"])==0) next
  dat.bloom[dat.bloom$Date==as.Date(DAT),"GDD5.cum"] <- met.all[met.all$DATE==as.Date(DAT), "GDD5.cum"]
}

summary(dat.bloom[dat.bloom$GDD5.cum<1e3,])
hist(dat.bloom$GDD5.cum)


ceca.gdd5.lme <- nlme::lme(GDD5.cum ~ yday, random=list(PlantID=~1), data=dat.bloom[,], na.action=na.omit)
summary(ceca.gdd5.lme)

dat.bloom$gdd.pred[!is.na(dat.bloom$GDD5.cum)] <- predict(ceca.gdd5.lme)
summary(dat.bloom)
plot(gdd.pred ~ GDD5.cum, data=dat.bloom)

# ----------------------
# Predict past bloom
# ----------------------
# Start with creating a data frame
dat.past <- aggregate(met.all[,c("TMAX", "TMIN", "TMEAN", "PRCP", "SNOW")],
                    by=met.all[,c("STATION", "YEAR")],
                    FUN=mean, na.rm=T)

# We have some periods missing data, but for our own sanity, we want all years represented,
#    so we make a dummy data frame with the years that we *should* have
yr.dummy <- data.frame(STATION=c(rep("USC00119221", length(1896:2007)),
                                 rep("USC00115097", length(2007:2018))),
                       YEAR=c(1896:2007, 2007:2018))

# Now lets merge that dummy data frame into the one with actual data so we have placeholders
dat.past <- merge(dat.past, yr.dummy, all=T)
dat.past <- dat.past[dat.past$YEAR>=1922,] # We're missing a lot of data, so lets just cut things to since the Arb's founding
summary(dat.past)

ceca.gdd5.mean <- mean(dat.bloom[dat.bloom$FullName=="Cercis canadensis L.","GDD5.cum"], na.rm=T); 
ceca.gdd5.sd <- sd(dat.bloom[dat.bloom$FullName=="Cercis canadensis L.","GDD5.cum"], na.rm=T)

dat.past$bloom.CECA <- NA
# dat.past$freeze.last <- NA
for(i in 1:nrow(dat.past)){
  YR=dat.past$YEAR[i]
  dat.tmp <- met.all[met.all$YEAR==YR, ]
  # summary(dat.tmp)
  if(nrow(dat.tmp)==0) next
  
  # # Killing Freeze (28˚F per K. Bachtell) (28-32)*5/9 = -2.2C
  # freeze.last <- max(dat.tmp[which(dat.tmp$TMIN<=-2 & dat.tmp$YDAY<=180), "YDAY"])
  # if(freeze.last != -Inf) dat.past[i,"freeze.last"] <- freeze.last
  
  # Bloom time
  bloom.pred <- min(dat.tmp[which(dat.tmp$GDD5.cum >= ceca.gdd5.mean), "YDAY"])
  if(bloom.pred != Inf) dat.past[i,"bloom.CECA"] <- bloom.pred
  
}
summary(dat.past)

summary(dat.gcm)
dat.fut <- aggregate(dat.gcm[,c("TMEAN", "PRCP", "GDD5.cum")],
                     by=dat.gcm[,c("model", "scenario", "YEAR")],
                     FUN=mean)
summary(dat.fut)

pb <- txtProgressBar(min=0, max=nrow(dat.fut), style=3)
for(i in 1:nrow(dat.fut)){
  setTxtProgressBar(pb, i)
  YR=dat.fut$YEAR[i]
  GCM = dat.fut$model[i]
  SCEN = dat.fut$scenario[i]
  dat.tmp <- dat.gcm[dat.gcm$YEAR==YR & dat.gcm$model==GCM & dat.gcm$scenario==SCEN, ]
  # summary(dat.tmp)
  if(nrow(dat.tmp)==0) next
  
  # # Killing Freeze (28˚F per K. Bachtell) (28-32)*5/9 = -2.2C
  # freeze.last <- max(dat.tmp[which(dat.tmp$TMIN<=-2 & dat.tmp$YDAY<=180), "YDAY"])
  # if(freeze.last != -Inf) dat.past[i,"freeze.last"] <- freeze.last
  
  # Bloom time
  bloom.pred <- min(dat.tmp[which(dat.tmp$GDD5.cum >= ceca.gdd5.mean), "YDAY"])
  if(bloom.pred != Inf) dat.fut[i,"bloom.CECA"] <- bloom.pred
  
}
summary(dat.fut)

write.csv(dat.past, "CECA_Bloom_Past.csv", row.names=F)
write.csv(dat.fut, "CECA_Bloom_Future.csv", row.names=F)
# ----------------------

# ----------------------
# Summarizing the future bloom predictions
# ----------------------
ceca.bloom <- aggregate(dat.bloom[,c("yday", "GDD5.cum")],
                        by=list(dat.bloom[,c("year")]),
                        FUN=mean, na.rm=T)
names(ceca.bloom)[1] <- "year"
ceca.bloom[,c("yday.sd", "GDD5.cum.sd")]  <- aggregate(dat.bloom[,c("yday", "GDD5.cum")],
                                                       by=list(dat.bloom[,c( "year")]),
                                                       FUN=sd, na.rm=T)[,c("yday", "GDD5.cum")]
ceca.bloom$type="Past"
ceca.bloom$type <- factor(ceca.bloom$type, levels=c("Past", "Future"))
summary(ceca.bloom)


bloom.fut <- aggregate(dat.fut$bloom.CECA,
                       by=dat.fut[,c("scenario", "YEAR")],
                       FUN=mean)
names(bloom.fut)[names(bloom.fut)=="x"] <- "bloom.mean"
bloom.fut$bloom.lo <- aggregate(dat.fut$bloom.CECA,
                                by=dat.fut[,c("scenario", "YEAR")],
                                FUN=quantile, 0.025)[,"x"]
bloom.fut$bloom.hi <- aggregate(dat.fut$bloom.CECA,
                                by=dat.fut[,c("scenario", "YEAR")],
                                FUN=quantile, 0.975)[,"x"]
summary(bloom.fut)


yrs.mark <- data.frame(Label=c("March 1", "Apr 1", "May 1", "Jun 1", "Jul 1"), 
                       Date=c("2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01", "2019-07-01"))
yrs.mark$mark.yday <- lubridate::yday(yrs.mark$Date)

bloom.both <- data.frame(type=c(rep("Past", nrow(dat.past)), rep("Future", nrow(bloom.fut))),
                         scenario=as.factor(c(rep("historical", nrow(dat.past)), paste(bloom.fut$scenario))),
                         year=c(dat.past$YEAR, bloom.fut$YEAR),
                         bloom.mean = c(dat.past$bloom.CECA, bloom.fut$bloom.mean),
                         bloom.lo = c(rep(NA, nrow(dat.past)), bloom.fut$bloom.lo),
                         bloom.hi = c(rep(NA, nrow(dat.past)), bloom.fut$bloom.hi))
bloom.both$type <- factor(bloom.both$type, levels=c("Past", "Future"))
summary(bloom.both)

library(ggplot2)
png(file.path(path.figs, "Redbud_Bloom_Modeled_Past_Future.png"), height=12, width=12, units="in", res=120)
ggplot(data=bloom.both) +
  facet_wrap(~type, scales="free_x", ncol=1) +
  geom_ribbon(aes(x=year, ymin=bloom.lo, ymax=bloom.hi, fill=scenario), alpha=0.5) + 
  geom_line(aes(x=year, y=bloom.mean, color=scenario), size=2) +
  geom_pointrange(data=ceca.bloom, aes(x=year, y=yday, ymin=yday-yday.sd, ymax=yday+yday.sd), size=1.5, color="green4") +
  geom_hline(yintercept = mean(dat.bloom$yday), linetype="dashed", color="gray30") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(name="Day of Peak Bloom", breaks=yrs.mark$mark.yday, label=yrs.mark$Label) +
  scale_color_manual(values=c("black", "blue2", "red3")) +
  scale_fill_manual(values=c("black", "blue2", "red3")) +
  theme(panel.background=element_rect(fill=NA, color="black", size=0.5),
        panel.grid = element_blank(),
        # panel.margin=unit(0, "lines"),
        axis.text=element_text(color="black", size=rel(3)),
        axis.title=element_text(color="black", size=rel(3.5), face="bold"),
        legend.position="top",
        legend.title = element_text(face="bold", size=rel(3)),
        legend.text=element_text(size=rel(3)),
        legend.key.size=unit(3, "lines"),
        strip.text=element_blank())
 dev.off()


# ----------------------

#----------------------------
