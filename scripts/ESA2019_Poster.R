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
