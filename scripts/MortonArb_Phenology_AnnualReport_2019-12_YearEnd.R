# -------------------------------------------------------------
# Doing a year-end summary for Phenology MOnitoring at The Morton Arboretum
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
path.figs <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Reports/2019_02_EndOfYear_Report/figures_2019_end"
if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)
# setwd(dir.base)

path.met <- "/Volumes/GoogleDrive/My Drive/Arboretum Met Data/"
# path.gis <- "/Volumes/GIS/Collections" # Path on a Mac
# path.gis <- "Y:/Collections" # Path on a PC
# -------------------------------------------------------------

# -------------------------------------------------------------
# Summarize the weather data
# -------------------------------------------------------------
# Get the Arb station met data
met1 <- read.csv(file.path(path.met, "GHCN-Daily", "MortonArb_GHCND-USC00119221_1895-2007.csv"))
met2 <- read.csv(file.path(path.met, "GHCN-Daily", "MortonArb_GHCND-USC00115097_2007-04-01_2019-12-31.csv"))
summary(met1)
summary(met2)

# bind the data together
cols.bind <- c("STATION", "NAME", "LATITUDE", "LONGITUDE", "ELEVATION", "DATE", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN")
met.all <- rbind(met1[,cols.bind], met2[,cols.bind])
met.all$DATE <- as.Date(met.all$DATE)
summary(met.all)

# Add in missing dates
df.date <- data.frame(DATE=seq(min(met.all$DATE), max(met.all$DATE), by=1))
summary(df.date)
met.all <- merge(met.all, df.date, all=T)
met.all$YEAR <- lubridate::year(met.all$DATE)
met.all$MONTH <- lubridate::month(met.all$DATE)
met.all$DAY <- lubridate::day(met.all$DATE)
met.all$DOY <- lubridate::yday(met.all$DATE)
summary(met.all)

# A bit of data cleaning: 
# - The value of -55C occurs in summer and is VERY odd -- bad value!
met.all$TMIN[met.all$TMIN< -40] <- NA


# ----------------
# Do some graphs
# ----------------
yrs.mark <- data.frame(Label=c("Feb 1", "Apr 1", "Jun 1", "Aug 1", "Oct 1", "Dec 1"), 
                       Date=c("2019-02-01", "2019-04-01", "2019-06-01", "2019-08-01", "2019-10-01", "2019-12-01"))
yrs.mark$mark.yday <- lubridate::yday(yrs.mark$Date)


# Make some plots comparing 2019 to past years
png(file.path(path.figs, "Meteorology_Temperature_Min_2018_2019.png"), height=4, width=6, units="in", res=180)
ggplot(data=met.all, aes(x=DOY, y=TMIN, group=YEAR)) +
  geom_line(size=0.2, alpha=0.2) +
  geom_line(data=met.all[met.all$YEAR>=2018,], aes(color=as.factor(YEAR)), size=1, alpha=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels=yrs.mark$Label) +
  scale_y_continuous(name="Daily Min Temp (deg. C)") +
  scale_color_manual(name="Year", values = c("blue2", "green4")) +
  theme(panel.background = element_rect(color="black", fill=NA),
        legend.position=c(0.1,0.85),
        legend.box.background = element_blank(),
        legend.key = element_rect(fill=NA),
        legend.title=element_text(size=rel(1.25), face="bold"),
        legend.text=element_text(size=rel(1.25)),
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title = element_text(size=rel(1.25), face="bold"),
        axis.line = element_line(color="black"))
dev.off()

# Doing some quick cumulative stats to graph
met.all <- met.all[order(met.all$DATE),]
met.all$TMEAN <- (met.all$TMAX + met.all$TMIN)/2
met.all$GDD5 <- ifelse(met.all$TMEAN<=5, 0, met.all$TMEAN-5)
met.all[,c("PRCP.cum", "GDD5.cum")] <- NA
summary(met.all)

for(YR in unique(met.all$YEAR)){
  if(min(met.all[met.all$YEAR==YR & !is.na(met.all$TMAX),"DOY"]) >= 7 | length(which(is.na(met.all[met.all$YEAR==YR,"TMAX"])))>=30) next # Skip this year if we're missing too much data
  
  dat.sub <- met.all[met.all$YEAR==YR,]
  
  # Allow missing data to not contribute to the cumulative totals
  dat.sub$PRCP[is.na(dat.sub$PRCP)] <- 0
  dat.sub$GDD5[is.na(dat.sub$GDD5)] <- 0
  
  dat.sub[1,c("PRCP.cum", "GDD5.cum")] <- dat.sub[1,c("PRCP", "GDD5")]
  for(i in 2:nrow(dat.sub)){
    dat.sub[i,c("PRCP.cum", "GDD5.cum")] <- dat.sub[i-1,c("PRCP.cum", "GDD5.cum")] + dat.sub[i,c("PRCP", "GDD5")]
  }
  met.all[met.all$YEAR==YR,c("PRCP.cum", "GDD5.cum")] <- dat.sub[,c("PRCP.cum", "GDD5.cum")]
}


png(file.path(path.figs, "Meteorology_Precipitation_Cumulative_2018_2019.png"), height=4, width=6, units="in", res=180)
ggplot(data=met.all, aes(x=DOY, y=PRCP.cum, group=YEAR)) +
  geom_line(size=0.2, alpha=0.2) +
  geom_line(data=met.all[met.all$YEAR>=2018,], aes(color=as.factor(YEAR)), size=1, alpha=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels=yrs.mark$Label) +
  scale_y_continuous(name="Cumulative Precipitation (mm)", expand=c(0,0)) +
  scale_color_manual(name="Year", values = c("blue2", "green4")) +
  theme(panel.background = element_rect(color="black", fill=NA),
        legend.position=c(0.1,0.85),
        legend.box.background = element_blank(),
        legend.key = element_rect(fill=NA),
        legend.title=element_text(size=rel(1.25), face="bold"),
        legend.text=element_text(size=rel(1.25)),
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title = element_text(size=rel(1.25), face="bold"),
        axis.line = element_line(color="black"))
dev.off()

png(file.path(path.figs, "Meteorology_Temperature_GDD5_cumulative_2018_2019.png"), height=4, width=6, units="in", res=180)
ggplot(data=met.all, aes(x=DOY, y=GDD5.cum, group=YEAR)) +
  geom_line(size=0.2, alpha=0.2) +
  geom_line(data=met.all[met.all$YEAR>=2018,], aes(color=as.factor(YEAR)), size=1, alpha=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels=yrs.mark$Label) +
  scale_y_continuous(name="Cumulative Growing Degree-Days", expand=c(0,0)) +
  scale_color_manual(name="Year", values = c("blue2", "green4")) +
  theme(panel.background = element_rect(color="black", fill=NA),
        legend.position=c(0.1,0.85),
        legend.box.background = element_blank(),
        legend.key = element_rect(fill=NA),
        legend.title=element_text(size=rel(1.25), face="bold"),
        legend.text=element_text(size=rel(1.25)),
        axis.text = element_text(size=rel(1.25), color="black"),
        axis.title = element_text(size=rel(1.25), face="bold"),
        axis.line = element_line(color="black"))
dev.off()
# ----------------

# ----------------
# Doing some quick summary stats
# ----------------
# Finding some low stats
met.all[met.all$YEAR==2019 & !is.na(met.all$TMIN) & met.all$TMIN==min(met.all[met.all$YEAR==2019, "TMIN"], na.rm=T), c("DATE", "TMIN")]
met.all[met.all$YEAR==2019 & met.all$DOY>180 & !is.na(met.all$TMIN) & met.all$TMIN==min(met.all[met.all$YEAR==2019 & met.all$DOY>180, "TMIN"], na.rm=T), c("DATE", "TMIN")]

# Finding day of first/last hard freeze: 28 F; source: https://www.weather.gov/iwx/fallfrostinfo  
met.all[met.all$DATE==max(met.all[met.all$YEAR==2019 & met.all$DOY<180 & !is.na(met.all$TMIN) & met.all$TMIN < -2.22, c("DATE")]),]

met.all[met.all$DATE==min(met.all[met.all$YEAR==2019 & met.all$DOY>180 & !is.na(met.all$TMIN) & met.all$TMIN < -2.22, c("DATE")]),]

met.all[met.all$YEAR==2019 & !is.na(met.all$TMAX) & met.all$TMAX==max(met.all[met.all$YEAR==2019, "TMAX"], na.rm=T), c("DATE", "TMAX")]

# ----------------


# -------------------------------------------------------------

# -------------------------------------------------------------
# Access & format the observations
# -------------------------------------------------------------
# get the data from a particular sheet
quercus <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Quercus", dat.yr=2019)
quercus$Collection <- as.factor("Quercus")
quercus$Year <- lubridate::year(quercus$Date.Observed)
summary(quercus)

acer <- clean.google(pheno.title = "Phenology_Observations_GoogleForm", collection="Acer", dat.yr=2019)
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

dat.all$Date.Observed <- as.Date(dat.all$Date.Observed)
dat.all$YEAR <- lubridate::year(dat.all$Date.Observed)
dat.all$MONTH <- lubridate::month(dat.all$Date.Observed)
dat.all$DAY <- lubridate::day(dat.all$Date.Observed)
dat.all$DOY <- lubridate::yday(dat.all$Date.Observed)


phenophase.obs <- names(dat.all)[grep(".observed", names(dat.all))] 
for(PHENO in phenophase.obs){
  dat.all[is.na(dat.all[,PHENO]),PHENO] <- "Did not look for"
  dat.all[,PHENO] <- factor(dat.all[,PHENO], levels=c("No", "Yes", "?", "Did not look for"))
}
summary(dat.all)

dat.all$Observer <- factor(dat.all$Observer, levels=c(sort(paste(unique(dat.all$Observer))[!unique(dat.all$Observer) %in% c("Rollinson", "Reidy")]), "Rollinson", "Reidy"))
#----------------------------

#----------------------------
# Do some density plots 
#----------------------------
mark.spring <- data.frame(Label=c("Mar 1", "Apr 1", "May 1", "Jun 1", "Jul 1"), 
                       Date=c("2019-03-01", "2019-04-01", "2019-05-01", "2019-06-01", "2019-07-01"))
mark.spring$mark.yday <- lubridate::yday(mark.spring$Date)

# Add a weights statement that will scale things based on number of individual trees
dat.all[dat.all$Collection=="Acer" & dat.all$Year==2019,"wt.tree"] <- length(unique(dat.all[dat.all$Collection=="Acer" & dat.all$Year==2019,"PlantNumber"]))
dat.all[dat.all$Collection=="Quercus" & dat.all$Year==2019,"wt.tree"] <- length(unique(dat.all[dat.all$Collection=="Quercus" & dat.all$Year==2019,"PlantNumber"]))
dat.all[dat.all$Collection=="Quercus" & dat.all$Year==2018,"wt.tree"] <- length(unique(dat.all[dat.all$Collection=="Quercus" & dat.all$Year==2018,"PlantNumber"]))

pheno.plot <- data.frame(code=c("leaf.buds.observed", "leaf.present.observed", "leaf.color.observed", "flower.open.observed", "fruit.present.observed", "fruit.ripe.observed"),
                         label=c("Bud Burst",
                                 "Leaves Present",
                                 "Colored Leaves",
                                 "Open Flowers",
                                 "Fruit Present", 
                                 "Ripe Fruit"))

for(i in 1:nrow(pheno.plot)){
  png(file.path(path.figs, paste0("Phenophases_DensityPlot_", pheno.plot$code[i], "_2018-2019.png")), height=4, width=6, units="in", res=180)
  print(
  ggplot(data=dat.all[dat.all[,paste(pheno.plot$code[i])]=="Yes",]) +
    ggtitle(pheno.plot$label[i]) +
    facet_grid(Collection~., scales="fixed") +
    geom_density(aes(x=DOY, fill=as.factor(Year), y=..count..), adjust=1, alpha=0.5) +
    scale_y_continuous(name="Number of Observations", expand=c(0,0)) +
    scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=yrs.mark$mark.yday, labels=yrs.mark$Label) +
    scale_fill_manual(name="Year", values=c("blue2", "green4")) +
    theme(panel.background=element_rect(fill=NA, color="black", size=0.5),
          panel.grid = element_blank(),
          # panel.margin=unit(0, "lines"),
          axis.text=element_text(size=rel(1.25), color="black"),
          axis.title=element_text(size=rel(1.25), face="bold"),
          legend.position="top",
          legend.title = element_text(face="bold", size=rel(1.25)),
          legend.text=element_text(size=rel(1.25)),
          legend.key.size=unit(1.25, "lines"),
          strip.text=element_text(face="bold", size=rel(1.5)))
  )
  dev.off()
}


# -----------------------
# Doing a leaf color *intensity* plot
# -----------------------
dat.all$leaf.color.intensity <- car::recode(dat.all$leaf.color.intensity, "'Less than 5%'='<5%'; '50%'='50-74%'; 'NA'='No Data'")
dat.all$leaf.color.intensity[is.na(dat.all$leaf.color.intensity)] <- "No Data"
dat.all$leaf.color.intensity <- factor(dat.all$leaf.color.intensity, c("0%", "<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%", "No Data"))

colors.fall <- c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026", "gray50")

mark.fall <- data.frame(Label=c("Mar 1", "May 1", "Jul 1", "Sep 1", "Nov 1"), 
                          Date=c("2019-03-01", "2019-05-01", "2019-07-01", "2019-09-01", "2019-11-01"))
mark.fall$mark.yday <- lubridate::yday(mark.fall$Date)

png(file.path(path.figs, paste0("Phenophases_DensityPlot_", "leaf.present", "_IntensityColor", "_2018-2019.png")), height=5, width=8, units="in", res=180)
ggplot(data=dat.all[dat.all$leaf.present.observed=="Yes",]) +
  # ggtitle("Colored Leaf") +
  facet_grid(Collection~Year, scales="fixed") +
  geom_density(aes(x=DOY, fill=leaf.color.intensity, y=..count..), position="stack", adjust=1.25, alpha=1) +
  scale_y_continuous(name="# Observations: Leaf Present", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=mark.fall$mark.yday, labels=mark.fall$Label) +
  scale_fill_manual(name="% Canopy\nwith Color", values=colors.fall) +
  theme(panel.background=element_rect(fill=NA, color="black", size=0.5),
        panel.grid = element_blank(),
        # panel.margin=unit(0, "lines"),
        axis.text=element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        legend.position="top",
        legend.title = element_text(face="bold", size=rel(1.25)),
        legend.text=element_text(size=rel(1.25)),
        legend.key.size=unit(1.25, "lines"),
        strip.text=element_text(face="bold", size=rel(1.5)))
dev.off()

png(file.path(path.figs, paste0("Phenophases_DensityPlot_", "leaf.present", "_IntensityColor", "_Quercus", "_2018-2019.png")), height=5, width=8, units="in", res=180)
ggplot(data=dat.all[dat.all$leaf.present.observed=="Yes" & dat.all$Collection=="Quercus",]) +
  # ggtitle("Colored Leaf") +
  facet_grid(Year~Collection, scales="fixed") +
  geom_density(aes(x=DOY, fill=leaf.color.intensity, y=..count..), position="stack", adjust=1.25, alpha=1) +
  scale_y_continuous(name="# Observations: Leaf Present", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=mark.fall$mark.yday, labels=mark.fall$Label) +
  scale_fill_manual(name="% Canopy\nwith Color", values=colors.fall) +
  theme(panel.background=element_rect(fill=NA, color="black", size=0.5),
        panel.grid = element_blank(),
        # panel.margin=unit(0, "lines"),
        axis.text=element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        legend.position="top",
        legend.title = element_text(face="bold", size=rel(1.25)),
        legend.text=element_text(size=rel(1.25)),
        legend.key.size=unit(1.25, "lines"),
        strip.text=element_text(face="bold", size=rel(1.5)))
dev.off()
# -----------------------

# -----------------------
# Doing a bud burst and/or leaf canopy chart
# -----------------------
dat.all$leaf.present.intensity <- as.character(dat.all$leaf.present.intensity)
dat.all$leaf.present.intensity <- car::recode(dat.all$leaf.present.intensity, "'Less than 5%'='<5%';
                                              '< 5%'='<5%'; '50%'='50-74%'; 'NA'='No Data'")
dat.all$leaf.present.intensity[is.na(dat.all$leaf.present.intensity)] <- "No Data"
dat.all$leaf.present.intensity <- factor(dat.all$leaf.present.intensity, c("0%", "<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%", "No Data"))
summary(dat.all$leaf.present.intensity)

colors.leaf <- c("#ffffcc", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#005a32", "gray50")


png(file.path(path.figs, paste0("Phenophases_DensityPlot_", "leaf.present", "_IntensityCanopy", "_2018-2019.png")), height=5, width=8, units="in", res=180)
ggplot(data=dat.all[dat.all$leaf.present.observed=="Yes",]) +
  # ggtitle("Colored Leaf") +
  facet_grid(Collection~Year, scales="fixed") +
  geom_density(aes(x=DOY, fill=leaf.present.intensity, y=..count..), position="stack", adjust=1.25, alpha=1) +
  scale_y_continuous(name="# Observations: Leaf Present", expand=c(0,0)) +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=mark.fall$mark.yday, labels=mark.fall$Label) +
  scale_fill_manual(name="% Canopy\nwith Leaves", values=colors.leaf) +
  theme(panel.background=element_rect(fill=NA, color="black", size=0.5),
        panel.grid = element_blank(),
        # panel.margin=unit(0, "lines"),
        axis.text=element_text(size=rel(1.25), color="black"),
        axis.title=element_text(size=rel(1.25), face="bold"),
        legend.position="top",
        legend.title = element_text(face="bold", size=rel(1.25)),
        legend.text=element_text(size=rel(1.25)),
        legend.key.size=unit(1.25, "lines"),
        strip.text=element_text(face="bold", size=rel(1.5)))
dev.off()

# -----------------------



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

first.event$diff.leaf.bud <- first.event$leaf.present.observed - first.event$leaf.buds.observed
first.event[!is.na(first.event$diff.leaf.bud) & first.event$diff.leaf.bud<0,]
summary(first.event)

mean(first.event$diff.leaf.bud[first.event$Year==2019 & first.event$diff.leaf.bud>=0], na.rm=T)
sd(first.event$diff.leaf.bud[first.event$Year==2019 & first.event$diff.leaf.bud>=0], na.rm=T)

hist(first.event$diff.leaf.bud[first.event$Year==2019 & first.event$diff.leaf.bud>=0])


# Leaf color
chron::month.day.year(jul=range(first.event[first.event$Year == 2019 & first.event$Collection=="Acer", "leaf.color.observed"], na.rm=T), origin=c(month=1, day=1, year=2019))
#----------------------------


#----------------------------
# Summarizing Peak Color
#----------------------------
summary(dat.all)

# Observations of color intensity ">95%" by collection in eyar
range(dat.all[dat.all$YEAR==2019 & dat.all$Collection=="Acer" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])

median(dat.all[dat.all$YEAR==2019 & dat.all$Collection=="Acer" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])
mean(dat.all[dat.all$YEAR==2019 & dat.all$Collection=="Acer" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])
sd(dat.all[dat.all$YEAR==2019 & dat.all$Collection=="Acer" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])

median(dat.all[dat.all$YEAR==2019 & dat.all$Collection=="Quercus" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])
mean(dat.all[dat.all$YEAR==2019 & dat.all$Collection=="Quercus" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])
sd(dat.all[dat.all$YEAR==2019 & dat.all$Collection=="Quercus" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])

median(dat.all[dat.all$YEAR==2018 & dat.all$Collection=="Quercus" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])
mean(dat.all[dat.all$YEAR==2018 & dat.all$Collection=="Quercus" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])
sd(dat.all[dat.all$YEAR==2018 & dat.all$Collection=="Quercus" & !is.na(dat.all$leaf.color.intensity) & dat.all$leaf.color.intensity==">95%","Date.Observed"])


#----------------------------

#----------------------------

