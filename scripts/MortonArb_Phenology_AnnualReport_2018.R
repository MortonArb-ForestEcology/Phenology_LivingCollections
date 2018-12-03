# Annual report (rough draft) for NPN Local Leaders final project
# Load some useful packages; (if you don't have these installed, you'll need to do so)
library(ggplot2); library(googlesheets); 

# For the npn_get_obs function:
library(raster); library(httr); library(jsonlite)

figures.out <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/Figures/2018"
dir.create(figures.out, recursive=T)
# ----------------------------
# 1. Grabbing our raw data
# ----------------------------
# # NOTE: The first time you run things, you're going to have to grant google sheets permission to 
#         access your Google Drive account. An Authentication token (.httr-oauth) will be stored 
#         in the working directory.  You should only need to do this once as long as you use the same
#         working directory.  Make sure this token does not get pushed to github etc.
# "Register" the sheet we want to work with (load the metadata so we can interact with it)
pheno.lc <- gs_title("Phenology_Observations_GoogleForm_2018")
pheno.lc

# get the data from a particular sheet
dat.raw <- data.frame(gs_read(pheno.lc, ws="Raw Observations"))

# Renaming some columns
names(dat.raw)[grep("OPTIONAL", names(dat.raw))] <- "Notes"
names(dat.raw)[grep("species", names(dat.raw))] <- "Species"

# Coming up with handy groups for our columns
cols.meta <- c("Timestamp", "Email.Address", "Observer", "Date.Observed", "Species", "PlantNumber", "Notes")
pheno.leaf <- names(dat.raw)[grep("leaf", tolower(names(dat.raw)))]
pheno.flower <- names(dat.raw)[grep("flower", tolower(names(dat.raw)))]
pheno.fruit <- names(dat.raw)[grep("fruit", tolower(names(dat.raw)))]

# Setting things to factors
for(i in 1:ncol(dat.raw)){
  if(class(dat.raw[,i])=="character") dat.raw[,i] <- as.factor(dat.raw[,i])
}
summary(dat.raw)
cols.id <- grep("accession", names(dat.raw))

dat.clean <- dat.raw[,c(cols.meta[!cols.meta=="PlantNumber"], pheno.leaf, pheno.flower, pheno.fruit)]
dat.clean$PlantNumber <- as.factor(apply(dat.raw[,cols.id], 1, FUN=function(x) {x[which(!is.na(x))][1]})) # Get the PlantNumber
dat.clean$Timestamp <- strptime(dat.clean$Timestamp, format="%m/%d/%Y %H:%M:%S")
dat.clean$Date.Observed <- as.Date(dat.clean$Date.Observed, format="%m/%d/%Y")
dat.clean <- dat.clean[,c(cols.meta, pheno.leaf, pheno.flower, pheno.fruit)] # Just re-organizing to how I like to see things
summary(dat.clean)

# Get rid of observations that have TEST in them or are before our last phenology training
rows.remove <- c(which(is.na(dat.clean$Species)), grep("TEST", toupper(dat.clean$NOTES)), grep("TEST", toupper(dat.clean$Observer)) )
if(length(rows.remove)>0) dat.clean <- dat.clean[(1:nrow(dat.clean) %in% rows.remove),] # 

# We had some rows where the year got entered wrong as 0018 rather than 2018
yr.wrong <- dat.clean[lubridate::year(dat.clean$Date.Observed)==0018,"Date.Observed"]

dat.clean[dat.clean$Date.Observed==yr.wrong,"Date.Observed"] <- as.Date(paste(2018, lubridate::month(yr.wrong), lubridate::day(yr.wrong), sep="-"))

dat.clean <- droplevels(dat.clean) # Get rid of unused levels
summary(dat.clean)

# Looking at some problematic records
dat.clean[is.na(dat.clean$PlantNumber),]

# Fixing missing accession numbers
dat.clean$PlantNumber <- as.character(dat.clean$PlantNumber)

# Once we figure out what that accession number is, change from "missing" to an actual accession number
# dat.clean[dat.clean$Observer=="Rose" & dat.clean$Species=="Quercus montana" & is.na(dat.clean$PlantNumber),"PlantNumber"] <- "missing" 

dat.clean$PlantNumber <- as.factor(dat.clean$PlantNumber)

# Cleaning up observer tags
summary(dat.clean$Observer)

# ----------------------------

# ----------------------------
# Generate some summary numbers for our phenology report
# ----------------------------
# Get some summary stats on our number of trees and Species
nrow(dat.clean) # Number of observations (not counting individual phenophases)
range(dat.clean$Date.Observed) # Date range (note: incomplete at moment)
length(unique(dat.clean$PlantNumber)) # Number of individuals observed (assuming no typos)
length(unique(dat.clean$Species)) # Number of Species observed (assuming no typos)


# Load some accessions metadata
quercus <- read.csv("/Volumes/GoogleDrive/My Drive/Morton_Data_Misc/2018-03-19_161744393-BRAHMSOnlineData.csv")
summary(quercus)

# GGMAP is broken :-( 
# library(ggmap)
# quercus.ext <- extent(range(quercus$BgLongitude, na.rm=T), range(quercus$BgLatitude, na.rm=T))
# sbbox <- make_bbox(lon = quercus$BgLongitude, lat = quercus$BgLatitude, f = .1)
# library(dismo)
# oaks <- gmap(, type="satellite")
# oaks <- get_map(location = sbbox, maptype = "satellite", source = "google", zoom=12)


# Find the earliest bud burst and first color by individual tree
budburst <- aggregate(dat.clean[dat.clean$Breaking.leaf.buds=="Yes", "Date.Observed"], 
                      by=dat.clean[dat.clean$Breaking.leaf.buds=="Yes", c("Species", "PlantNumber")], 
                      FUN=min)
names(budburst)[which(names(budburst)=="x")] <- "Date.Observed"
budburst$type <- "budburst-first"
summary(budburst)

budburst2 <- aggregate(dat.clean[dat.clean$Breaking.leaf.buds=="Yes", "Date.Observed"], 
                       by=dat.clean[dat.clean$Breaking.leaf.buds=="Yes", c("Species", "PlantNumber")], 
                       FUN=mean)
names(budburst2)[which(names(budburst2)=="x")] <- "Date.Observed"
budburst2$type <- "budburst-mean"
summary(budburst2)

leafout <- aggregate(dat.clean[dat.clean$Leaf.observed=="Yes", "Date.Observed"], 
                       by=dat.clean[dat.clean$Leaf.observed=="Yes", c("Species", "PlantNumber")], 
                       FUN=min)
names(leafout)[which(names(leafout)=="x")] <- "Date.Observed"
leafout$type <- "leafout-first"
leafout[leafout$Date.Observed>"2018-07-01","Date.Observed"] <- NA
summary(leafout)

budburst <- rbind(budburst, budburst2, leafout)
budburst <- merge(budburst, quercus[,c("PlantNumber", "BgLongitude", "BgLatitude")])
budburst$doy <- lubridate::yday(budburst$Date.Observed)
summary(budburst)

dates.bb <- c("2018-05-01", "2018-05-08", "2018-05-15", "2018-05-22", "2018-05-29")
doy.bb <- sapply(dates.bb, lubridate::yday)

png(file.path(figures.out, "Budburst_First_2018_Map.png"), height=4, width=7, units="in", res=120)
ggplot(data=budburst[budburst$type=="budburst-first" & budburst$doy>90,]) +
  coord_cartesian() +
  ggtitle("Date of First Budburst") +
  geom_point(data=quercus, aes(x=BgLongitude, y=BgLatitude), size=1, color="gray50") +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=doy), size=3) +
  theme_bw() +
  scale_colour_distiller(palette=rev("PiYG"), breaks=doy.bb, labels=dates.bb, trans="reverse") +
  theme(legend.position=c(0.25, 0.25),
        legend.text=element_text(color='white', face="bold"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        panel.background=element_rect(fill="black"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks=element_blank())
dev.off()

dates.lo <- c("2018-05-01", "2018-05-15", "2018-06-01", "2018-06-15")
doy.lo <- sapply(dates.lo, lubridate::yday)

png(file.path(figures.out, "Budburst_First_2018_Map.png"), height=4, width=7, units="in", res=120)
ggplot(data=budburst[budburst$type=="leafout-first" ,]) +
  coord_cartesian() +
  ggtitle("Date of First Budburst") +
  geom_point(data=quercus, aes(x=BgLongitude, y=BgLatitude), size=1, color="gray50") +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=doy), size=3) +
  theme_bw() +
  scale_colour_distiller(palette=rev("PiYG"), breaks=doy.lo, labels=dates.lo, trans="reverse") +
  theme(legend.position=c(0.25, 0.25),
        legend.text=element_text(color='white', face="bold"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        panel.background=element_rect(fill="black"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks=element_blank())
dev.off()

png(file.path(figures.out, "Budburst_First_2018.png"), height=4, width=7, units="in", res=120)
ggplot(data=budburst[budburst$type=="budburst-first",]) +
  ggtitle("Date of First Budburst") +
  geom_histogram(aes(x=Date.Observed, fill=Species), binwidth=7) +
  theme_bw() +
  guides(fill=F) +
  # scale_fill_discrete(colors=brewer.pal(length(unique(budburst$Species)),"Set3")) +
  theme(legend.position="bottom")
dev.off()

png(file.path(figures.out, "LeafOut_First_2018.png"), height=4, width=7, units="in", res=120)
ggplot(data=budburst[budburst$type=="leafout-first",]) +
  ggtitle("Date of First Leaf Out") +
  geom_histogram(aes(x=Date.Observed, fill=Species), binwidth=7) +
  theme_bw() +
  guides(fill=F) +
  # scale_fill_discrete(colors=brewer.pal(length(unique(budburst$Species)),"Set3")) +
  theme(legend.position="bottom")
dev.off()

png(file.path(figures.out, "SpringPhenology_2018.png"), height=4, width=7, units="in", res=120)
ggplot(data=budburst[budburst$doy>90 & !is.na(budburst$doy) & budburst$type != "budburst-mean",]) +
  # ggtitle("Date of Budburst") +
  facet_grid(type~.) +
  geom_histogram(aes(x=Date.Observed, fill=Species), binwidth=7) +
  theme_bw() +
  guides(fill=F) +
  # scale_fill_discrete(colors=brewer.pal(length(unique(budburst$Species)),"Set3")) +
  theme(legend.position="bottom")
dev.off()





# Fall Color
fall.color <- aggregate(dat.clean[!is.na(dat.clean$Leaf.color.observed) & dat.clean$Leaf.color.observed=="Yes", "Date.Observed"], 
                      by=dat.clean[!is.na(dat.clean$Leaf.color.observed) & dat.clean$Leaf.color.observed=="Yes", c("Species", "PlantNumber")], 
                      FUN=min)
names(fall.color)[which(names(fall.color)=="x")] <- "Date.Observed"
fall.color$type <- "first"
summary(fall.color)

fall.color2 <- aggregate(dat.clean[!is.na(dat.clean$Leaf.color.observed) & dat.clean$Leaf.color.observed=="Yes", "Date.Observed"], 
                       by=dat.clean[!is.na(dat.clean$Leaf.color.observed) & dat.clean$Leaf.color.observed=="Yes", c("Species", "PlantNumber")], 
                       FUN=mean)
names(fall.color2)[which(names(fall.color2)=="x")] <- "Date.Observed"
fall.color2$type <- "mean"
summary(fall.color2)

cat.peak <- c(">95%", "50-74%", "75-94%")
ind.peak <- dat.clean$Leaf.color.observed=="Yes" & !is.na(dat.clean$Leaf.color.observed) & dat.clean$Leaf.color.intensity %in% cat.peak

color.peak <- aggregate(dat.clean[ind.peak, "Date.Observed"], 
                         by=dat.clean[ind.peak, c("Species", "PlantNumber")], 
                         FUN=mean)
names(color.peak)[which(names(color.peak)=="x")] <- "Date.Observed"
color.peak$type <- "peak"
summary(color.peak)


fall.color <- rbind(fall.color, fall.color2, color.peak)
fall.color <- merge(fall.color, quercus[,c("PlantNumber", "BgLongitude", "BgLatitude")])
fall.color$doy <- lubridate::yday(fall.color$Date.Observed)
summary(fall.color)


dates.fc <- c("2018-09-01", "2018-09-15", "2018-10-01", "2018-10-15", "2018-11-01")
doy.fc <- sapply(dates.fc, lubridate::yday)

png(file.path(figures.out, "FallColor_Peak_2018_Map.png"), height=4, width=7, units="in", res=120)
ggplot(data=fall.color[fall.color$type=="peak",]) +
  ggtitle("Mean Date of Peak Color") +
  geom_point(data=quercus, aes(x=BgLongitude, y=BgLatitude), size=1, color="gray50") +
  geom_point(aes(x=BgLongitude, y=BgLatitude, color=doy), size=3) +
  theme_bw() +
  scale_colour_distiller(palette="RdYlBu", direction=2, breaks=doy.fc, labels=dates.fc, trans="reverse") +
  theme(legend.position=c(0.25, 0.4),
        legend.text=element_text(color='white', face="bold"),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        panel.background=element_rect(fill="black"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks=element_blank())
dev.off()

png(file.path(figures.out, "FallColor_First_2018.png"), height=4, width=7, units="in", res=120)
ggplot(data=fall.color[fall.color$type=="first",]) +
  ggtitle("Date of First Fall Color") +
  geom_histogram(aes(x=Date.Observed, fill=Species), binwidth=7) +
  theme_bw() +
  guides(fill=F) +
  # scale_fill_discrete(colors=brewer.pal(length(unique(budburst$Species)),"Set3")) +
  theme(legend.position="bottom")
dev.off()

png(file.path(figures.out, "FallColor_Peak_2018.png"), height=4, width=7, units="in", res=120)
ggplot(data=fall.color[fall.color$type=="peak",]) +
  # ggtitle("Date of Peak Fall Color") +
  geom_histogram(aes(x=Date.Observed, fill=Species), binwidth=7) +
  theme_bw() +
  guides(fill=F) +
  labs(x="Date") +
  # scale_x_continuous(name="Date", expand=c(0,0)) +
  scale_y_continuous(name="Number Trees", expand=c(0,0)) +
  # scale_fill_discrete(colors=brewer.pal(length(unique(budburst$Species)),"Set3")) +
  theme(legend.position="bottom",
        axis.title = element_text(size=rel(2), face="bold"),
        axis.text  = element_text(size=rel(2)),
        panel.grid = element_blank())
dev.off()


png(file.path(figures.out, "FallColor_2018.png"), height=4, width=7, units="in", res=120)
ggplot(data=fall.color) +
  ggtitle("Date of Fall Color") +
  facet_grid(type~.) +
  geom_histogram(aes(x=Date.Observed, fill=Species), binwidth=7) +
  theme_bw() +
  guides(fill=F) +
  # scale_fill_discrete(colors=brewer.pal(length(unique(budburst$Species)),"Set3")) +
  theme(legend.position="bottom")
dev.off()


# ----------------------------

