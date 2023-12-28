# ---------------------------------
# 0. Set up some general file paths
# ---------------------------------
library(ggplot2)
library(gghighlight)
# path.google <- "G:/My Drive" # Windows
path.google <- "/Volumes/GoogleDrive/My Drive" # Mac


path.out <- file.path(path.google, "LivingCollections_Phenology/Phenology Forecasting")
path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2023_02_EndOfYear_Report/figures_2023_end")
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

dir.met <- "../data_raw/meteorology"
dir.create(dir.met, recursive=T, showWarnings = F)
# ---------------------------------


# ---------------------------------
# 1. NOAA COOP Station data
# Use FedData Package to download station data and put it here
# ---------------------------------
ID="USC00115097"
vars.want <- c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD")
path.ghcn=c("../data_raw/meteorology/GHCN_extracted/")
dir.raw="../data_raw/meteorology/GHCN_raw/"

source("phenology weather/met_download_GHCN.R"); 
source("phenology weather/met_gapfill.R")

download.ghcn(ID=ID, vars.in=vars.want, path.save=path.ghcn, dir.raw=dir.raw, gapfill=T, method="https")
# -------------------------------------


# -------------------------------------
# Calculating some cumulative statistics
# -------------------------------------
# Create a function to calculate values we're interested in for prediction
calc.indices <- function(dat){
  # Assumes upper case column names of: TMAX, TMIN, PRCP, YDAY
  # For chilling days, only start after the solstice (June 20th)
  dat$TMEAN <- apply(dat[,c("TMAX", "TMIN")], 1, mean)
  dat$GDD0 <- ifelse(dat$TMEAN>0, dat$TMEAN-0, 0)
  dat$GDD5 <- ifelse(dat$TMEAN>5, dat$TMEAN-5, 0)
  dat$CDD0 <- ifelse(dat$YDAY>172 & dat$TMEAN<0, 0-dat$TMEAN, 0)
  dat$CDD2 <- ifelse(dat$YDAY>172 & dat$TMEAN< -2, -2-dat$TMEAN, 0)
  dat$DaysNoRain <- NA
  
  dat[, c("GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum", "PRCP.cum")] <- cumsum(dat[, c("GDD0", "GDD5", "CDD0", "CDD2", "PRCP")])
  
  dat[, c("NORAIN.cum")] <- cumsum(ifelse(dat[,"PRCP"]>0, 1, 0))
  
  # Calculating days since rain just in case
  dat$DaysNoRain[1] <- ifelse(dat$PRCP[1]>0, 0, 1)
  for(i in 2:nrow(dat)){
    dat$DaysNoRain[i] <- ifelse(dat$PRCP[i]>0, dat$DaysNoRain[i-1]+1, 0)
  }
  return(dat)
}

# For the "historical" GHCN data
dat.ghcn <- read.csv(file.path(path.ghcn, "USC00115097_latest.csv"))
dat.ghcn$DATE <- as.Date(dat.ghcn$DATE)
summary(dat.ghcn)

yr.min <- 2007
yr.max <- lubridate::year(Sys.Date())
dat.ghcn2 <- data.frame()
for(YR in yr.min:yr.max){
  rows.yr <- which(dat.ghcn$YEAR==YR)
  # dat.yr <- dat.ghcn[rows.yr,]
  dat.tmp <- calc.indices(dat=dat.ghcn[rows.yr,])
  dat.ghcn2 <- rbind(dat.ghcn2, dat.tmp)
}
summary(dat.ghcn2)
head(dat.ghcn2)
# dat.ghcn2[dat.ghcn2$DATE=="2020-04-09",]

#creating a data frame of just the last 5 years of weather data
dat.ghcn3 <- dat.ghcn2[dat.ghcn2$YEAR>=2019,]

#making sure the date being shown only shows spring dates
dat.ghcn33 <- dat.ghcn3[dat.ghcn3$YDAY<=359,]
summary(dat.ghcn33)
head(dat.ghcn33)

#Subsetting out uncecessary columns for the phenology report
dat.ghcn4 <- dat.ghcn33[ ,c("YEAR","MONTH", "TMAX", "TMIN","PRCP", "DATE", "YDAY", "TMEAN", "PRCP.cum", "GDD5.cum")]
summary(dat.ghcn4)
head(dat.ghcn4)

#Just getting daily percipitation
dat.ghcn5 <- dat.ghcn4[ ,c("YEAR", "MONTH", "DATE", "YDAY", "PRCP.cum")]
summary(dat.ghcn5)

#calculating the mean daily percipation across years
dat.ghcn5mean <- aggregate(PRCP.cum ~ YDAY,dat=dat.ghcn5, FUN=mean, NA.rm=T)

#attemtption to generte a graph
png(file.path(path.figs,"Cumulative_Precipitation.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.ghcn5) +
  geom_line(aes(x=YDAY, y=PRCP.cum, color=as.factor(YEAR)))+
  geom_smooth(data=dat.ghcn5mean)+ (aes(x=YDAY, y=PRCP.cum))+
  # scale_color_manual(name="Year") +
  # scale_fill_manual(name="Year") +
  labs(title="Cumulative Precipitation", y="Precipitation in cm", x="Day of Year", color="Year") +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +  # Set breaks every 25 days
  theme_classic()
dev.off()

#using a smooth point graph I don't know if this is relevant
ggplot(data=dat.ghcn5) +
  geom_smooth(aes(x=YDAY, y=PRCP.cum, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  labs(title="Cumulative Precipitation", y="Precipitation in cm", x="Day of Year", color="Year", fill="Year")

#doing the same thing as lines 73-83 above but for TMEAN
dat.ghcn6 <- dat.ghcn4[ ,c("YEAR", "MONTH", "DATE", "YDAY", "TMEAN")]
summary(dat.ghcn6)

dat.ghcn6mean <- aggregate(TMEAN ~ YDAY,dat=dat.ghcn6, FUN=mean, NA.rm=T)

#(color = "green", linetype = "dashed", aes(x=YDAY, Y=TMEAN))+
#graph of Mean Temperature

  #png(file.path(path.figs,"Average Daily Temperature.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.ghcn6) +
  geom_line(aes(x=YDAY, y=TMEAN, color=as.factor(YEAR)))+
 geom_smooth(aes (x=YDAY, y=TMEAN))+
  gghighlight::gghighlight(YEAR== "2023") +
   geom_smooth(data= dat.ghcn6mean, color = "black", linetype = "dashed", aes(x=YDAY, y=TMEAN))+
  labs(title="Average Daily Temperature", y="Temperature deg. C", x="Day of Year", color="Year") +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  theme_classic()
dev.off() 


#using a smooth point graph I don't know if this is relevant
ggplot(data=dat.ghcn6) +
  geom_smooth(aes(x=YDAY, y=TMEAN, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  labs(title="Average Daily Temperature", y="Temperature ?C", x="Day of Year", fill="Year", color="Year")

#Just getting GDD5
dat.ghcn7 <- dat.ghcn4[ ,c("YEAR", "MONTH", "DATE", "YDAY", "GDD5.cum")]
summary(dat.ghcn7)

dat.ghcn7 <- dat.ghcn7 [dat.ghcn7$YDAY<=180,]
summary(dat.ghcn7)

#attemtption to generte a graph
#png(file.path(path.figs,"Cumulative GDD5.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.ghcn7) +
  geom_line(aes(x=YDAY, y=GDD5.cum, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  labs(title="Cumulative Growing Degree Days", y="Cumulative GDD5", x="Day of Year", color="Year") +
  theme_classic()
dev.off()

#using a smooth point graph I don't know if this is relevant
ggplot(data=dat.ghcn7) +
  geom_smooth(aes(x=YDAY, y=GDD5.cum, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  labs(title="Cumulative Growing Degree Days", y="?", x="Day of Year", fill="Year", color="Year")

################# Graphing for everything since 200#################
dat.ghcn13 <- dat.ghcn2[dat.ghcn2$YEAR>=2008,]
summary(dat.ghcn13)
head(dat.ghcn13)

#Subsetting out uncecessary columns for the phenology report
dat.ghcn14 <- dat.ghcn13[ ,c("YEAR","MONTH", "TMAX", "TMIN","PRCP", "DATE", "YDAY", "TMEAN", "PRCP.cum", "GDD5.cum")]
summary(dat.ghcn14)
head(dat.ghcn14)

#Just getting daily percipitation
dat.ghcn15 <- dat.ghcn14[ ,c("YEAR", "MONTH", "DATE", "YDAY", "PRCP.cum")]
summary(dat.ghcn15)

#calculating the mean daily percipation across years
dat.ghcn15mean <- aggregate(PRCP.cum ~ YDAY,data=dat.ghcn15, FUN=mean, NA.rm=T)

#attemtption to generte a graph
#png(file.path(path.figs,"Cumulative Precipitation Since 2007.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.ghcn15) +
  geom_line(aes(x=YDAY, y=PRCP.cum, color=as.factor(YEAR)))+
  gghighlight::gghighlight(YEAR== "2023")     +
  geom_smooth(data=dat.ghcn15mean)+ (aes(x=YDAY, y=PRCP.cum))+
  # scale_color_manual(name="Year") +
  # scale_fill_manual(name="Year") +
  labs(title="Cumulative Precipitation", y="Precipitation in cm", x="Day of Year", color="Year") +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  theme_classic()
dev.off()

#using a smooth point graph I don't know if this is relevant
ggplot(data=dat.ghcn15) +
  geom_smooth(aes(x=YDAY, y=PRCP.cum, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  labs(title="Cumulative Precipitation", y="Precipitation in cm", x="Day of Year", color="Year", fill="Year")

#doing the same thing as lines 73-83 above but for TMEAN
dat.ghcn16 <- dat.ghcn14[ ,c("YEAR", "MONTH", "DATE", "YDAY", "TMEAN")]
summary(dat.ghcn16)
dat.ghcn16mean <- aggregate(TMEAN ~ YDAY,dat=dat.ghcn6, FUN=mean, NA.rm=T)

#graph of Mean Temperature
#png(file.path(path.figs,"Average Daily Temperature Since 2007.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.ghcn16) +
  geom_line(aes(x=YDAY, y=TMEAN, color=as.factor(YEAR)))+
  geom_smooth(aes (x=YDAY, y=TMEAN))+
  gghighlight::gghighlight(YEAR== "2023") +
  geom_smooth(data= dat.ghcn16mean, color = "black", linetype = "dashed", aes(x=YDAY, y=TMEAN))+
  labs(title="Average Daily Temperature", y="Temperature deg. C", x="Day of Year", color="Year") +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +  # Set breaks every 25 days
  theme_classic()
dev.off() 

#using a smooth point graph I don't know if this is relevant
ggplot(data=dat.ghcn16) +
  geom_smooth(aes(x=YDAY, y=TMEAN, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  labs(title="Average Daily Temperature", y="Temperature ?C", x="Day of Year", fill="Year", color="Year")

#Just getting GDD5
dat.ghcn17 <- dat.ghcn14[ ,c("YEAR", "MONTH", "DATE", "YDAY", "GDD5.cum")]
summary(dat.ghcn17)

dat.ghcn17 <- dat.ghcn17 [dat.ghcn17$YDAY<=180,]
summary(dat.ghcn17)

#attemtption to generte a graph
#png(file.path(path.figs,"Cumulative GDD5 Since 2007.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.ghcn17) +
  geom_line(aes(x=YDAY, y=GDD5.cum, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  gghighlight::gghighlight(YEAR== "2023") +
  labs(title="Cumulative Growing Degree Days", y="Cumulative GDD5", x="Day of Year", color="Year") +
  theme_classic()
dev.off()

#using a smooth point graph I don't know if this is relevant
ggplot(data=dat.ghcn17) +
  geom_smooth(aes(x=YDAY, y=GDD5.cum, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  labs(title="Cumulative Growing Degree Days", y="?", x="Day of Year", fill="Year", color="Year")

#Getting cumulative precipitation for just this year
dat.ghcn18 <- dat.ghcn2[dat.ghcn2$YEAR=="2023", c("YEAR", "MONTH", "DATE", "YDAY", "PRCP.cum")]
summary(dat.ghcn18)
#looking at cumulative precipitation only before December first since most trees will have gone dormant by then.
dat.ghcn18 <- dat.ghcn18 [dat.ghcn18$YDAY<=359,]
summary(dat.ghcn18)

#comparing it to the averages for all years without December and for all years before this year
dat.ghcn151 <- dat.ghcn14[ ,c("YEAR", "MONTH", "DATE", "YDAY", "PRCP.cum")]
summary(dat.ghcn151)
dat.ghcn151 <- dat.ghcn151 [dat.ghcn151$YDAY<=359,]
summary(dat.ghcn151)
dat.ghcn151 <- dat.ghcn151 [dat.ghcn151$YEAR<2023,]
summary(dat.ghcn151)

#writing a csv out need to change the data fram to what ever .ghcn I'm writing
#write.csv(dat.ghcn2, file.path(path.out, "data", "Weather_ArbCOOP_historical_latest.csv"), row.names=F)

