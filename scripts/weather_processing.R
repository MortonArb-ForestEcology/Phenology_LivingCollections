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
#png(file.path(path.figs,"Cumulative_Precipitation.png"), height=4, width=6, units="in", res=320)
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
  gghighlight::gghighlight(YEAR== "2024") +
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
  gghighlight::gghighlight(YEAR== "2024")     +
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

# Create date conversions
yd2date <- as.Date(1:365, origin="2008-01-01")
mo.day <- format(yd2date, "%b %d")

png(file.path(path.figs,"Mean Daily Temperature Since 2007.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.ghcn16) +
  geom_line(aes(x=YDAY, y=TMEAN, color=as.factor(YEAR))) +
  geom_smooth(aes(x=YDAY, y=TMEAN)) +
  gghighlight::gghighlight(YEAR== "2024") +
  geom_smooth(data=dat.ghcn16mean, color="black", linetype="dashed", aes(x=YDAY, y=TMEAN)) +
  labs(title="Average Daily Temperature", y="Temperature", x="Date", color="Year") +
  scale_x_continuous(
    breaks = seq(1, 365, by = 30), 
    labels = mo.day[seq(1, 365, by = 30)] 
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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

yd2date <- as.Date(1:366, origin="2008-01-01")
mo.day <- format(yd2date , "%b %d")

#attemtption to generte a graph
#png(file.path(path.figs,"Cumulative GDD5 Since 2007.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.ghcn17) +
  geom_line(aes(x=YDAY, y=GDD5.cum, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  gghighlight::gghighlight(YEAR== "2024") +
  labs(title="Cumulative Growing Degree Days", y="Cumulative GDD5", x="Day of Year", color="Year") +
  theme_classic()
dev.off()

ggplot(data=dat.ghcn17) +
  #png(file.path(path.figs,"Cumulative GDD5 Since 2007.png"), height=4, width=6, units="in", res=320)+
  geom_line(aes(x=YDAY, y=GDD5.cum, fill=as.factor(YEAR), color=as.factor(YEAR))) +
  gghighlight::gghighlight(YEAR== "2024") +
  labs(title="Cumulative Growing Degree Days", y="Cumulative GDD5", x="Date", color="Year") +
  scale_x_continuous(
    breaks = seq(1, 180, by = 30), 
    labels = mo.day[seq(1, 180, by = 30)] 
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#using a smooth point graph I don't know if this is relevant
ggplot(data=dat.ghcn17) +
  geom_smooth(aes(x=YDAY, y=GDD5.cum, fill=as.factor(YEAR), color=as.factor(YEAR)))+
  labs(title="Cumulative Growing Degree Days", y="?", x="Day of Year", fill="Year", color="Year")

#Getting cumulative precipitation for just 2022
dat.ghcn18 <- dat.ghcn2[dat.ghcn2$YEAR=="2023", c("YEAR", "MONTH", "DATE", "YDAY", "PRCP.cum")]
summary(dat.ghcn18)
#looking at cumulative precipitation only before December first since most trees will have gone dormant by then.
dat.ghcn18 <- dat.ghcn18 [dat.ghcn18$YDAY<=331,]
summary(dat.ghcn18)

#compring it to the averages for all years without December and for all years before this year
dat.ghcn151 <- dat.ghcn14[ ,c("YEAR", "MONTH", "DATE", "YDAY", "PRCP.cum")]
summary(dat.ghcn151)
dat.ghcn151 <- dat.ghcn151 [dat.ghcn151$YDAY<=331,]
summary(dat.ghcn151)
dat.ghcn151 <- dat.ghcn151 [dat.ghcn151$YEAR<2023,]
summary(dat.ghcn151)

#writing a csv out need to change the data fram to what ever .ghcn I'm writing
#write.csv(dat.ghcn2, file.path(path.out, "data", "Weather_ArbCOOP_historical_latest.csv"), row.names=F)

#Just getting cumulative days without rain
#png(file.path(path.figs,"Cumulative Precipitation.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.ghcn14) +
  aes(x=YDAY, y=NORAIN.cum, color=as.factor(YEAR))+
  geom_line()+
  geom_line(data = dat.ghcn14[dat.ghcn14$YEAR == 2023, ], aes(color = "2023"), size = 1.0) +
  geom_line(data = dat.ghcn14[dat.ghcn14$YEAR == 2021, ], aes(color = "2021"), size = 1.0) +
  geom_line(data = dat.ghcn14[dat.ghcn14$YEAR == 2012, ], aes(color = "2012"), size = 1.0) +
  scale_color_manual(values = c("2023" = "goldenrod", "2021" = "darkblue",'2012'="red3")) +
  labs(title="Cumulative Days without Rain", y="Cumulative days without rain", x="Day of Year", color="Year") +
  theme_classic()
#dev.off()

#getting today days without rain

#dev.off()
# Identify drought periods
dat.ghcn3$drought_period <- with(dat.ghcn3, ave(DaysNoRain, YEAR, FUN = function(x) sequence(rle(x == 0)$lengths)))

# Calculate mean duration of drought periods for each day
mean_drought_duration <- aggregate(drought_period ~ YDAY, data = dat.ghcn3, FUN = mean)

# Plotting drought periods by year
ggplot(dat.ghcn3, aes(x = YDAY, y = drought_period, fill = as.factor(YEAR))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(mean_drought_duration$drought_period), linetype = "dashed", color = "black", size =0.5) +
  labs(title = "Drought Periods by Year",
       x = "Day of the Year",
       y = "Duration of Drought Period",
       fill = "Year") +
  theme_minimal() +
  theme(legend.position = "none")+
  facet_wrap( ~YEAR,)








###More drought graphs +
  geom_line(data = dat.ghcn4, aes(y = PRCP.cum / max(PRCP.cum), color = as.factor(YEAR)), size = 1, linetype = "dashed") +
  labs(title = "Drought Periods as a Function of Days Without Rain, Temperature, and Precipitation",
       x = "Day of the Year",
       y = "Normalized Values",
       fill = "Year") +
  facet_wrap(~ variable, scales = "free_y", nrow = 3) +
  theme_minimal() +
  theme(legend.position = "none")
  facet_wrap(~ YEAR, scales = "free_y")

# Calculate z-score for DaysNoRain
dat.ghcn3$DaysNoRain_z <- with(dat.ghcn3, ave(DaysNoRain, YDAY, FUN = function(x) (x - mean(x)) / sd(x)))

# Plotting z-score of days without rain by year
ggplot(dat.ghcn3, aes(x = YDAY, y = DaysNoRain_z, color = as.factor(YEAR))) +
  geom_box(size = 0.25) +
  labs(title = "Z-Score of Days Without Rain by Year",
       x = "Day of the Year",
       y = "Z-Score of Days Without Rain",
       color = "Year") +
  theme_minimal() +
  theme(legend.position = "none")+
  facet_wrap(~ YEAR, scales = "free_y") 

ggplot(dat.ghcn3, aes(x = YDAY, y = DaysNoRain_z, fill = as.factor(YEAR))) +
  geom_bar(stat = "identity") +
  labs(title = "Z-Score of Days Without Rain by Year",
       x = "Day of the Year",
       y = "Z-Score of Days Without Rain",
       fill = "Year") +
  theme_minimal() +
  theme(legend.position = "none")+
  ylim(-1, 2)+
  facet_wrap(~ YEAR,) 


####
# -------------------------------------
# Calculating SPI
# -------------------------------------
# Create a function to calculate SPI
calc_spi <- function(prcp) {
  n <- length(prcp)
  
  # Convert NA values to 0
  prcp[is.na(prcp)] <- 0
  
  # Calculate cumulative precipitation
  cum_prcp <- cumsum(prcp)
  
  # Calculate SPI using a simple approach
  spi_values <- (cum_prcp - mean(cum_prcp)) / sd(cum_prcp)
  
  # Return SPI values
  return(spi_values)
}

# Calculate SPI for the specified precipitation variable
dat.ghcn3$SPI <- unlist(tapply(dat.ghcn3$PRCP, dat.ghcn3$YEAR, function(x) calc_spi(x)))

# -------------------------------------
# Plotting SPI by day and year
# -------------------------------------
# Plot SPI values by day and year
ggplot(dat.ghcn3, aes(x = YDAY, y = SPI, color = as.factor(YEAR))) +
  geom_line(size=0.25) +
  labs(title = "SPI by Day and Year",
       x = "Day of Year",
       y = "SPI",
      color="Year" ) +
  theme_minimal()
