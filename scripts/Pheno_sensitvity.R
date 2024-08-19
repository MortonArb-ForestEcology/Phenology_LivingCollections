`# ---------------------------------
# 0. Set up some general file paths
# ---------------------------------
#---------------------------------
library(ggplot2)
library(gghighlight)
library(readr)
library(lubridate)
library(tidyverse)
library(gganimate)
library(dplyr)
library(transformr)``
path.google <- "G:/My Drive" # Windows
#path.google <- "/Volumes/GoogleDrive/My Drive" # Mac
path.dat <- file.path(path.google,"LivingCollections_Phenology/Data_Observations")
#path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2023_02_EndOfYear_Report/figures_2023_end")
# this is for google -># 
path.figs <- "~/Google Drive/My Drive/LivingCollections_Phenology/Reports/2024_01_Mid_Year_Report/figures_2024"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")
#Jenk path
path.dot <- file.path("G:/.shortcut-targets-by-id/0B_Fbr697pd36WEJBQmRHbVRIZUE/LivingCollections_Phenology/Data_Observations/")


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
  dat$GDD2 <- ifelse(dat$TMEAN>2, dat$TMEAN-2, 0)
  dat$GDD5 <- ifelse(dat$TMEAN>5, dat$TMEAN-5, 0)
  dat$CDD0 <- ifelse(dat$YDAY>172 & dat$TMEAN<0, 0-dat$TMEAN, 0)
  dat$CDD2 <- ifelse(dat$YDAY>172 & dat$TMEAN< -2, -2-dat$TMEAN, 0)
  dat$DaysNoRain <- NA
  
  dat[, c("GDD0.cum", "GDD5.cum", "CDD0.cum", "CDD2.cum", "PRCP.cum", "GDD2.cum")] <- cumsum(dat[, c("GDD0", "GDD5", "CDD0", "CDD2", "PRCP","GDD2")])
  
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

#creating a data frame of just the last 6 years of weather data
dat.ghcn3 <- dat.ghcn2[dat.ghcn2$YEAR>=2018,]
#adjusting to only have 2023
dat.ghcn3 <- dat.ghcn3[dat.ghcn3$YEAR<=2023,]

#making sure the date being shown only shows spring dates
dat.ghcn33 <- dat.ghcn3[dat.ghcn3$YDAY<=359,]
summary(dat.ghcn33)
head(dat.ghcn33)

#Subsetting out uncecessary columns for the phenology report
dat.ghcn4 <- dat.ghcn33[ ,c("YEAR","MONTH", "TMAX", "TMIN","PRCP", "DATE", "YDAY", "TMEAN", "PRCP.cum", "GDD5.cum", "GDD2.cum", "GDD2","GDD5")]
summary(dat.ghcn4)
head(dat.ghcn4)

##########################################
###Read in Pheno data
##########################################
dat.22 <- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_All_2022_FINAL.csv"))
dat.21 <- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_All_2021_FINAL.csv"))
dat.20<- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_All_2020_FINAL.csv"))
dat.19 <- read.csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_All_2019_FINAL.csv"))
dat.18 <- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_All_2018_FINAL.csv"))
#reading in 2023 data
acer23<- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_Acer_2023_FINAL.csv"))
quercus23 <- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_Quercus_2023_FINAL.csv"))
ulmus23 <- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_Ulmus_2023_FINAL.csv"))
#reading in 2024 data
acer24<- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_Acer_2024_Spring.csv"))
quercus24 <- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_Quercus_2024_Spring.csv"))
ulmus24 <- read_csv(file.path(path.dot,"LivingCollectionPhenology_ObservationData_Ulmus_2024_Spring.csv"))
##binding 
dat.23<- rbind(ulmus23, quercus23, acer23)
dat.24<- rbind(ulmus24, quercus24, acer24)
##one line to bind them all
dat.all <- rbind(dat.24,dat.23, dat.22, dat.21, dat.20, dat.19, dat.18)

#getting the correct date format
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
dat.all$Date <- as.Date(paste0("2018-", dat.all$yday), format="%Y-%j")

##werid 2027 value
summary(dat.all)
#removing the "Q.macrocarpa" collection because that is not necessary for the end of year pheology report 
dat.all <- dat.all[!(dat.all$Collection %in% c("Q. macrocarpa", "Tilia")), ]

#Checking what is present in the collection column to see if it's been removed
unique(dat.all$Collection)

#Setting a spring only data frame because we did not make observations in spring 2020
dat.spring <- dat.all[dat.all$Year != "2020", ]
#further excluding all elm observations for 2021 since we did not observe phenology that spring
dat.spring <- dat.spring[!(dat.spring$Collection == "Ulmus" & dat.spring$Year == "2021"), ]
##checking unique values of the Year and Collection column in dat.spring
unique(dat.spring$Year)
unique(dat.spring$Collection)
##somewhere we have a year listed as 2027..come back to this. 
dat.huh <- dat.all[dat.all$Year == "2027", ]
head(dat.huh)
#it's dale in 12/11/ 2022

#### doing just Quercus macrocarpa
quma <- dat.all[dat.all$Species=="Quercus macrocarpa",]
summary(quma)
unique(quma$Species)


#creating a data frame of just flowers present observed in Quercus marcocarpa
quma.fo <- quma[quma$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(quma.fo)
unique(quma.fo$PlantNumber)
# Adding yday to quma.fo
quma.fo$yday <- lubridate::yday(quma.fo$Date.Observed)
summary(quma.fo)
qual.fo <- qual.fo[qual.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged <- merge(quma.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")

# Fit linear model
model <- lm(yday ~ GDD2.cum, data = dat.merged)

# Summary of the model
summary(model)

# Extracting the slope
slope <- coef(model)["GDD2.cum"]
slope

# Visualize the relationship
ggplot(dat.merged, aes(x = GDD2.cum, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity, First Open Flower, Q.macrocarpa",
       x = "Cumulative GDD2",
       y = "First Flower, DOY") +
  theme_minimal()

#### doing just Quercus alba
quab <- dat.all[dat.all$Species=="Quercus alba",]
summary(quab)
unique(quab$Species)


#creating a data frame of just flowers present observed in Quercus alba
quab.fo <- quab[quab$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(quab.fo)
unique(quab.fo$PlantNumber)

# Adding yday to quab.fo
quab.fo$yday <- lubridate::yday(quab.fo$Date.Observed)
summary(quab.fo)
quab.fo <- quab.fo[quab.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged1 <- merge(quab.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")

# Fit linear model
model <- lm(yday ~ GDD2, data = dat.merged1)

# Summary of the model
summary(model)

# Extracting the slope
slope <- coef(model)["GDD2.cum"]
slope

# Visualize the relationship
ggplot(dat.merged1, aes(x = GDD2.cum, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity, First Open Flower Q.alba",
       x = "Cumulative GDD2",
       y = "First Flower,DOY") +
  theme_minimal()

#### doing all Quercus 
dat.qu <- dat.spring [dat.spring$Collection=="Quercus",]
summary(dat.qu)
unique(dat.qu$Collection)


#creating a data frame of just flowers present observed 
dat.qufo <- dat.qu[dat.qu$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(dat.qufo)

# Adding yday to dat.qufo
dat.qufo$yday <- lubridate::yday(dat.qufo$Date.Observed)
summary(dat.qufo)

## cutting off dates because oaks shouldn't be flowering in fall
dat.qufo <- dat.qufo[dat.qufo$yday<=152,]


# Merging phenology data with meteorological data based on Date
dat.merged2 <- merge(dat.qufo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")

# Fit linear model
model <- lm(yday ~ GDD2.cum, data = dat.merged2)

# Summary of the model
summary(model)

# Extracting the slope
slope <- coef(model)["GDD2.cum"]
slope

# Visualize the relationship
ggplot(dat.merged2, aes(x = GDD2.cum, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity, First Flower Quercus",
       x = "Cumulative GDD2",
       y = "First Flower, DOY") +
  theme_minimal()
#############################################################################
####doing GDD5

#### doing just Quercus macrocarpa
quma <- dat.all[dat.all$Species=="Quercus macrocarpa",]
summary(quma)
unique(quma$Species)


#creating a data frame of just flowers present observed in Quercus marcocarpa
quma.fo <- quma[quma$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(quma.fo)

# Adding yday to quma.fo
quma.fo$yday <- lubridate::yday(quma.fo$Date.Observed)
summary(quma.fo)
quma.fo <- quma.fo[quma.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged3 <- merge(quma.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")

# Fit linear model
model <- lm(yday ~ GDD5.cum, data = dat.merged3)

# Summary of the model
summary(model)

# Extracting the slope
slope <- coef(model)["GDD5.cum"]
slope

# Visualize the relationship
ggplot(dat.merged3, aes(x = GDD5.cum, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity, First Open Flower Q.macrocarpa",
       x = "Cumulative GDD5",
       y = "First Flower, DOY") +
  theme_minimal()

#### doing just Quercus alba
quab <- dat.all[dat.all$Species=="Quercus alba",]
summary(quab)
unique(quab$Species)


#creating a data frame of just flowers present observed in Quercus marcocarpa
quab.fo <- quab[quab$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(quab.fo)

# Adding yday to quab.fo
quab.fo$yday <- lubridate::yday(quab.fo$Date.Observed)
summary(quab.fo)
quab.fo <- quab.fo[quab.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged4 <- merge(quab.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")

# Fit linear model
model <- lm(yday ~ GDD5, data = dat.merged4)

# Summary of the model
summary(model)

# Extracting the slope
slope <- coef(model)["GDD5.cum"]
slope

# Visualize the relationship
ggplot(dat.merged4, aes(x = GDD5.cum, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity, First Open Flower,Q.alba",
       x = "Cumulative GDD5",
       y = "First Flower, DOY") +
  theme_minimal()

#### doing all Quercus 
dat.qu <- dat.spring [dat.spring$Collection=="Quercus",]
summary(dat.qu)
unique(dat.qu$Collection)


#creating a data frame of just flowers present observed 
dat.qufo <- dat.qu[dat.qu$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(dat.qufo)

# Adding yday to dat.qufo
dat.qufo$yday <- lubridate::yday(dat.qufo$Date.Observed)
summary(dat.qufo)
dat.qufo <- dat.qufo[dat.qufo$yday<=152,]
## cutting off dates because oaks shouldn't be flowering in fall
dat.qufo <- dat.qufo[dat.qufo$yday<=152,]


# Merging phenology data with meteorological data based on Date
dat.merged5 <- merge(dat.qufo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")

# Fit linear model
model <- lm(yday ~ GDD5.cum, data = dat.merged5)

# Summary of the model
summary(model)

# Extracting the slope
slope <- coef(model)["GDD5.cum"]
slope

# Visualize the relationship
ggplot(dat.merged5, aes(x = GDD5.cum, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity, First Open Flower, Quercus",
       x = "Cumulative GDD5",
       y = "First Flower, DOY") +
  theme_minimal()
#################################
###Attempting with GDD up to the  point of flowering
##################################



#### doing just Quercus macrocarpa
quma <- dat.all[dat.all$Species=="Quercus macrocarpa",]
summary(quma)
unique(quma$Species)


#creating a data frame of just flowers present observed in Quercus marcocarpa
quma.fo <- quma[quma$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(quma.fo)

# Adding yday to quma.fo
quma.fo$yday <- lubridate::yday(quma.fo$Date.Observed)
summary(quma.fo)
quma.fo <- quma.fo[quma.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged6 <- merge(quma.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")
#only selecting GDD2 up to the point of flowering
dat.merged7 <- dat.merged6 %>% group_by(PlantNumber) %>% mutate(GDD2.cumu2f = cumsum(GDD2))

# Use the cumulative GDD2 up to the point of flowering in the model
model_cumulative <- lm(yday ~ GDD2.cumu2f, data = dat.merged7)

# Summary of the model
summary(model_cumulative)
slope <- coef(model_cumulative)["GDD2.cumu2f"]
slope

# Visualize the relationship
ggplot(dat.merged7, aes(x = GDD2.cumu2f, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity,First Flower, Q.macrocarpa",
       x = "Cumulative GDD2 up to Flowering",
       y = "Day of Year (First Flower)") +
  theme_minimal()


#### doing just Quercus alba
qual <- dat.all[dat.all$Species=="Quercus alba",]
summary(qual)
unique(qual$Species)


#creating a data frame of just flowers present observed in Quercus marcocarpa
qual.fo <- qual[qual$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(qual.fo)

# Adding yday to qual.fo
qual.fo$yday <- lubridate::yday(qual.fo$Date.Observed)
summary(qual.fo)
qual.fo <- qual.fo[qual.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged8 <- merge(qual.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")
#only selecting GDD2 up to the point of flowering
dat.merged9 <- dat.merged8 %>% group_by(PlantNumber) %>% mutate(GDD2.cumu2f = cumsum(GDD2))

# Use the cumulative GDD2 up to the point of flowering in the model
model_cumulative <- lm(yday ~ GDD2.cumu2f, data = dat.merged9)

# Summary of the model
summary(model_cumulative)
slope <- coef(model_cumulative)["GDD2.cumu2f"]
slope
# Visualize the relationship
ggplot(dat.merged9, aes(x = GDD2.cumu2f, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity,First Flower, Q.alba",
       x = "Cumulative GDD2 up to Flowering",
       y = "Day of Year (First Flower)") +
  theme_minimal()


#### doing Quercus 
dat.allq <- dat.all[dat.all$Collection=="Quercus",]
summary(dat.allq)
unique(dat.allq$Species)


#creating a data frame of just flowers present observed in Quercus marcocarpa
dat.allq.fo <- dat.allq[dat.allq$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(dat.allq.fo)

# Adding yday to dat.allq.fo
dat.allq.fo$yday <- lubridate::yday(dat.allq.fo$Date.Observed)
summary(dat.allq.fo)
dat.allq.fo <- dat.allq.fo[dat.allq.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged10 <- merge(dat.allq.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")
#only selecting GDD2 up to the point of flowering
dat.merged11 <- dat.merged10 %>% group_by(PlantNumber) %>% mutate(GDD2.cumu2f = cumsum(GDD2))

# Use the cumulative GDD2 up to the point of flowering in the model
model_cumulative <- lm(yday ~ GDD2.cumu2f, data = dat.merged11)

# Summary of the model
summary(model_cumulative)
slope <- coef(model_cumulative)["GDD2.cumu2f"]
slope

# Visualize the relationship
ggplot(dat.merged11, aes(x = GDD2.cumu2f, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity,First Flower, Q.alba",
       x = "Cumulative GDD2 up to Flowering",
       y = "Day of Year (First Flower)") +
  theme_minimal()

#### 
##GDD5
####




#### doing just Quercus macrocarpa
quma <- dat.all[dat.all$Species=="Quercus macrocarpa",]
summary(quma)
unique(quma$Species)


#creating a data frame of just flowers present observed in Quercus marcocarpa
quma.fo <- quma[quma$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(quma.fo)

# Adding yday to quma.fo
quma.fo$yday <- lubridate::yday(quma.fo$Date.Observed)
summary(quma.fo)
quma.fo <- quma.fo[quma.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged12 <- merge(quma.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")
#only selecting GDD5 up to the point of flowering
dat.merged13 <- dat.merged12 %>% group_by(PlantNumber) %>% mutate(GDD5.cumu2f = cumsum(GDD5))

# Use the cumulative GDD5 up to the point of flowering in the model
model_cumulative <- lm(yday ~ GDD5.cumu2f, data = dat.merged13)

# Summary of the model
summary(model_cumulative)
slope <- coef(model_cumulative)["GDD2.cumu2f"]
slope
# Visualize the relationship
ggplot(dat.merged13, aes(x = GDD5.cumu2f, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity,First Flower, Q.macrocarpa",
       x = "Cumulative GDD5 up to Flowering",
       y = "Day of Year (First Flower)") +
  theme_minimal()


#### doing just Quercus alba
qual <- dat.all[dat.all$Species=="Quercus alba",]
summary(qual)
unique(qual$Species)


#creating a data frame of just flowers present observed in Quercus marcocarpa
qual.fo <- qual[qual$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(qual.fo)

# Adding yday to qual.fo
qual.fo$yday <- lubridate::yday(qual.fo$Date.Observed)
summary(qual.fo)
qual.fo <- qual.fo[qual.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged14 <- merge(qual.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")
#only selecting GDD5 up to the point of flowering
dat.merged15 <- dat.merged14 %>% group_by(PlantNumber) %>% mutate(GDD5.cumu2f = cumsum(GDD5))

# Use the cumulative GDD5 up to the point of flowering in the model
model_cumulative <- lm(yday ~ GDD5.cumu2f, data = dat.merged15)

# Summary of the model
summary(model_cumulative)
slope <- coef(model_cumulative)["GDD2.cumu2f"]
slope

# Visualize the relationship
ggplot(dat.merged15, aes(x = GDD5.cumu2f, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity,First Flower, Q.alba",
       x = "Cumulative GDD5 up to Flowering",
       y = "Day of Year (First Flower)") +
  theme_minimal()


#### doing Quercus 
dat.allq <- dat.all[dat.all$Collection=="Quercus",]
summary(dat.allq)
unique(dat.allq$Species)


#creating a data frame of just flowers present observed in Quercus marcocarpa
dat.allq.fo <- dat.allq[dat.allq$flower.open.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "flower.open.observed")]
summary(dat.allq.fo)

# Adding yday to dat.allq.fo
dat.allq.fo$yday <- lubridate::yday(dat.allq.fo$Date.Observed)
summary(dat.allq.fo)
dat.allq.fo <- dat.allq.fo[dat.allq.fo$yday<=152,]
# Merging phenology data with meteorological data based on Date
dat.merged16 <- merge(dat.allq.fo, dat.ghcn4, by.x = "Date.Observed", by.y = "DATE")
#only selecting GDD5 up to the point of flowering
dat.merged17 <- dat.merged16 %>% group_by(PlantNumber) %>% mutate(GDD5.cumu2f = cumsum(GDD5))

# Use the cumulative GDD5 up to the point of flowering in the model
model_cumulative <- lm(yday ~ GDD5.cumu2f, data = dat.merged17)

# Summary of the model
summary(model_cumulative)
slope <- coef(model_cumulative)["GDD2.cumu2f"]
slope

# Visualize the relationship
ggplot(dat.merged17, aes(x = GDD5.cumu2f, y = yday)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Phenological Sensitivity,First Flower, Q.alba",
       x = "Cumulative GDD5 up to Flowering",
       y = "Day of Year (First Flower)") +
  theme_minimal()
