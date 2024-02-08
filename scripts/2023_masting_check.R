#*****# A scriopt to visualize and explore masting 
#***#Brendon Reidy 2/24



library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)
library(gganimate)
library(dplyr)
library(transformr)
## Individual file pathing may differ
path.google <- "~/Google Drive/My Drive" # Mac
path.dat <- file.path(path.google,"/LivingCollections_Phenology/Data_Observations")
#path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2023_02_EndOfYear_Report/figures_2023_end")
# this is for google -># 
path.figs <- "~/Google Drive/My Drive/LivingCollections_Phenology/Reports/2023_02_End_Of_Year_Report/figures_2023_end"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
## =^.^= ## # =^.^= # will need all this after checking up from the individual level

#Reading in the historic data for the previous years 
# 
# dat.22 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2022_FINAL.csv"))
# dat.21 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2021_FINAL.csv"))
# dat.20<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2020_FINAL.csv"))
# dat.19 <- read.csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2019_FINAL.csv"))
# dat.18 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2018_FINAL.csv"))
# #reading in 2023 data
# acer23<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Acer_2023_FINAL.csv"))
## =^.^= ## # =^.^= # will need all this after checking up from the individual level
# quercus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2023_FINAL.csv"))
# ulmus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Ulmus_2023_FINAL.csv"))
# ##binding 
# dat.23<- rbind(ulmus23, quercus23, acer23)
# 
# ##one line to bind them all
# dat.all <- rbind(dat.23, dat.22, dat.21, dat.20, dat.19, dat.18)
# 
# #getting the correct date format
# dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
# dat.all$Date <- as.Date(paste0("2018-", dat.all$yday), format="%Y-%j")
# 
# ##werid 2027 value
# summary(dat.all)
# #removing the "Q.macrocarpa" collection because that is not necessary for the end of year pheology report 
# dat.all <- dat.all[!(dat.all$Collection %in% c("Q. macrocarpa", "Tilia")), ]
# 
# #Checking what is present in the collection column to see if it's been removed
# unique(dat.all$Collection)
# 
# #Setting a spring only data frame because we did not make observations in spring 2020
# dat.spring <- dat.all[dat.all$Year != "2020", ]
# #further excluding all elm observations for 2021 since we did not observe phenology that spring
# dat.spring <- dat.spring[!(dat.spring$Collection == "Ulmus" & dat.spring$Year == "2021"), ]
# ##checking unique values of the Year and Collection column in dat.spring
# unique(dat.spring$Year)
# unique(dat.spring$Collection)


#Reading in the historic data for the previous years 
# 
dat.22 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2022_FINAL.csv"))
dat.21 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2021_FINAL.csv"))
dat.20<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2020_FINAL.csv"))
dat.19 <- read.csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2019_FINAL.csv"))
dat.18 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2018_FINAL.csv"))
#reading quercus 2023 data
quercus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2023_FINAL.csv"))

#One ring to bind them
dat.all <- rbind(quercus23, dat.22, dat.21, dat.20, dat.19, dat.18)

#getting the correct date format
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
dat.all$Date <- as.Date(paste0("2018-", dat.all$yday), format="%Y-%j")
# Subsetting by quercus 
dat.qu <- dat.all[dat.all$Collection == "Quercus", ]
summary(dat.qu)



#Filter to find rows where PlantNumber appears only in every year
dat.these <- dat.qu %>%
  group_by(PlantNumber) %>%
  filter(n_distinct(Year) == 2023 - 2018 + 1) %>%
  distinct(PlantNumber)  # To keep only unique PlantNumbers


#filter the main data fram by the plant numbers we want
dat.qj  <- semi_join(dat.qu, dat.these, by = "PlantNumber")
summary(dat.qj)
#checking to make sure there is at least some difference in these.
length(dat.qj$PlantNumber)
length(dat.qu$PlantNumber)

#getting a single tree to start. Let's go for a white oak
dat.wo <- dat.qj[dat.qj$Species == "Quercus alba",]

#find the unique Plant numbers and # entries per Plant number
unique(dat.wo$PlantNumber)
table(dat.wo$PlantNumber)
#Now a single tree has been selected by one of the plant numbers above in this case 134-U*66
#create a data frame of just that tree

dat.1t <- dat.wo[dat.wo$PlantNumber == "134-U*66",]
#check to make sure there is only this plant number
unique(dat.1t$PlantNumber)
table(dat.1t$PlantNumber)
###graphing for this collapsed

#getting the phenophases information I need. 
dat.fpi <- dat.1t[dat.1t$fruit.present.observed=="Yes", c("Date.Observed", "PlantNumber", "Year", "Date", "yday", "fruit.present.intensity")]
summary(dat.fpi)

dat.fpi$Date.Observed <- as.Date(dat.fpi$Date.Observed)

#Setting my yday
dat.fpi$yday <- lubridate::yday(dat.fpi$Date.Observed)
dat.fpi <- dat.fpi [dat.fpi$yday>=150,]
summary(dat.fpi)


dat.fpi <- aggregate(yday ~ PlantNumber + Year + fruit.present.intensity + Date.Observed + Date , dat=dat.fpi, FUN=min, NA.rm=T)
summary(dat.fpi)

head(dat.fpi)

dat.fpi <- dat.fpi %>% filter(!fruit.present.intensity %in% c("0%","0"))

dat.fpi$yday <- lubridate::yday(dat.fpi$Date.Observed)
summary(dat.fpi)

dat.fpi$Date <- as.Date(paste0("2023-", dat.fpi$yday), format="%Y-%j")

ggplot(data=dat.fpi) +
  png(file.path(path.figs,"Q.alba_134-U*66_fruit_present_intensity.png"), height=4, width=6, units="in", res=320)+
  geom_histogram(alpha=1, bins= 50, aes(x=yday, fill=fruit.present.intensity)) +
  facet_grid(Year ~ ., scales="free_x") +
  theme_bw() +
  scale_x_continuous(breaks = seq(150, 365, by = 7)) +
  labs(title="Fruit Present Intensity in Q.alba 134-U*66 by year", x="Day of Year")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off ()

# ggplot(data=dat.fpi) +
#   geom_histogram(alpha=1, aes(x=Date, fill=fruit.present.intensity)) +
#   facet_grid(Year ~ ., scales="free_x") +
#   theme_bw() +
#   scale_x_date(date_breaks = "2 week", date_labels = "%b") + # Setting breaks for each month
#   labs(title="Fruit Present Intensity in Q.alba 134-U*66", x="Date of Year") 

# ggplot(data=dat.fpi) +
#   geom_density(alpha=0.5, aes(x=yday, fill=fruit.present.intensity)) +
#   facet_grid(Year ~ ., scales="free_y") +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(150, 365, by = 10)) +
#   labs(title="Fruit Present Intensity in Q.alba 134-U*66", x="Day of Year")  # Modified the x-axis label
# 

#getting the phenophases information I need. 
dat.fri <- dat.1t[dat.1t$fruit.drop.observed=="Yes", c("Date.Observed", "PlantNumber", "Year", "Date", "yday", "fruit.drop.intensity")]
summary(dat.fri)

dat.fri$Date.Observed <- as.Date(dat.fri$Date.Observed)

#Setting my yday
dat.fri$yday <- lubridate::yday(dat.fri$Date.Observed)
dat.fri <- dat.fri [dat.fri$yday>=0,]
summary(dat.fri)


dat.fri <- aggregate(yday ~ PlantNumber + Year + fruit.drop.intensity + Date.Observed + Date , dat=dat.fri, FUN=min, NA.rm=T)
summary(dat.fri)

head(dat.fri)

dat.fri <- dat.fri %>% filter(!fruit.drop.intensity %in% c("0%","0"))

dat.fri$yday <- lubridate::yday(dat.fri$Date.Observed)
summary(dat.fri)

dat.fri$Date <- as.Date(paste0("2023-", dat.fri$yday), format="%Y-%j")

ggplot(data=dat.fri) +
  png(file.path(path.figs,"Q.alba_134-U*66_fruit_drop_intensity.png"), height=4, width=6, units="in", res=320)+
  geom_histogram(alpha=1, bins= 50, aes(x=yday, fill=fruit.drop.intensity)) +
  facet_grid(Year ~ ., scales="free_x") +
  theme_bw() +
  scale_x_continuous(breaks = seq(150, 365, by = 7)) +
  labs(title="Fruit Ripe Intensity in Q.alba 134-U*66 by year", x="Day of Year")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# ggplot(data=dat.fri) +
#   geom_histogram(alpha=0.5, aes(x=Date, fill=fruit.drop.intensity)) +
#   facet_grid(Year ~ ., scales="free_x") +
#   theme_bw() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b") + # Setting breaks for each month
#   labs(title="Fruit Drop Intensity", x="Date of Year") 


# ggplot(data=dat.fri) +
#   geom_density(alpha=0.5, aes(x=yday, fill=fruit.drop.intensity)) +
#   facet_grid(Year ~ ., scales="free_y") +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(150, 365, by = 10)) +
#   labs(title="Fruit Drop Intensity in Q.alba 134-U*66", x="Day of Year")  # Modified the x-axis label

#################



### doing a different tree because that one is obnoxious- and by obnoxious I mean maybe missing some data.
#find the unique Plant numbers
unique(dat.wo$PlantNumber)
table(dat.wo$PlantNumber)
#Now a single tree has been selected by one of the plant numbers above in this case 684-33*1

dat.2t <- dat.wo[dat.wo$PlantNumber == "684-33*1",]
#check to make sure there is only this plant number
unique(dat.2t$PlantNumber)
table(dat.2t$PlantNumber)

#getting the phenophases information I need. 
dat.fpi <- dat.2t[dat.2t$fruit.present.observed=="Yes", c("Date.Observed", "PlantNumber", "Year", "Date", "yday", "fruit.present.intensity")]
summary(dat.fpi)

dat.fpi$Date.Observed <- as.Date(dat.fpi$Date.Observed)

#Setting my yday
dat.fpi$yday <- lubridate::yday(dat.fpi$Date.Observed)
dat.fpi <- dat.fpi [dat.fpi$yday>=0,]
summary(dat.fpi)


dat.fpi <- aggregate(yday ~ PlantNumber + Year + fruit.present.intensity + Date.Observed + Date , dat=dat.fpi, FUN=min, NA.rm=T)
summary(dat.fpi)

head(dat.fpi)

dat.fpi <- dat.fpi %>% filter(!fruit.present.intensity %in% c("0%","0"))

dat.fpi$yday <- lubridate::yday(dat.fpi$Date.Observed)
summary(dat.fpi)

dat.fpi$Date <- as.Date(paste0("2023-", dat.fpi$yday), format="%Y-%j")


ggplot(data=dat.fpi) +
  png(file.path(path.figs,"Q.alba_682-33*1_fruit_present_intensity.png"), height=4, width=6, units="in", res=320)+
  geom_histogram(alpha=1, bins=50, aes(x=yday, fill=fruit.present.intensity)) +
  facet_grid(Year ~ ., scales="free_x") +
  theme_bw() +
  scale_x_continuous(breaks = seq(150, 365, by = 7)) +
  labs(title="Fruit Present Intensity in Q.alba 684-33*1 by year", x="Day of Year")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# ggplot(data=dat.fpi) +
#   geom_histogram(alpha=0.5, aes(x=Date, fill=fruit.present.intensity)) +
#   facet_grid(Year ~ ., scales="free_x") +
#   theme_bw() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b") + # Setting breaks for each month
#   labs(title="Fruit Present Intensity", x="Date of Year") 

# ggplot(data=dat.fpi) +
#   geom_density(alpha=0.5, bins=50, aes(x=yday, fill=fruit.present.intensity)) +
#   facet_grid(Year ~ ., scales="free_y") +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(150, 365, by = 10)) +
#   labs(title="Fruit Present Intensity in Q.alba 684-33*1", x="Day of Year")  # Modified the x-axis label
# 

#getting the phenophases information I need. 
dat.fri <- dat.2t[dat.2t$fruit.drop.observed=="Yes", c("Date.Observed", "PlantNumber", "Year", "Date", "yday", "fruit.drop.intensity")]
summary(dat.fri)

dat.fri$Date.Observed <- as.Date(dat.fri$Date.Observed)

#Setting my yday
dat.fri$yday <- lubridate::yday(dat.fri$Date.Observed)
dat.fri <- dat.fri [dat.fri$yday>=0,]
summary(dat.fri)


dat.fri <- aggregate(yday ~ PlantNumber + Year + fruit.drop.intensity + Date.Observed + Date , dat=dat.fri, FUN=min, NA.rm=T)
summary(dat.fri)

head(dat.fri)

dat.fri <- dat.fri %>% filter(!fruit.drop.intensity %in% c("0%","0"))

dat.fri$yday <- lubridate::yday(dat.fri$Date.Observed)
summary(dat.fri)

dat.fri$Date <- as.Date(paste0("2023-", dat.fri$yday), format="%Y-%j")


ggplot(data=dat.fri) +
  png(file.path(path.figs,"Q.alba_682-33*1_fruit_drop_intensity.png"), height=4, width=6, units="in", res=320)+
  geom_histogram(alpha=1, bins=50, aes(x=yday, fill=fruit.drop.intensity)) +
  facet_grid(Year ~ ., scales="free_x") +
  theme_bw() +
  scale_x_continuous(breaks = seq(150, 365, by = 7)) +
  labs(title="Fruit Drop Intensity in Q.alba 684-33*1 by year", x="Day of Year")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# ggplot(data=dat.fri) +
#   geom_histogram(alpha=0.5, aes(x=Date, fill=fruit.drop.intensity)) +
#   facet_grid(Year ~ ., scales="free_x") +
#   theme_bw() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b") + # Setting breaks for each month
#   labs(title="Fruit Drop Intensity", x="Date of Year") 

# ggplot(data=dat.fri) +
#   geom_density(alpha=0.5, aes(x=yday, fill=fruit.drop.intensity)) +
#   facet_grid(Year ~ ., scales="free_y") +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(150, 365, by = 7)) +
#   labs(title="Fruit Present Intensity in Q.alba 684-33*1", x="Day of Year")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggplot(data = dat.fri) +
#   geom_point(aes(x = Date, y = fruit.drop.intensity, color = fruit.drop.intensity)) +
#   facet_grid(Year ~ .) +
#   theme_bw() +
#   labs(title = "Date of  Fruit Drop Intensity in Q.alba 684-33*1", x = "Date", y = "Fruit Drop Intensity", fill = "Intensity")+
#   scale_x_date(date_breaks = "1 day", date_labels = "%b %d")



### doing this for white oak in general now so I can just use the dat.wo data frame

#getting the phenophases information I need. 
dat.fpi <- dat.wo[dat.wo$fruit.present.observed=="Yes", c("Date.Observed", "PlantNumber", "Year", "Date", "yday", "fruit.present.intensity")]
summary(dat.fpi)

dat.fpi$Date.Observed <- as.Date(dat.fpi$Date.Observed)

#Setting my yday
dat.fpi$yday <- lubridate::yday(dat.fpi$Date.Observed)
summary(dat.fpi)


dat.fpi <- aggregate(yday ~ PlantNumber + Year + fruit.present.intensity + Date.Observed + Date , dat=dat.fpi, FUN=min, NA.rm=T)
summary(dat.fpi)

head(dat.fpi)

dat.fpi <- dat.fpi %>% filter(!fruit.present.intensity %in% c("0%","0"))

dat.fpi$yday <- lubridate::yday(dat.fpi$Date.Observed)
summary(dat.fpi)

dat.fpi$Date <- as.Date(paste0("2023-", dat.fpi$yday), format="%Y-%j")


ggplot(data=dat.fpi) +
  png(file.path(path.figs,"Q.alba_fruit_present_intensity.png"), height=4, width=6, units="in", res=320)+
  geom_histogram(alpha=1, bins= 50,aes(x=yday, fill=fruit.present.intensity)) +
  facet_grid(Year ~ ., scales="free_x") +
  theme_bw() +
  scale_x_continuous(breaks = seq(150, 365, by = 7)) +
  labs(title="Fruit Present Intensity in Q.alba by year ", x="Day of Year")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# ggplot(data=dat.fpi) +
#   geom_histogram(alpha=0.5, aes(x=Date, fill=fruit.present.intensity)) +
#   facet_grid(Year ~ ., scales="free_x") +
#   theme_bw() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b") + # Setting breaks for each month
#   labs(title="Fruit Present Intensity", x="Date of Year") 

# ggplot(data=dat.fpi) +
#   geom_density(alpha=0.5, aes(x=yday, fill=fruit.present.intensity)) +
#   facet_grid(Year ~ ., scales="free_y") +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(150, 365, by = 10)) +
#   labs(title="Fruit Present Intensity in Q.alba 684-33*1", x="Day of Year")  # Modified the x-axis label

#getting the phenophases information I need. 
dat.fri <- dat.wo[dat.wo$fruit.drop.observed=="Yes", c("Date.Observed", "PlantNumber", "Year", "Date", "yday", "fruit.drop.intensity")]
summary(dat.fri)

dat.fri$Date.Observed <- as.Date(dat.fri$Date.Observed)

#Setting my yday
dat.fri$yday <- lubridate::yday(dat.fri$Date.Observed)
summary(dat.fri)


dat.fri <- aggregate(yday ~ PlantNumber + Year + fruit.drop.intensity + Date.Observed + Date , dat=dat.fri, FUN=min, NA.rm=T)
summary(dat.fri)

head(dat.fri)

dat.fri <- dat.fri %>% filter(!fruit.drop.intensity %in% c("0%","0"))

dat.fri$yday <- lubridate::yday(dat.fri$Date.Observed)
summary(dat.fri)

dat.fri$Date <- as.Date(paste0("2023-", dat.fri$yday), format="%Y-%j")

ggplot(data=dat.fri) +
  png(file.path(path.figs,"Q.alba_fruit_drop_intensity.png"), height=4, width=6, units="in", res=320)+
  geom_histogram(alpha=1, bins = 50, aes(x=yday, fill=fruit.drop.intensity)) +
  facet_grid(Year ~ ., scales="free_x") +
  theme_bw() +
  scale_x_continuous(breaks = seq(150, 365, by = 7)) +
  labs(title="Fruit Drop Intensity in Q.alba by year", x="Day of Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# ggplot(data=dat.fri) +
#   geom_histogram(alpha=0.5, aes(x=Date, fill=fruit.drop.intensity)) +
#   facet_grid(Year ~ ., scales="free_x") +
#   theme_bw() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b") + # Setting breaks for each month
#   labs(title="Fruit Drop Intensity", x="Date of Year") 

# ggplot(data=dat.fri) +
#   geom_density(alpha=1, bins=50, aes(x=yday, fill=fruit.drop.intensity)) +
#   facet_grid(Year ~ ., scales="free_y") +
#   theme_bw() +
#   scale_x_continuous(breaks = seq(150, 365, by = 7)) +
#   labs(title="Fruit Present Intensity in Q.alba", x="Day of Year")+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggplot(data = dat.fri) +
#   geom_point(aes(x = Date, y = fruit.drop.intensity, color = fruit.drop.intensity)) +
#   facet_grid(Year ~ .) +
#   theme_bw() +
#   labs(title = "Date of  Fruit Drop Intensity in Q.alba 684-33*1", x = "Date", y = "Fruit Drop Intensity", fill = "Intensity")+
#   scale_x_date(date_breaks = "1 day", date_labels = "%b %d")
