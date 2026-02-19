# A new script with some combined collections analyses/graphs for the end of the year report
library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)
library(gganimate)
library(dplyr)
library(transformr)


# 1. Pathing -----------------------------------------------------------------
path.google <- "~/Google Drive/My Drive" # Mac
path.dat <- file.path(path.google,"/LivingCollections_Phenology/Data_Observations")
#path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2023_02_EndOfYear_Report/figures_2023_end")
# this is for google -># 
path.figs <- "~/Google Drive/My Drive/LivingCollections_Phenology/Reports/2025_02_End_Of_Year_Report/figures_2025_end"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")


# 2. Arb Data------


##* Reading in the historic data for the previous years----

dat.22 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2022_FINAL.csv"))
dat.21 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2021_FINAL.csv"))
dat.20<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2020_FINAL.csv"))
dat.19 <- read.csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2019_FINAL.csv"))
dat.18 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2018_FINAL.csv"))
#reading in 2023 data
acer23<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Acer_2023_FINAL.csv"))
quercus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2023_FINAL.csv"))
ulmus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Ulmus_2023_FINAL.csv"))
##binding 
dat.23<- rbind(ulmus23, quercus23, acer23)

#reading in 2024 data
acer24<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Acer_2024_FINAL.csv"))
quercus24 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2024_FINAL.csv"))
ulmus24 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Ulmus_2024_FINAL.csv"))
##binding 
dat.24<- rbind(ulmus24, quercus24, acer24)

#reading in 2025 data
acer25<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Acer_2025_FINAL.csv"))
quercus25 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2025_FINAL.csv"))
ulmus25 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Ulmus_2025_FINAL.csv"))
##binding 
dat.25<- rbind(ulmus25, quercus25, acer25)

##one line to bind them all
dat.all <- rbind(dat.25,dat.24,dat.23, dat.22, dat.21, dat.20, dat.19, dat.18)


##* Formatting date and checks----
#getting the correct date format
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
dat.all$Date <- as.Date(paste0("2018-", dat.all$yday), format="%Y-%j")

##werid 2027 value
summary(dat.all)
#removing the "Q.macrocarpa" collection because that is not necessary for the end of year pheology report 
dat.all <- dat.all[!(dat.all$Collection %in% c("Q. macrocarpa", "Tilia")), ]

#Checking what is present in the collection column to see if it's been removed
unique(dat.all$Collection)

##* Setting a spring only data frame---- 
#because we did not make observations in spring 2020
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

# 3. Colored leaves Present----
#Getting a graph of colored leaf observations



dat.lc <- dat.all[dat.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.color.observed","Date", "Collection")]
dat.lc <- dat.lc[!is.na(dat.lc$PlantNumber),]
summary(dat.lc)
head(dat.lc)

#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.lc$Date.Observed)
max(dat.lc$Date.Observed)
range(dat.lc$Date.Observed)
mean(dat.lc$Date.Observed,na.rm=T)

#Now make my Yday
dat.lc$yday <- lubridate::yday(dat.lc$Date.Observed)
summary(dat.lc)


#only looking at trees that showed fall color from 9/1 on before day 350
dat.llc <- dat.lc[dat.lc$yday >= 100 & dat.lc$yday <= 350, ]
summary(dat.llc)

#aggregating dat.llc so it shows me the date of first leaf color for every plant number and species 
leaf.color <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.llc, FUN=min, na.rm=T)
summary(leaf.color)
head(leaf.color)

leaf.color$Date <- as.Date(paste0("2023-", leaf.color$yday), format="%Y-%j")

##* Graphing####
ggplot(data=leaf.color) +
  facet_grid(Collection~ .,scales="free_y") +
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442", "2023"="purple3","2024"="orange","2025"="red" )) +
  scale_color_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442", "2023"= "purple3","2024"="orange","2025"="red")) +
  theme_bw() +
  labs(title="Mean Day of First Leaf Color Present", x="Date", fill="Year")
dev.off()


ggplot(data=leaf.color) +
  facet_grid(Collection~ .,scales="free_y") +
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442", "2023"="purple3" )) +
  scale_color_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442", "2023"= "purple3")) +
  theme_bw() +
  labs(title="Mean Day of First Leaf Color Present", x="Date", fill="Year")
dev.off()
###Highlight
ggplot(data=leaf.color) +
  png(file.path(path.figs,"All_First_Leaf_Color_2023_highlight.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection ~ ., scales="free_y") +
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Mean Day of First Leaf Color Present", x="Date", fill="Year")
dev.off()

ggplot(data=leaf.color) +
  facet_grid(Collection ~ ., scales="free_y") +
  geom_line(aes(x=Date, y = after_stat(count), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +
  scale_color_manual(name="Year", values=c("2024"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Count of 'Yes' over Time", x="Date", color="Year", y="Count of 'Yes'")

###** Historical mean----

###Plotting 2024 vs historic dates of first leaf color
#Selecting out 2024
historical_mean <- leaf.color[leaf.color$Year != "2025", ] %>%group_by(Collection, yday) %>%
summarise(mean_density = mean(n(), na.rm = TRUE))

# Graphing 2024 to the historical mean
ggplot() +
  png(file.path(path.figs,"First_Leaf_Color_Historic.png"), height=5, width=7, units="in", res=320)+
  facet_grid(Collection ~ ., scales = "free_y") +
  # Add the historical mean as a gray band
  geom_density(data = leaf.color[leaf.color$Year != "2025", ], aes(x = Date),fill = "gray80",  alpha = 0.25) +
  geom_density(data = leaf.color[leaf.color$Year == "2025", ], aes(x = Date), fill = "blue",alpha = 0.25) +
  scale_x_date(date_labels = "%b %d",date_breaks = "1 month") +
  theme_bw() +
  labs(title = "2025 Leaf Color Timing vs. Historical Pattern (2018-2024)", x = "Date", y = "Density") +
  theme(legend.position = "top")
dev.off()
####


ggplot(data=leaf.color) +
  # png(file.path(path.figs,"All_First_Leaf_Color_hist.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection~ . ) + # This is the code that will stack everything
  geom_histogram(alpha=0.5, binwidth =10, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2024"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2024"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Leaf Color Present", x="Day of Year")
dev.off()

#4. Leaf color instensity ---- 
### getting leaf color intensity
dat.lci <- dat.all[dat.all$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.intensity", "Collection")]
summary(dat.lci)
dat.lci <- dat.lci[!is.na(dat.lci$PlantNumber),]
summary(dat.lci)

#Checking to make sure date ranges are correct
min(dat.lci$Date.Observed)
max(dat.lci$Date.Observed)
mean(dat.lci$Date.Observed)
range(dat.lci$Date.Observed)

#Setting my yday
dat.lci$yday <- lubridate::yday(dat.lci$Date.Observed)
summary(dat.lci)

#setting my yday to only show dates later in the season and the current date
dat.lci <- dat.lci [dat.lci$yday>=200,]
#dat.lci <- dat.lci [dat.lci$yday<=Sys.Date(),]
summary(dat.lci)

#removing "0 and NA's
dat.lci <- aggregate(yday ~ PlantNumber + Species + Year + Collection + leaf.color.intensity + Date.Observed , dat=dat.lci, FUN=min, NA.rm=T)
summary(dat.lci)
head(dat.lci)

dat.lci$yday <- lubridate::yday(dat.lci$Date.Observed)
summary(dat.lci)

##* Graphing----
png(file.path(path.figs,"Leaf_Present_Intensity.png"), height=4, width=6, units="in", res=320)
ggplot(data=dat.lci) +
  geom_histogram(alpha=1.5, binwidth=10, aes(x=yday, fill=leaf.color.intensity)) +
  facet_grid(Collection ~ .) +
  scale_fill_discrete(
    name = "Leaf Color Intensity",
    limits = c("0%", "<5%", "5-24%", "25-49%", "50-74%", "75-94%", ">95%"),
    na.translate = FALSE
  ) +
  scale_x_continuous(
    breaks = c(213, 244, 274, 305, 335),
    labels = c("Aug 1", "Sep 1", "Oct 1", "Nov 1", "Dec 1")
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Leaf color Intensity", x="Date")
dev.off()



# Create a ggplot
ggplot(data = leaf.color, aes(x = Date, color = as.factor(Year))) + 
  geom_point(stat = "count", position = "stack", alpha = 0.25) +
  facet_grid(Collection ~ ., scales = "free_y") +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 week") +
  scale_fill_manual(name = "Year", values = c("2023" = "red", "others" = "gray"), guide = guide_legend(override.aes = list(fill = c("red")))) +
  scale_color_manual(name = "Year", values = c("2023" = "red", "others" = "gray")) +
  theme_bw() +
  labs(title = "Leaf Color Observations",
       x = "Date",
       y = "Number of 'Yes' Records") +
  theme_bw() +
  labs(title = "Leaf Color Observations",
       x = "Date",
       y = "Number of 'Yes' Records") +
  theme_bw() +
  transition_states(Date, transition_length = 2, state_length = 1) +
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, falloff = 'sine-in', exclude_phase = 'enter')


#5. Falling leaves----

dat.fl <- dat.all[dat.all$leaf.falling.observed=="Yes",  c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.falling.observed", "Date", "Collection")]
dat.fl <- dat.fl[!is.na(dat.fl$PlantNumber),]
summary(dat.fl)
head(dat.fl)

#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fl$Date.Observed)
max(dat.fl$Date.Observed)
range(dat.fl$Date.Observed)
mean(dat.fl$Date.Observed,na.rm=T)

#Now make my Yday
dat.fl$yday <- lubridate::yday(dat.fl$Date.Observed)
summary(dat.fl)


#only looking at trees that showed falling leavesin the last half of the year
dat.ffl <- dat.fl [dat.fl$yday>=120,]
summary(dat.ffl)

falling.leaves <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.ffl, FUN=min, na.rm=T)
summary(falling.leaves)
head(falling.leaves)

#putting date in the sbbreviated calendar way
falling.leaves$Date <- as.Date(paste0("2018-", falling.leaves$yday), format="%Y-%j")


##* Graphing----
ggplot(data=falling.leaves) +
#png(file.path(path.figs,"All_First_Falling_Leaf_dens_highlight.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection ~ .) + # This is the code that will stack everything
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Mean Day of Falling Leaves Present", x="Date", fill="Year")+
  theme_bw()

  dev.off()

#6. Breaking leaf buds----

dat.lb <- dat.spring[dat.spring$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.breaking.buds.observed","Date", "Collection")]
dat.lb <- dat.lb[!is.na(dat.lb$PlantNumber),]
summary(dat.lb)
head(dat.lb)

#finding the minimimum and maximum range and mean of the dates breaking leaf buds were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.lb$Date.Observed)
max(dat.lb$Date.Observed)
range(dat.lb$Date.Observed)
mean(dat.lb$Date.Observed,na.rm=T)

#Now make my Yday
dat.lb$yday <- lubridate::yday(dat.lb$Date.Observed)
summary(dat.lb)


#only looking at trees that showed breaking leaf buds in the first half of the year
dat.lb <- dat.lb [dat.lb$yday<=180,]
summary(dat.lb)

#aggregating quercus.lf so it shows me the date of first breaking leaf buds for  every plant number and species 
breaking.buds <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lb, FUN=min, na.rm=T)
summary(breaking.buds)
head(breaking.buds)

breaking.buds$Date <- as.Date(paste0("2023-", breaking.buds$yday), format="%Y-%j")

##* Graphing----
#png(file.path(path.figs,"Leaf_Breaking_Buds_dens_highlight.png"), height=4, width=6, units="in", res=320)
ggplot(data=breaking.buds) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Mean Day of First Breaking Leaf Buds", x="Date", fill="Year")
dev.off()


#7. Leaves Present----

dat.lp <- dat.spring[dat.spring$leaf.present.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.present.observed", "Date", "Collection")]
dat.lp <- dat.lp[!is.na(dat.lp$PlantNumber),]
summary(dat.lp)
head(dat.lp)

#finding the minimimum and maximum range and mean of the dates leaf present was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.lp$Date.Observed)
max(dat.lp$Date.Observed)
range(dat.lp$Date.Observed)
mean(dat.lp$Date.Observed,na.rm=T)

#Now make my Yday
dat.lp$yday <- lubridate::yday(dat.lp$Date.Observed)
summary(dat.lp)


#only looking at trees that showed leaf present in the first 200 days of the year
dat.lp <- dat.lp [dat.lp$yday >= 75 &dat.lp$yday<=200,]
#summary(dat.lp)

#aggregating quercus.lf so it shows me the date of first leaf present for  every plant number and species 
leaves.present <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lp, FUN=min, na.rm=T)
summary(leaves.present)
head(leaves.present)

leaves.present$Date <- as.Date(paste0("2023-", leaves.present$yday), format="%Y-%j")

##* Graphing----
png(file.path(path.figs,"All_First_Leaf_Present_dens_highlight.png"), height=4, width=6, units="in", res=320)
ggplot(data=leaves.present) +
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Mean Day of First Leaves Present", x="Date", fill="Year")
dev.off()

#8. Leaves present intensity----
dat.lpi <- dat.all[dat.all$leaf.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.present.intensity", "Date", "Collection")]
summary(dat.lpi)
dat.lpi <- dat.lpi[!is.na(dat.lpi$PlantNumber),]
summary(dat.lpi)

#Checking to make sure date ranges are correct
min(dat.lpi$Date.Observed)
max(dat.lpi$Date.Observed)
mean(dat.lpi$Date.Observed)
range(dat.lpi$Date.Observed)

#Setting my yday
dat.lpi$yday <- lubridate::yday(dat.lpi$Date.Observed)
summary(dat.lpi)

#setting my yday to only show dates later in the season and the current date
#dat.lpi <- dat.lpi [dat.lpi$yday>=180,]
#dat.lpi <- dat.lpi [dat.lpi$yday<=Sys.Date(),]
#summary(dat.lpi)

#removing "0 and NA's
dat.lpi <- aggregate(yday ~ PlantNumber + Species + Year + Collection + leaf.present.intensity + Date.Observed , dat=dat.lpi, FUN=min, NA.rm=T)
summary(dat.lpi)
head(dat.lpi)
#filtering out 0%
dat.lpi <- dat.lpi %>% filter( leaf.present.intensity != "0%")
dat.lpi <- dat.lpi %>% filter( leaf.present.intensity != "Select an option")


dat.lpi$yday <- lubridate::yday(dat.lpi$Date.Observed)
summary(dat.lpi)

dat.lpi$Date <- as.Date(paste0("2024-", dat.lpi$yday), format="%Y-%j")
#leaves.present.intensity <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.lpi, FUN=min, NA.rm=T)
#summary(leaves.present.intensity)
#head(leaves.present.intensity)
##* Graphing----
#png(file.path(path.figs,"All_Leaf_Present_Intensity.png"), height= 5, width= 7, units="in", res=320)
ggplot(data=dat.lpi) +
  geom_histogram(alpha=1.5, binwidth =15, aes(x=Date, fill=leaf.present.intensity,))+
  facet_grid(Year~Collection)+
  scale_x_date(date_labels="%b %d", date_breaks="3 month") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 75, hjust = 1), size = 8) +
  labs(title="Leaves Present Intensity", x="Day of Year",)
dev.off()


#9. Flower present intensity---- 

dat.fpi <- dat.spring[dat.spring$flower.open.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.open.intensity", "Date", "Collection")]
summary(dat.fpi)
dat.fpi <- dat.fpi[!is.na(dat.fpi$PlantNumber),]
summary(dat.fpi)

#Checking to make sure date ranges are correct
min(dat.fpi$Date.Observed)
max(dat.fpi$Date.Observed)
mean(dat.fpi$Date.Observed)
range(dat.fpi$Date.Observed)

#Setting my yday
dat.fpi$yday <- lubridate::yday(dat.fpi$Date.Observed)
summary(dat.fpi)

#setting my yday to only show dates later in the season and the current date
#dat.fpi <- dat.fpi [dat.fpi$yday>=180,]
#dat.fpi <- dat.fpi [dat.fpi$yday<=Sys.Date(),]
#summary(dat.fpi)

#removing "0 and NA's
dat.fpi <- aggregate(yday ~ PlantNumber + Species + Year + Collection + flower.open.intensity + Date.Observed , dat=dat.fpi, FUN=min, NA.rm=T)
summary(dat.fpi)
head(dat.fpi)

dat.fpi$yday <- lubridate::yday(dat.fpi$Date.Observed)
summary(dat.fpi)

##thereas a weird value of "Select an option, removing this and coming back later to fix it
unique(dat.fpi$flower.open.intensity)
# Remove rows where flower.open.intensity is "Select an option" and 0%
dat.fpi <- dat.fpi %>% filter(!flower.open.intensity %in% c("Select an option", "0%"))
unique(dat.fpi$flower.open.intensity)

dat.fpi$Date <- as.Date(paste0("2024-", dat.fpi$yday), format="%Y-%j")
##* Graphing----
#png(file.path(path.figs,"All_Flower_Open_Intensity.png"), height=4, width=6, units="in", res=320)+
ggplot(data=dat.fpi) +
  geom_histogram(alpha=1.5, binwidth =10, aes(x=Date, fill=flower.open.intensity,))+
  facet_grid(Year~Collection, scales="free")+
  scale_x_date(date_labels="%b %d", date_breaks="3 month") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Open Flower Intensity", x="Day of Year",)
dev.off()



#10. Fruit present and ripe fruit intensity---- 

dat.fri <- dat.spring[dat.spring$fruit.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.intensity", "Date", "Collection")]
summary(dat.fri)
dat.fri <- dat.fri[!is.na(dat.fri$PlantNumber),]
summary(dat.fri)

#Checking to make sure date ranges are correct
min(dat.fri$Date.Observed)
max(dat.fri$Date.Observed)
mean(dat.fri$Date.Observed)
range(dat.fri$Date.Observed)

#Setting my yday
dat.fri$yday <- lubridate::yday(dat.fri$Date.Observed)
summary(dat.fri)

#setting my yday to only show dates later in the season and the current date
#dat.fri <- dat.fri [dat.fri$yday>=180,]
#dat.fri <- dat.fri [dat.fri$yday<=Sys.Date(),]
#summary(dat.fri)

#removing "0 and NA's
dat.fri <- aggregate(yday ~ PlantNumber + Species + Year + Collection + fruit.present.intensity + Date.Observed , dat=dat.fri, FUN=min, NA.rm=T)
summary(dat.fri)
head(dat.fri)

dat.fri$yday <- lubridate::yday(dat.fri$Date.Observed)
summary(dat.fri)

##thereas a weird value of "Select an option, removing this and coming back later to fix it
unique(dat.fri$fruit.present.intensity)
# Remove rows where fruit.present.intensity is "Select an option" and 0%
dat.fri <- dat.fri %>% filter(!fruit.present.intensity %in% c("Select an option", "0%"))
unique(dat.fri$fruit.present.intensity)

dat.fri$Date <- as.Date(paste0("2024-", dat.fri$yday), format="%Y-%j")
## * Graphing----
png(file.path(path.figs,"All_Fruit_Present_Intensity.png"), height=4, width=6, units="in", res=320)+
  ggplot(data=dat.fri) +
  geom_histogram(alpha=1.5, binwidth =10, aes(x=Date, fill=fruit.present.intensity,))+
  facet_grid(Year~Collection, scales="free")+
  scale_x_date(date_labels="%b %d", date_breaks="3 month") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Fruit Present Intensity", x="Day of Year",)
dev.off()


dat.rfi <- dat.spring[dat.spring$fruit.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.intensity", "Date", "Collection")]
summary(dat.rfi)
dat.rfi <- dat.rfi[!is.na(dat.rfi$PlantNumber),]
summary(dat.rfi)

#Checking to make sure date ranges are correct
min(dat.rfi$Date.Observed)
max(dat.rfi$Date.Observed)
mean(dat.rfi$Date.Observed)
range(dat.rfi$Date.Observed)

#Setting my yday
dat.rfi$yday <- lubridate::yday(dat.rfi$Date.Observed)
summary(dat.rfi)

#setting my yday to only show dates later in the season and the current date
#dat.rfi <- dat.rfi [dat.rfi$yday>=180,]
#dat.rfi <- dat.rfi [dat.rfi$yday<=Sys.Date(),]
#summary(dat.rfi)

#removing "0 and NA's
dat.rfi <- aggregate(yday ~ PlantNumber + Species + Year + Collection + fruit.ripe.intensity + Date.Observed , dat=dat.rfi, FUN=min, NA.rm=T)
summary(dat.rfi)
head(dat.rfi)

dat.rfi$yday <- lubridate::yday(dat.rfi$Date.Observed)
summary(dat.rfi)

##thereas a weird value of "Select an option, removing this and coming back later to fix it
unique(dat.rfi$fruit.ripe.intensity)
# Remove rows where fruit.present.intensity is "Select an option" and 0%
dat.rfi <- dat.rfi %>% filter(!fruit.ripe.intensity %in% c("Select an option", "0%"))
unique(dat.rfi$fruit.ripe.intensity)

dat.rfi$Date <- as.Date(paste0("2024-", dat.rfi$yday), format="%Y-%j")
## * Graphing----
#png(file.path(path.figs,"All_Fruit_Present_Intensity.png"), height=4, width=6, units="in", res=320)+
ggplot(data=dat.rfi) +
  geom_histogram(alpha=1.5, binwidth =10, aes(x=Date, fill=fruit.ripe.intensity,))+
  facet_grid(Year~Collection, scales="free")+
  scale_x_date(date_labels="%b %d", date_breaks="3 month") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Fruit Present Intensity", x="Day of Year",)
dev.off()


# 11. Leaves increasing in size----

dat.li <- dat.spring[dat.spring$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "leaf.increasing.observed", "Date", "Collection")]
dat.li <- dat.li[!is.na(dat.li$PlantNumber),]
summary(dat.li)
head(dat.li)

#finding the minimimum and maximum range and mean of the dates leaves increasing in size was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.li$Date.Observed)
max(dat.li$Date.Observed)
range(dat.li$Date.Observed)
mean(dat.li$Date.Observed,na.rm=T)

#Now make my Yday
dat.li$yday <- lubridate::yday(dat.li$Date.Observed)
summary(dat.li)


#only looking at trees that showed leaves increasing in size in the first half of the year
dat.li <- dat.li [dat.li$yday<=180,]

summary(dat.li)

#aggregating quercus.lf so it shows me the date of first leaf increasing in size for  every plant number and species 
leaves.increasing <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.li, FUN=min, na.rm=T)
summary(leaves.increasing)
head(leaves.increasing)

leaves.increasing$Date <- as.Date(paste0("2018-", leaves.increasing$yday), format="%Y-%j")
## * Graphing----
png(file.path(path.figs,"All_Leaf_Increasing_dens_highlight.png"), height=4, width=6, units="in", res=320)
ggplot(data=leaves.increasing) +
  facet_grid(Collection~ ., scales = "free_y") + # This is the code that will stack everything
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Mean Day of Leaves Increasing in Size Observed", x="Day of Year")
dev.off()

ggplot(data=leaves.increasing) +
  facet_grid(Collection~ ., scales = "free_x") + # This is the code that will stack everything
  geom_histogram(alpha=0.5, aes(x=yday, fill=as.factor(Year), color=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"= "#F0E442", "2023"="purple3" )) +
  scale_color_manual(name="Year", values=c("2018"="#CC79A7", "2019"="#009E73", "2020"="gray", "2021"="#0072B2", "2022"="#F0E442", "2023"= "purple3")) +
  theme_bw()+
  labs(title="Average Day of Leaves Increasing in Size Observed", x="Day of Year")
dev.off()

# 12. Getting a graph of flower buds observations----

dat.fb <- dat.spring[dat.spring$flower.buds.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.buds.observed", "Date","Collection")]
dat.fb <- dat.fb[!is.na(dat.fb$PlantNumber),]
summary(dat.fb)
head(dat.fb)

#finding the minimimum and maximum range and mean of the dates flower buds were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fb$Date.Observed)
max(dat.fb$Date.Observed)
range(dat.fb$Date.Observed)
mean(dat.fb$Date.Observed,na.rm=T)

#Now make my Yday
dat.fb$yday <- lubridate::yday(dat.fb$Date.Observed)
summary(dat.fb)

min(dat.fb$Date.Observed[dat.fb$Year == 2025], na.rm = TRUE)
max(dat.fb$Date.Observed[dat.fb$Year == 2025], na.rm = TRUE)
range(dat.fb$Date.Observed[dat.fb$Year == 2025], na.rm = TRUE)

#only looking at trees that showed flower buds in the first half of the year
#dat.fb <- dat.fb [dat.fb$yday<=180,]
summary(dat.fb)
#checking Ulmus flower dates
min(dat.fb$Date.Observed[dat.fb$Collection == "Ulmus"], na.rm = TRUE)
max(dat.fb$Date.Observed[dat.fb$Collection == "Ulmus"], na.rm = TRUE)
range(dat.fb$Date.Observed[dat.fb$Collection == "Ulmus"], na.rm = TRUE)

#aggregating quercus.lf so it shows me the date of first flower buds for  every plant number and species 
flower.buds <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fb, FUN=min, na.rm=T)
summary(flower.buds)
head(flower.buds)

flower.buds$Date <- as.Date(paste0("2024-", flower.buds$yday), format="%Y-%j")

## * Graphing----
png(file.path(path.figs,"All_Flowers_or_Flower_Buds_highlight.png"), height=4, width=6, units="in", res=320)
ggplot(data=flower.buds) +
  facet_grid(Collection~ ., scales="free") + # This is the code that will stack everything
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Average Day of Flower Buds or Flowers Observed", x="Day of Year")
dev.off()



# 13. Open flowers----

dat.fo <- dat.spring[dat.spring$flower.open.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.open.observed", "Date", "Collection")]
dat.fo <- dat.fo[!is.na(dat.fo$PlantNumber),]
summary(dat.fo)
head(dat.fo)

#finding the minimimum and maximum range and mean of the dates open flowers were observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fo$Date.Observed)
max(dat.fo$Date.Observed)
range(dat.fo$Date.Observed)
mean(dat.fo$Date.Observed,na.rm=T)

#Now make my Yday
dat.fo$yday <- lubridate::yday(dat.fo$Date.Observed)
summary(dat.fo)


#only looking at trees that showed open flowers in the first half of the year
#dat.fo <- dat.fo [dat.fo$yday<=180,]
summary(dat.fo)

#aggregating quercus.lf so it shows me the date of open flowers for  every plant number and species 
flower.open <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fo, FUN=min, na.rm=T)
summary(flower.open)
head(flower.open)

flower.open$Date <- as.Date(paste0("2018-", flower.open$yday), format="%Y-%j")

## * Graphing----
png(file.path(path.figs,"All_Flowers_Open_Highlight.png"), height=4, width=6, units="in", res=320)
ggplot(data=flower.open) +
  facet_grid(Collection~ ., scales = "free_y") + # This is the code that will stack everything
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Mean Day of Open Flowers Observed", x="Day of Year")
dev.off()


# 14. Pollen----

dat.fp <- dat.spring[dat.spring$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "PlantNumber", "Year", "flower.pollen.observed","Date", "Collection")]
dat.fp <- dat.fp[!is.na(dat.fp$PlantNumber),]
summary(dat.fp)
head(dat.fp)

#finding the minimimum and maximum range and mean of the dates pollen was observed on our trees.
#Note the na.rm=T which is removing N/A values
min(dat.fp$Date.Observed)
max(dat.fp$Date.Observed)
range(dat.fp$Date.Observed)
mean(dat.fp$Date.Observed,na.rm=T)

#Now make my Yday
dat.fp$yday <- lubridate::yday(dat.fp$Date.Observed)
summary(dat.fp)


#only looking at trees that showed pollen in the first half of the year
#dat.fp <- dat.fp [dat.fp$yday<=180,]
summary(dat.fp)

#aggregating quercus.lf so it shows me the date of first pollen for  every plant number and species 
flower.pollen <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fp, FUN=min, na.rm=T)
summary(flower.pollen)
head(flower.pollen)

flower.pollen$Date <- as.Date(paste0("2024-", flower.pollen$yday), format="%Y-%j")

## * Graphing----
png(file.path(path.figs,"All_Flowers_Pollen_highlight.png"), height=4, width=6, units="in", res=320)
ggplot(data=flower.pollen) +
  facet_grid(Collection ~ ., scales="free_y") +
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Average Day of Flower Pollen Observed", x="Day of Year")
dev.off()

# 15. Fruit present----

dat.fr <- dat.spring[dat.spring$fruit.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed", "Date", "Collection")]
summary(dat.fr)
dat.fr <- dat.fr[!is.na(dat.fr$PlantNumber),]
summary(dat.fr)

#Checking to make sure date ranges are correct
min(dat.fr$Date.Observed)
max(dat.fr$Date.Observed)
mean(dat.fr$Date.Observed)
range(dat.fr$Date.Observed)

#Setting my yday
dat.fr$yday <- lubridate::yday(dat.fr$Date.Observed)
summary(dat.fr)

#setting my yday to only show dates later in the season and the current date
#dat.fr <- dat.fr [dat.fr$yday<=09,]
#dat.fr <- dat.fr [dat.fr$yday<=Sys.Date(),]
#summary(dat.fr)

#aggregating to only show me observations that are present
fruit.present <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fr, FUN=min, na.rm=T)
summary(fruit.present)
head(fruit.present)

fruit.present$Date <- as.Date(paste0("2018-", fruit.present$yday), format="%Y-%j")

## * Graphing----
ggplot(data=fruit.present) +
    png(file.path(path.figs,"All_Fruit_present_highlight.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection ~ ., scales="free_y") +
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Mean Day of First Fruit Present", x="Date", fill="Year")
dev.off()

# 16. Ripe fruit----
dat.rf <- dat.spring[dat.spring$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed","Date","Collection")]
summary(dat.rf)
dat.rf <- dat.rf[!is.na(dat.rf$PlantNumber),]
summary(dat.rf)

#Checking to make sure date ranges are correct
min(dat.rf$Date.Observed)
max(dat.rf$Date.Observed)
mean(dat.rf$Date.Observed)
range(dat.rf$Date.Observed)

#Setting my yday
dat.rf$yday <- lubridate::yday(dat.rf$Date.Observed)
summary(dat.rf)

#setting my yday to only show dates later in the season and the current date
#dat.rf <- dat.rf [dat.rf$yday>=180,]
#dat.rf <- dat.rf [dat.rf$yday<=Sys.Date(),]
#summary(dat.rf)

#aggregating to only show me observations that are present
ripe.fruit <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.rf, FUN=min, na.rm=T)
summary(ripe.fruit)
head(ripe.fruit)

ripe.fruit$Date <- as.Date(paste0("2024-", ripe.fruit$yday), format="%Y-%j")

## * Graphing----
ggplot(data=ripe.fruit) +
   png(file.path(path.figs,"All_Ripe_Fruit_Present_highlight.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection ~ ., scales="free_y") +
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Mean Day of First Ripe Fruit Present", x="Date", fill="Year")

dev.off()


# 17. Fruit drop----
dat.fd <- dat.spring[dat.spring$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed", "Date","Collection")]
summary(dat.fd)
dat.fd <- dat.fd[!is.na(dat.fd$PlantNumber),]
summary(dat.fd)

#Checking to make sure date ranges are correct
min(dat.fd$Date.Observed)
max(dat.fd$Date.Observed)
mean(dat.fd$Date.Observed)
range(dat.fd$Date.Observed)

#Setting my yday
dat.fd$yday <- lubridate::yday(dat.fd$Date.Observed)
summary(dat.fd)

#setting my yday to only show dates later in the season and the current date
#dat.fd <- dat.fd [dat.fd$yday>=180,]
#dat.fd <- dat.fd [dat.fd$yday<=Sys.Date(),]
#summary(dat.fd)

#aggregating to only show me observations that are present
fruit.drop <- aggregate(yday ~ PlantNumber + Species + Year + Collection , data=dat.fd, FUN=min, na.rm=T)
summary(fruit.drop)
head(fruit.drop)

fruit.drop$Date <- as.Date(paste0("2024-", fruit.drop$yday), format="%Y-%j")

## * Graphing----
ggplot(data=fruit.drop) +
  png(file.path(path.figs,"All_Fruit_Drop_Present_highlight.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection ~ ., scales="free_y") +
  geom_density(alpha=0.25, aes(x=Date, fill=as.factor(Year), color=as.factor(Year))) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") +  # Format x-axis as month and date with a 1 month break
  scale_fill_manual(name="Year", values=c("2025"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2025"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Mean Day of First Fruit Drop Present", x="Date", fill="Year")
dev.off()


###Animations


#doing freq
ggplot(data=leaf.color) +
  #png(file.path(path.figs,"All_First_Leaf_Color_freqpoly.png"), height=4, width=6, units="in", res=320)+
  facet_grid(Collection~ .) + # This is the code that will stack everything
  geom_freqpoly(alpha=0.5, bins = 45, aes(x=Date,color=as.factor(Year), fill=as.factor(Year))) +
  scale_fill_manual(name="Year", values=c("2024"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2024"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Leaf Color Present", x="Day of Year")
dev.off()


# 18. Animations----
p <- ggplot(data=leaf.color) +
  facet_grid(Collection~ .,scales="free_y") +
  geom_freqpoly(alpha=0.25,aes(x=Date, color=as.factor(Year),)) +
  scale_x_date(date_labels="%b %d", date_breaks="1 month") + 
  scale_fill_manual(name="Year", values=c("2023"="red", "others"="gray"), guide=guide_legend(override.aes=list(fill=c("red")))) +
  scale_color_manual(name="Year", values=c("2023"="red", "others"="gray")) +
  theme_bw() +
  labs(title="Leaf Color Present", x="Day of Year") +
  transition_states(Date, transition_length = 2, state_length = 1)+
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 
# check  the animation as a gif
animate(p, nframes = 100, fps = 7)

anim_save(file.path(path.figs, animation = animate(p, nframes = 100), fps = 7))

dat.lb23 <- dat.24[dat.24$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed","leaf.breaking.buds.observed","leaf.present.observed", "Collection")] 
dat.lb23 <- dat.lb23[!is.na(dat.lb23$PlantNumber),]
summary(dat.lb23)

dat.lp23 <- dat.24[dat.24$leaf.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed","leaf.breaking.buds.observed","leaf.present.observed", "Collection")] 
dat.lp23 <- dat.lp23[!is.na(dat.lp23$PlantNumber),]
summary(dat.lp23)

dat.lc23 <- dat.24[dat.24$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed", "leaf.breaking.buds.observed","leaf.present.observed", "Collection")]  
dat.lc23 <- dat.lc23[!is.na(dat.lc23$PlantNumber),]
summary(dat.lc23)

dat.23lam <- rbind(dat.lc23,dat.lp23,dat.lb23)
summary(dat.23lam)
dat.23lam$yday <- lubridate::yday(dat.23lam$Date.Observed)
summary(dat.23lam)

dat.23y <- rbind(dat.lc23,dat.lp23,dat.lb23)
summary(dat.23y)
dat.23y$yday <- lubridate::yday(dat.23y$Date.Observed)
summary(dat.23y)

#Finally realized I could make another ifelse statment the "else" in and ifelse statment- astounding levels of stupidity here. 
dat.23y$pheno = with(dat.23y, ifelse(dat.23y$leaf.color.observed=="Yes", "Leaf Color Observed",
                                     ifelse(dat.23y$leaf.present.observed=="Yes" & dat.23y$leaf.breaking.buds.observed=="Yes", "Leaves Present Observed",
                                            ifelse(dat.23y$leaf.breaking.buds.observed=="Yes", "Leaf Breaking Buds Observed", "Leaves Present Observed"))))




####### many versions of the same graph

# Large pole dark
p<-ggplot(dat.23y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-100,325) +
  theme_dark()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)

# small pole dark

p<-ggplot(dat.23y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-50,325) +
  theme_dark()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)

# no pole dark
p<-ggplot(dat.23y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(0,325) +
  theme_dark()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)


## Light
#Large pole light
p<-ggplot(dat.23y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-100,325) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)

#small pole light

p<-ggplot(dat.23y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-50,325) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)
# no pole light
p<-ggplot(dat.23y) + 
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(0,325) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 


animate(p, fps=8)



###
#Small pole light- facet- collections
p<-ggplot(dat.23y) + 
  facet_wrap_interactive(Collection~ .)+
  geom_bar(alpha=11,aes(x=yday, fill=pheno,))+ ylim(-50,200) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 

animate(p, fps=8)



dat.lb23i <- dat.24[dat.24$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed", "leaf.breaking.buds.observed","leaf.present.observed", "Collection", "leaf.breaking.buds.intensity", "leaf.present.intensity", "leaf.color.intensity" )] 
dat.lb23i <- dat.lb23i[!is.na(dat.lb23i$PlantNumber),]
summary(dat.lb23i)

dat.lp23i <- dat.24[dat.24$leaf.present.observed=="Yes",c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed", "leaf.breaking.buds.observed","leaf.present.observed", "Collection", "leaf.breaking.buds.intensity", "leaf.present.intensity", "leaf.color.intensity" )]
dat.lp23i <- dat.lp23i[!is.na(dat.lp23i$PlantNumber),]
summary(dat.lp23i)

dat.lc23i <- dat.24[dat.24$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed", "leaf.breaking.buds.observed","leaf.present.observed", "Collection", "leaf.breaking.buds.intensity", "leaf.present.intensity", "leaf.color.intensity" )]  
dat.lc23i <- dat.lc23i[!is.na(dat.lc23i$PlantNumber),]
summary(dat.lc23i)


dat.23z <- rbind(dat.lc23i,dat.lp23i,dat.lb23i)
summary(dat.23z)
dat.23z$yday <- lubridate::yday(dat.23z$Date.Observed)
summary(dat.23z)

#doesn't work exactly
dat.23z$pheno = with(dat.23z, ifelse(dat.23z$leaf.color.intensity %in% c("50-74%", "75-94%",">95%")| dat.23z$leaf.present.intensity %in% c("0%", "<5%", "5-24%","25-49%") , "Leaf Color Observed",
                                     ifelse(dat.23z$leaf.present.intensity %in% c("50-74%", "75-94%", ">95%") & dat.23z$leaf.breaking.buds.observed=="Yes", "Leaves Present Observed",
                                            ifelse(dat.23z$leaf.breaking.buds.observed=="Yes", "Leaf Breaking Buds Observed", "Leaves Present Observed"))))


#sort of works
dat.23z$pheno = with(dat.23z, ifelse(leaf.breaking.buds.observed=="Yes" & leaf.present.observed!="Yes", "Leaf Breaking Buds Observed",
                                     ifelse(leaf.breaking.buds.observed=="Yes" & leaf.present.observed=="Yes", "Leaves Present Observed",
                                            ifelse(leaf.color.intensity %in% c("50-74%", "75-94%",">95%")| leaf.present.intensity %in% c("0%", "<5%", "5-24%","25-49%") , "Leaf Color Observed", "Leaves Present Observed"))))


dat.23z$pheno = with(dat.23z, ifelse(dat.23z$leaf.color.intensity %in% c("50-74%", "75-94%",">95%")| dat.23z$leaf.present.intensity %in% c("0%", "<5%", "5-24%","25-49%") , "Leaf Color",
                                     ifelse(dat.23z$leaf.present.observed== "Yes" & dat.23z$leaf.breaking.buds.observed=="Yes", "Leaves Present ",
                                            ifelse(dat.23z$leaf.breaking.buds.observed=="Yes" & dat.23z$leaf.present.observed!= "Yes","Leaf Breaking Buds ", 
                                                   ifelse(dat.23z$leaf.breaking.buds.observed=="Yes" | dat.23z$leaf.color.observed=="Yes","Leaf Breaking Buds", "Leaves Present")))))

p<-ggplot(dat.23z) + 
  facet_wrap(Collection~.)+
  geom_bar(alpha=0.5,aes(x=yday, fill=pheno,))+ ylim(-50,325) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 

animate(p, fps=7.5)


#View follow animate with free  y scale
p<-ggplot(dat.23z) + 
  facet_wrap(~Collection , scales = "free_y")+
  geom_bar(alpha=0.5,aes(x=yday, fill=pheno,))+ ylim(-50,325) +
  theme_bw()+
  labs(title="Leaf Phenopases", x="Day of Year",)+
  #coord_polar(start = 200)+
  transition_states(yday, transition_length = 30, state_length =30)+
  ease_aes(x = 'sine-out', y = 'sine-out') + 
  view_follow()+
  shadow_mark(1, size = 2, alpha = TRUE, wrap = TRUE, #exclude_layer = c(2, 3),
              falloff = 'sine-in', exclude_phase = 'enter') 

animate(p, fps=7.5)




############Misc: In progress #################
###
#getting averages for date of  phenophases occurace in certain years

####Open flowers quercus
dat.ofa18 <- quercus18[quercus18$flower.open.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "flower.open.observed")]
summary(dat.of)
#####Fruit Present quercus & acer
#2018 quercus
dat.fpa18 <- quercus18[quercus18$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpa18)
#2019 quercus
dat.fpa19 <- quercus19[quercus19$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpa19)
#2021 quercus
dat.fpa21 <- quercus21[quercus21$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpa21)
#2019 acer
dat.afpa19 <- acer19[acer19$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.afpa19)
#2021 acer
dat.afpa21 <- acer21[acer21$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.afpa21)

##### Ripe fruit
#quercus 21
dat.rfa21 <- quercus21[quercus21$fruit.ripe.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.rfa21)
#2019 acer
dat.arfa19 <- acer19[acer19$fruit.ripe.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.arfa19)
#2021 acer
dat.arfa21 <- acer21[acer21$fruit.ripe.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.arfa21)

### Fruit Drop
#quercus 21
dat.fda21 <- quercus21[quercus21$fruit.drop.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.rfa21)
#2019 acer
dat.afda19 <- acer19[acer19$fruit.drop.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.arfa19)
#2021 acer
dat.afda21 <- acer21[acer21$fruit.drop.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.arfa21)

##### breaking leaf buds 22
dat.bbq22 <- quercus22[quercus22$leaf.breaking.buds.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "leaf.breaking.buds.observed")]
summary(dat.bbq22)

dat.bba22 <- acer22[acer22$leaf.breaking.buds.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "leaf.breaking.buds.observed")]
summary(dat.bba22)


### Flower buds 22
dat.fbq22 <- quercus22[quercus22$flower.buds.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "flower.buds.observed")]
summary(dat.bbq22)

dat.fba22 <- acer22[acer22$flower.buds.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "flower.buds.observed")]
summary(dat.bba22)

### open flowers 22
dat.ofq22 <- quercus22[quercus22$flower.open.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "flower.open.observed")]
summary(dat.ofq22)

dat.ofa22 <- acer22[acer22$flower.open.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "flower.open.observed")]
summary(dat.ofa22)

### pollen 22
dat.pfq22 <- quercus22[quercus22$flower.pollen.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "flower.pollen.observed")]
summary(dat.ofq22)

dat.pfa22 <- acer22[acer22$flower.pollen.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "flower.pollen.observed")]
summary(dat.ofa22)


### fruit present
dat.fpq22 <- quercus22[quercus22$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpq22)

dat.fpa22 <- acer22[acer22$fruit.present.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpa22)

### Ripe fruit 
dat.frq22 <- quercus22[quercus22$fruit.ripe.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.frq22)

dat.fra22 <- acer22[acer22$fruit.ripe.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.fra22)

### Fruit Drop
dat.fdq22 <- quercus22[quercus22$fruit.drop.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.frq22)

dat.fda22 <- acer22[acer22$fruit.drop.observed=="Yes", c("Date.Observed","Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.fda22)


##** 2023  below #####
##### breaking leaf buds 22
dat.bbq23 <- quercus23[quercus23$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.breaking.buds.observed")]
summary(dat.bbq23)
head(dat.bbq23)

dat.bba23 <- acer23[acer23$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.breaking.buds.observed")]
summary(dat.bba23)
head(dat.bba23)

dat.bbu23 <- ulmus23[ulmus23$leaf.breaking.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.breaking.buds.observed")]
summary(dat.bbu23)
head(dat.bbu23)

### Leaf Present Observed
dat.lpq23 <- quercus23[quercus23$leaf.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.present.observed")]
summary(dat.lpq23)
head(dat.lpq23)

dat.lpa23 <- acer23[acer23$leaf.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.present.observed")]
summary(dat.lpa23)
head(dat.lpa23)

dat.lpu23 <- ulmus23[ulmus23$leaf.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.present.observed")]
summary(dat.lpu23)
head(dat.lpu23)

### Leaf Increasing Observed
dat.liq23 <- quercus23[quercus23$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.increasing.observed")]
summary(dat.liq23)
head(dat.liq23)

dat.lia23 <- acer23[acer23$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.increasing.observed")]
summary(dat.lia23)
head(dat.lia23)

dat.liu23 <- ulmus23[ulmus23$leaf.increasing.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.increasing.observed")]
summary(dat.liu23)
head(dat.liu23)

### Leaf Color Observed
dat.lcq23 <- quercus23[quercus23$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed")]
summary(dat.lcq23)
head(dat.lcq23)

dat.lca23 <- acer23[acer23$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed")]
summary(dat.lca23)
head(dat.lca23)

dat.lcu23 <- ulmus23[ulmus23$leaf.color.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.color.observed")]
summary(dat.lcu23)
head(dat.lcu23)

### Leaf Falling Observed
dat.lfq23 <- quercus23[quercus23$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.falling.observed")]
summary(dat.lfq23)
head(dat.lfq23)

dat.lfa23 <- acer23[acer23$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.falling.observed")]
summary(dat.lfa23)
head(dat.lfa23)

dat.lfu23 <- ulmus23[ulmus23$leaf.falling.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "leaf.falling.observed")]
summary(dat.lfu23)
head(dat.lfu23)


### Flower Buds 23
dat.fbq23 <- quercus23[quercus23$flower.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.buds.observed")]
summary(dat.fbq23)
head(dat.fbq23)

dat.fba23 <- acer23[acer23$flower.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.buds.observed")]
summary(dat.fba23)
head(dat.fba23)

dat.fbu23 <- ulmus23[ulmus23$flower.buds.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.buds.observed")]
summary(dat.fbu23)
head(dat.fbu23)

### Open Flowers 23
dat.ofq23 <- quercus23[quercus23$flower.open.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.open.observed")]
summary(dat.ofq23)
head(dat.ofq23)

dat.ofa23 <- acer23[acer23$flower.open.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.open.observed")]
summary(dat.ofa23)
head(dat.ofa23)

dat.ofu23 <- ulmus23[ulmus23$flower.open.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.open.observed")]
summary(dat.ofu23)
head(dat.ofu23)
### Pollen 23
dat.pfq23 <- quercus23[quercus23$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.pollen.observed")]
summary(dat.pfq23)
head(dat.pfq23)

dat.pfa23 <- acer23[acer23$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.pollen.observed")]
summary(dat.pfa23)
head(dat.pfa23)

dat.pfu23 <- ulmus23[ulmus23$flower.pollen.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "flower.pollen.observed")]
summary(dat.pfu23)
head(dat.pfu23)

### Fruit Present
dat.fpq23 <- quercus23[quercus23$fruit.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpq23)
head(dat.fpq23)

dat.fpa23 <- acer23[acer23$fruit.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpa23)
head(dat.fpa23)

dat.fpu23 <- ulmus23[ulmus23$fruit.present.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.present.observed")]
summary(dat.fpu23)
head(dat.fpu23)

### Ripe Fruit
dat.frq23 <- quercus23[quercus23$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.frq23)
head(dat.frq23)

dat.fra23 <- acer23[acer23$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.fra23)
head(dat.fra23)

dat.fru23 <- ulmus23[ulmus23$fruit.ripe.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.ripe.observed")]
summary(dat.fru23)
head(dat.fru23)

### Fruit Drop
dat.fdq23 <- quercus23[quercus23$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.fdq23)
head(dat.fdq23)

dat.fda23 <- acer23[acer23$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.fda23)
head(dat.fda23)

dat.fdu23 <- ulmus23[ulmus23$fruit.drop.observed=="Yes", c("Date.Observed", "Species", "Year", "PlantNumber", "fruit.drop.observed")]
summary(dat.fdu23)
head(dat.fdu23)



# New stuff ----
# Function to calculate median date and first week proportions for a specific phenophase and collection
analyze_phenophase <- function(data, collection_name, phenophase_column) {
  # First, verify the column exists
  if(!(phenophase_column %in% names(data))) {
    cat("Warning: Column", phenophase_column, "not found in data\n")
    return(NULL)
  }
  
  # First, verify the collection exists in the data
  if(!(collection_name %in% unique(data$Collection))) {
    cat("Warning: Collection", collection_name, "not found in data\n")
    return(NULL)
  }
  
  # Subset data for specific collection
  collection_data <- data[data$Collection == collection_name, ]
  
  # Split into 2025 and historical
  data_2025 <- collection_data[collection_data$Year == "2025", ]
  data_historical <- collection_data[collection_data$Year != "2025", ]
  
  # Handle NA values in phenophase column
  data_2025[is.na(data_2025[,phenophase_column]), phenophase_column] <- "No"
  data_historical[is.na(data_historical[,phenophase_column]), phenophase_column] <- "No"
  
  # Check if we have any observations
  n_2025 <- sum(data_2025[,phenophase_column] == "Yes", na.rm=TRUE)
  n_historical <- sum(data_historical[,phenophase_column] == "Yes", na.rm=TRUE)
  
  if(n_2025 == 0 || n_historical == 0) {
    return(list(
      collection = collection_name,
      phenophase = phenophase_column,
      median_2025 = NA,
      median_historical = NA,
      prop_first_week_march_2025 = NA,
      prop_first_week_march_historical = NA,
      n_2025 = n_2025,
      n_historical = n_historical
    ))
  }
  
  # Calculate median dates using the yday values
  yday_2025 <- data_2025$yday[data_2025[,phenophase_column] == "Yes"]
  yday_historical <- data_historical$yday[data_historical[,phenophase_column] == "Yes"]
  
  median_2025 <- median(yday_2025, na.rm=TRUE)
  median_historical <- median(yday_historical, na.rm=TRUE)
  
  # Calculate first week of March proportions (yday 60-67)
  prop_2025 <- sum(yday_2025 >= 60 & yday_2025 <= 67, na.rm=TRUE) / length(yday_2025)
  prop_historical <- sum(yday_historical >= 60 & yday_historical <= 67, na.rm=TRUE) / length(yday_historical)
  
  # Return results
  return(list(
    collection = collection_name,
    phenophase = phenophase_column,
    median_2025 = median_2025,
    median_historical = median_historical,
    prop_first_week_march_2025 = prop_2025,
    prop_first_week_march_historical = prop_historical,
    n_2025 = n_2025,
    n_historical = n_historical
  ))
}

# Print current unique collections to verify what's in the data
cat("Collections present in data:\n")
print(unique(dat.spring$Collection))

# Verify phenophase columns exist
cat("\nChecking if phenophase columns exist:\n")
phenophases <- c(
  "leaf.breaking.buds.observed",
  "leaf.present.observed",
  "leaf.increasing.observed",
  "leaf.color.observed",
  "leaf.falling.observed",
  "flower.buds.observed",
  "flower.open.observed",
  "flower.pollen.observed",
  "fruit.present.observed",
  "fruit.ripe.observed",
  "fruit.drop.observed"
)

for(phase in phenophases) {
  cat(phase, ":", phase %in% names(dat.spring), "\n")
}

# Create empty list to store results
results <- list()

# Run analysis for each combination
collections <- c("Acer", "Quercus", "Ulmus")
for(col in collections) {
  cat("\nProcessing collection:", col, "\n")
  for(pheno in phenophases) {
    cat("  Processing phenophase:", pheno, "\n")
    result <- analyze_phenophase(dat.spring, col, pheno)
    if(!is.null(result)) {
      results[[paste(col, pheno, sep="_")]] <- result
    }
  }
}

# Function to print results in a readable format
print_phenology_results <- function(results) {
  for(name in names(results)) {
    r <- results[[name]]
    cat("\n=======================================================\n")
    cat("Collection:", r$collection, "\n")
    cat("Phenophase:", r$phenophase, "\n")
    if(!is.na(r$median_2025)) {
      cat("Median Day of Year 2025:", round(r$median_2025), 
          " (", format(as.Date("2025-01-01") + round(r$median_2025) - 1, "%B %d"), ")\n")
      cat("Median Day of Year Historical:", round(r$median_historical),
          " (", format(as.Date("2025-01-01") + round(r$median_historical) - 1, "%B %d"), ")\n")
      cat("Proportion in First Week of March 2025:", round(r$prop_first_week_march_2025 * 100, 1), "%\n")
      cat("Proportion in First Week of March Historical:", round(r$prop_first_week_march_historical * 100, 1), "%\n")
      cat("Number of observations 2025:", r$n_2025, "\n")
      cat("Number of observations Historical:", r$n_historical, "\n")
    } else {
      cat("No observations available for comparison\n")
    }
  }
}

# Print results
print_phenology_results(results)

