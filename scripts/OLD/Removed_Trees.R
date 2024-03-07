library(ggplot2); library(grid);library (tidyr);library(googlesheets);
library(dplyr); library(readbulk); library(googlesheets4)

#Making sure the working directory in is google drive. 
setwd("~/Google Drive/My Drive/LivingCollections_Phenology")
dir.base <- "Google Drive/My Drive/LivingCollections_Phenology"

path.dat <- file.path(dir.base, "Observing Lists")
path.gone <- file.path(dir.base, "Data_Observations")
path.out <- "Google Drive/My Drive/LivingCollections_Phenology/Observing Lists"


#Loading tree lists and place in data frame dat.all
dat.all <- read_bulk(directory = "../LivingCollections_Phenology/Observing Lists/Quercus", extension = ".csv", header=FALSE,)
head(dat.all)

colnames(dat.all)<- c("Obs.List", "PlantNumber", "Taxon", "Vernacular","BgLatitude", "BgLongitude","GardenGrid", "GardenSubGrid", "File")

summary(dat.all)
head(dat.all)

dat.gone <- read_sheet("https://docs.google.com/spreadsheets/d/16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ/edit#gid=0")
df.gone <-data.frame(dat.gone, header=FALSE)

head(df.gone)

#choosing only the columns I want from dat.gone, plant number in this case

#dat.gone1 <- subset(dat.gone, select = c(PlantNumber))

dat.com <- c(dat.all, df.gone$PlantNumber)

dat.com

summary(dat.com)

anti_join(dat.all$PlantNumber, df.gone$PlantNumber)



new.dat <- within(dat.all, rm(dat.gone1$PlantNumber))
  

new.dat <- dat.all[!match(dat.all$PlantNumber,dat.gone1$PlantNumber)]

new.dat < 

View(new.dat)



my_list[names(my_list) %in% "b" == FALSE]
my_list[names(my_list) != "b"] 
                     
