library (tidyr); library(dplyr); library(readbulk); library(googlesheets4)

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

#Joinging two data frames with anti_join, to remove all the values for PlantNumber that match between dat.all and df.gone
new.dat <- anti_join(dat.all, df.gone, by=("PlantNumber"))
summary(new.dat)

#Renaming colums to make things a little easier
colnames(new.dat)<- c("Obs.List", "PlantNumber", "Taxon", "Vernacular","BgLatitude", "BgLongitude","GardenGrid", "GardenSubGrid", "Spacer")
head(new.dat)

# Split dataframe by the "Spacer" column
split_new.dat <- split(new.dat, list(new.dat$Spacer))

# Loop to write out new .csv files  based upon the splits created in the split_new.dat (based around the "Spacer" col. 
# Will create the updated observation lists 
######Change file path out#######
for (Spacer in names(split_new.dat)) {
  write.csv(split_new.dat[[Spacer]], paste0("~/Desktop/R junk/", Spacer, ".csv"),)
}



