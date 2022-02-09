library (tidyr); library(dplyr); library(readbulk); library(googlesheets4)

#Making sure the working directory in is google drive. 
setwd("~/Google Drive/My Drive/LivingCollections_Phenology")
dir.base <- "Google Drive/My Drive/LivingCollections_Phenology"


path.out <- "Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/Quercus"


#Batch loading trees from the Quercus tree observation lists and placing them in data frame dat.all
dat.all <- read_bulk(directory = "../LivingCollections_Phenology/Observing Lists/Quercus", extension = ".csv", header=FALSE,)
head(dat.all)

#Up dating col name from V1, V2, etc to reflect the information in those columns 
colnames(dat.all)<- c("Obs.List", "PlantNumber", "Taxon", "Vernacular","BgLatitude", "BgLongitude","GardenGrid", "GardenSubGrid", "Spacer")

summary(dat.all)
head(dat.all)

#Loading the Removed trees google sheet and placing it in a data frame
dat.gone <- read_sheet("https://docs.google.com/spreadsheets/d/16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ/edit#gid=0")
df.gone <-data.frame(dat.gone, header=FALSE)

head(df.gone)

#Joinging two data frames with anti_join, to remove all the values for PlantNumber that match 
#between dat.all and df.gone and placing them in the new data frame new.dat
new.dat <- anti_join(dat.all, df.gone, by=("PlantNumber"))
summary(new.dat)

# Splitting the new.dat dataframe by names in the the "Spacer" column, splits the data into
# the updated observations lists, and the list containing all oaks. 
split_new.dat <- split(new.dat, list(new.dat$Spacer))

# Loop to write out new .csv filesbased upon the splits created in the split_new.dat 

######Change file path to path.out if everything works correctly#######

for (Spacer in names(split_new.dat)) {
  write.csv(split_new.dat[[Spacer]], paste0("~/Desktop/R junk/", Spacer, ".csv"),)
}



