library (tidyr); library(dplyr); library(readbulk); library(googlesheets4)

#Making sure the working directory in is google drive. 
setwd("~/Google Drive/My Drive/LivingCollections_Phenology")
dir.base <- "Google Drive/My Drive/LivingCollections_Phenology"

path.google <- "~/GoogleDrive/My Drive" # Mac
path.out <- "~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/Quercus/"

#Batch loading trees from the Quercus tree observation lists and placing them in data frame dat.all
dat.all <- read_bulk(directory = "../LivingCollections_Phenology/Observing Lists/Quercus", extension = ".csv", header= TRUE,)
head(dat.all)
dat.all <- subset(dat.all, select = c(1:8))

#head(dat.all)
#Up dating col name from V1, V2, etc to reflect the information in those columns 
#colnames(dat.all)<- c("Obs.List", "PlantNumber", "Taxon", "Vernacular","BgLatitude", "BgLongitude","GardenGrid", "GardenSubGrid", "Spacer")


summary(dat.all)
head(dat.all)

#Loading the Removed trees google sheet and placing it in a data frame
dat.gone <- read_sheet("https://docs.google.com/spreadsheets/d/16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ/edit#gid=0")
df.gone <-data.frame(dat.gone, header=TRUE)

head(df.gone)

#Joinging two data frames with anti_join, to remove all the values for PlantNumber that match 
#between dat.all and df.gone and placing them in the new data frame new.dat
new.dat <- anti_join(dat.all, df.gone, by=("PlantNumber"))

#deleting duplicates
new.dat <- new.dat[!duplicated(new.dat), ]

summary(new.dat)

head(new.dat)

#Writing a csv of new.dat to be our quercus all list
write.csv(new.dat, paste0(path.out, "Quercus.csv"), row.names = FALSE)


# Splitting the new.dat dataframe by names in the the "Obs.List" column, splits the data into
# the updated observations lists, and the list containing all oaks. 
split_new.dat <- split(new.dat, list(new.dat$Obs.List))

# Loop to write out new .csv filesbased upon the splits created in the split_new.dat 

for (Obs.List in names(split_new.dat)) {
  write.csv(split_new.dat[[Obs.List]], paste0(path.out,"ObservingList_Quercus_",Obs.List,".csv"), row.names = F,)
}


