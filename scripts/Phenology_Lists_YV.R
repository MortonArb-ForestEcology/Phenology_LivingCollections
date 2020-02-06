#This is to pick out trees of interest for youth volunteering

library (tidyr)
library(readbulk)
library(dplyr)
path.out <- "G:/My Drive/LivingCollections_Phenology/Observing Lists/Youth_Volunteers/"

setwd(path.out)

#loading our excel file of tree list

YV.dat <- read_bulk(directory = "Species_Lists", extension = ".csv", header = FALSE, skip=1,)

#Subsetting our needed columns
YV.sub <- subset(YV.dat, select = c(1:3, 10, 12:15))

colnames(YV.sub) <- c("AcessionID", "Taxon", "CommonName", "GardenLocation", "GardenGrid",
                      "GardenSubgrid", "Latitude", "Longitude")


YV.sub <- YV.sub %>% separate(Taxon, c("Genus", "Species"), " ", extra="merge")

#Creating a list of garden locatins we want to pull out of the larger data frame
gardenlist <- list("Childrens Garden", "Meadow Lake", "Firefly Pond", "Administration Building",
                   "Arbor Court", "Ground Cover Garden", "Hedge Garden", "Maze Garden", "Visitor Center")

#Creating a list of taxa we want to pull out of the larger data frame
taxalist <- list("Acer", "Capinus", "Ceris", "Fagus","Ginkgo", "Pinus","Quercus", "Syringa", "Ulmus")


#Comparing the full list to our gardenlist to pull out what we want
YV.org <- YV.sub[(YV.sub$GardenLocation %in% gardenlist),]
YV.org <- YV.org[(YV.org$Genus %in% taxalist),]

#Ordering my alphabetical order of column
YV.org <- arrange(YV.org, GardenLocation)

YV.final <- YV.org[(is.na(YV.org$Latitude) == FALSE),]

write.csv(YV.final, file.path(path.out, file = "Full_Species_List.csv"), row.names=FALSE)

View(YV.org)
