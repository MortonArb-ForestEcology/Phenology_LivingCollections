#This is to pick out trees of interest for youth volunteering

library (tidyr)
library(data.table)

path.out <- "G:/My Drive/LivingCollections_Phenology/Observing Lists/Youth_Volunteers/"

setwd(path.out)

#loading our excel file of tree list

YV.dat <- read.csv("Quercus_YV_List.csv")

#renamed the gardenlocation column to have a logical name
colnames(YV.dat)[10] <- "GardenLocation"

#Creating a list of garden locatins we want to pull out of the larger data frame
gardenlist <- list("Childrens Garden", "Meadow Lake", "Firefly Pond")

#Creating a list of taxa we want to pull out of the larger data frame
taxalist <- list("Quercus")

#Comparing the full list to our gardenlist to pull out what we want
YV.org <- YV.dat[(YV.dat$GardenLocation %in% gardenlist),]


write.csv(YV.org, file.path(path.out, file = "Quercus_YV_final.csv"), row.names=FALSE)

View(YV.orgt)



YV.org <- YV.org[(YV.org$Taxon %like% taxalist),]