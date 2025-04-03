# Script to run some quick stats and figures to include in monthly phenology reports

library(ggplot2)
pathDat <- "~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations"

datNow <- read.csv(file.path(pathDat, paste0("LivingCollectionPhenology_ObservationData_All_", lubridate::year(Sys.Date()), "_latest.csv")))

datNow <- datNow[datNow$ObserverID!="Test",]
summary(datNow)
head(datNow)
# Loop through our columns to make the formats what we want
for(COL in names(datNow)){
  if(COL == "Comments") next
  if(COL %in% c("DateEntered", "DateObserved")){
    datNow[,COL] <- as.Date(datNow[,COL])
  } else {
    datNow[,COL] <- as.factor(datNow[,COL])
  }
}
summary(datNow)

summary(datNow[datNow$ObserverID=="Rose",])
tail(datNow[datNow$ObserverID=="Rose",])

# summary(datNow[datNow$ObserverID=="Xiao",])


# datNow[datNow$ObserverID=="Test",]

length(unique(datNow$ObserverID))
length(unique(datNow$PlantID))
length(unique(paste(datNow$Genus, datNow$Species)))

# Looking at how many trees have had leaf budburst yet
datBud <- aggregate(DateObserved ~ PlantID + Genus + Species + BreakingLeafBudsObserved, data=datNow, FUN=length)

summary(datBud[datBud$BreakingLeafBudsObserved=="y",])
length(unique(datBud$PlantID[datBud$BreakingLeafBudsObserved=="y"]))/length(unique(datBud$PlantID))
length(unique(datBud$PlantID[datBud$Genus=="Acer"]))
length(unique(datBud$PlantID[datBud$Genus=="Ulmus"]))


# Looking at Flower buds
datFlowerBud <- aggregate(DateObserved ~ PlantID + Genus + Species + FlowerBudsObserved, data=datNow, FUN=length)

summary(datFlowerBud[datFlowerBud$FlowerBudsObserved=="y",])
length(unique(datFlowerBud$PlantID[datFlowerBud$FlowerBudsObserved=="y"]))/length(unique(datFlowerBud$PlantID))
length(unique(datFlowerBud$PlantID[datFlowerBud$Genus=="Acer"]))
length(unique(datFlowerBud$PlantID[datFlowerBud$Genus=="Ulmus"]))


# Looking at Open Flowers
datFlowerOpen <- aggregate(DateObserved ~ PlantID + Genus + Species + FlowerOpenObserved, data=datNow, FUN=length)

summary(datFlowerOpen[datFlowerOpen$FlowerOpenObserved=="y",])
length(unique(datFlowerOpen$PlantID[datFlowerOpen$FlowerOpenObserved=="y"]))/length(unique(datFlowerOpen$PlantID))
length(unique(datFlowerOpen$PlantID[datFlowerOpen$Genus=="Acer"]))
length(unique(datFlowerOpen$PlantID[datFlowerOpen$Genus=="Ulmus"]))
