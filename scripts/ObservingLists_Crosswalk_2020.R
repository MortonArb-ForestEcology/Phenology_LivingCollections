# Crosswalking 2019 and 2020 monitoring lists
# Problems: 
#   -- when doing 2020 updates, number changed and that has caused some confusion
#   -- we overwrote old lists, meaning we've lost records of who had what or what old lists were
# Solution: 
#   -- for each person, get a list of the trees they had last year (based on their obs) and find
#      the 2020 list with the most overlap.

dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"



# Get the data from last year
source("clean_google_form.R")

# library(googlesheets)

quercus.2019 <- clean.google(collection="Quercus", dat.yr="2019")
quercus.2019$collection <- as.factor("Quercus")
summary(quercus.2019)

acer.2019 <- clean.google(collection="Acer", dat.yr="2019")
acer.2019$collection <- as.factor("Acer")
summary(acer.2019)

dat.all <- rbind(quercus.2019, acer.2019)
summary(dat.all)

# Get 2020 observing lists
quercus.list <- read.csv(file.path(dir.base, "Observing Lists/Quercus", "ObservingLists_Quercus.csv"))
acer.list <- read.csv(file.path(dir.base, "Observing Lists/Acer", "ObservingLists_Acer.csv"))
quercus.list$collection <- "Quercus"
acer.list$collection <- "Acer"
quercus.list$Obs.List <- paste(quercus.list$collection, quercus.list$Obs.List, sep="-")
acer.list$Obs.List <- paste(acer.list$collection, acer.list$Obs.List, sep="-")

obs.list <- rbind(quercus.list, acer.list)
summary(obs.list)

dat.all <- merge(dat.all, obs.list[,c("Obs.List", "collection", "PlantNumber")])
dat.all$Obs.List <- as.factor(dat.all$Obs.List)
dat.all$collection <- as.factor(dat.all$collection)
summary(dat.all)

# Getting observations by person by plant
obs.check <- aggregate(dat.all$Date.Observed, by=dat.all[,c("Observer", "collection", "Obs.List", "PlantNumber")], FUN=length)
# names(obs.check) <- c("Observer", "Observation.Last")
summary(obs.check)

# Getting the number of trees on each list for each person
obs.xwalk <- aggregate(x ~ Observer + collection + Obs.List, data=obs.check, FUN=length)
obs.xwalk$n.obs <- aggregate(x ~ Observer + collection + Obs.List, data=obs.check, FUN=sum)$x
summary(obs.xwalk)
obs.xwalk[obs.xwalk$Observer=="Houston",]
obs.xwalk[obs.xwalk$Observer=="Buerger",]

obs.xwalk2[obs.xwalk2$Observer=="Houston",]
obs.xwalk2[obs.xwalk2$Observer=="Buerger",]
