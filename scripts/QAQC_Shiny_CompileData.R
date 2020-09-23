# Making some better graphs to show what's going on with the phenology in the living collections

library(googlesheets4)
library(ggplot2); library(grid) # graphing packages
library(plotly)
library(car)
library(lubridate)

# ----------------
# get the data from each collection
# ----------------
# Source my cleaning function
source("clean_google_form.R")

quercus <- clean.google(collection="Quercus", dat.yr=lubridate::year(Sys.Date()), gsauth="crollinson@mortonarb.org")
# summary(quercus)
# tail(quercus)
# quercus[quercus$Date.Observed>Sys.Date(),1:6]

acer <- clean.google(collection="Acer", dat.yr=lubridate::year(Sys.Date()))
# summary(acer)

ulmus <- clean.google(collection="Ulmus", dat.yr=lubridate::year(Sys.Date()))
# summary(ulmus)

dat.all <- rbind(quercus, acer, ulmus)
summary(dat.all)

# Add in observing lists
quercus.list <- read.csv(file.path("../data/ObservingLists", "ObservingLists_Quercus.csv"))
acer.list <- read.csv(file.path("../data/ObservingLists", "ObservingLists_Acer.csv"))
ulmus.list <- read.csv(file.path("../data/ObservingLists", "ObservingLists_Ulmus.csv"))
quercus.list$collection <- "Quercus"
acer.list$collection <- "Acer"
ulmus.list$collection <- "Ulmus"

obs.list <- rbind(quercus.list, acer.list, ulmus.list)
obs.list$Obs.List <- paste(obs.list$collection, obs.list$Obs.List, sep="-")

# summary(obs.list)

dat.all <- merge(dat.all, obs.list[,c("Obs.List", "collection", "PlantNumber", "BgLatitude", "BgLongitude")])
dat.all$Obs.List <- as.factor(dat.all$Obs.List)
dat.all$collection <- as.factor(dat.all$collection)
# summary(dat.all)

# ----------------

# summary(dat.all)

#extracting the month to put into graph
dat.all$Month <- month(as.POSIXlt(dat.all$Date.Observed, format="%Y/%m/%d"))
dat.all$Day <- day(as.POSIXlt(dat.all$Date.Observed, format="%Y/%m/%d"))
dat.all$Year <- year(as.POSIXlt(dat.all$Date.Observed, format="%Y/%m/%d"))
# dat.all$Time <- format(dat.all$Timestamp, '%H:%M:%S')
# is.numeric(dat.all$Date.Observed)
# 
# summary(dat.all)


# dat.all.spp <- paste(unique(dat.all$Species))
phenophases <- names(dat.all)[grep(".observed", names(dat.all))]
intensity <- names(dat.all)[grep(".intensity", names(dat.all))]

# Converting phenophases in to character columns instead of factors
for(i in 1:length(phenophases)){
  dat.all[,phenophases[i]] <- as.character(dat.all[,phenophases[i]])
}
for(i in 1:length(intensity)){
  dat.all[, intensity[i]] <- as.character(dat.all[, intensity[i]])
}

dat.all.stack <- stack(dat.all[,phenophases], drop=F)
names(dat.all.stack) <- c("status", "phenophase")
dat.all.stack$phenophase <- gsub(".observed", "", dat.all.stack$phenophase)
dat.all.stack[,c("collection", "Obs.List", "Observer", "Date.Observed", "Species", "PlantNumber", "Timestamp", "Notes")] <- dat.all[,c("collection", "Obs.List", "Observer", "Date.Observed", "Species", "PlantNumber", "Timestamp", "Notes")]
# summary(dat.all.stack) 
# head(dat.all.stack)

dat.int <- stack(dat.all[,intensity], drop=F)
names(dat.int) <- c("intensity", "phenophase")
dat.int$phenophase <- gsub(".intensity", "", dat.int$phenophase)
dat.int[,c("collection", "Obs.List", "Observer", "Date.Observed", "Species", "PlantNumber", "Timestamp", "Notes")] <- dat.all[,c("collection", "Obs.List", "Observer", "Date.Observed", "Species", "PlantNumber", "Timestamp", "Notes")]

dat.all.stack <- merge(dat.all.stack, dat.int, all=T)
write.csv(dat.all.stack, "pheno_qaqc_shiny/pheno_compiled.csv", row.names=F)


#getting shiny to work
library(shiny)
library(shinydashboard)
library(htmltools)
#runExample("01_hello")

setwd("pheno_qaqc_shiny/")
rsconnect::deployApp(forceUpdate = T, launch.browser = F)
# shinyApp()