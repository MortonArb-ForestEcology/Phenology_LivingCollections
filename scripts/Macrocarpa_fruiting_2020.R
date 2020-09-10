# Attempting to get the current fruit status for Quercus Macrocarpa

# install.packages("devtools")
library('devtools')
# devtools::install_github("usa-npn/rnpn")

library(googlesheets4); library(car); library(lubridate)
library(ggplot2); library(tidyr)

path.out <- "G:/My Drive/LivingCollections_Phenology/Observing Lists/"


# Making some directories to help with organization
## NOTE: This assumes you opened R by double-clicking this script in your github folder.  Your working directory (in the bar of the "Console" tab) should be [SOMETHIGN]/Collections-Habitat/scripts
# If it's not, you'll need to set your working directory to be here
# Once you do that, we can use the same file paths without having to worry about differences in where your github folder is vs. mine

if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

# -----------------------------------
# 1. Arb Data
# -----------------------------------
source("clean_google_form.R")


# Downloading 2020 data
quercus20 <- clean.google(collection="Quercus", dat.yr=2020)
quercus20$Collection <- as.factor("Quercus")
quercus20$Year <- lubridate::year(quercus20$Date.Observed)
summary(quercus20)

# Putting 2020 into the a data frame to make working with it easier
quercus <- rbind(quercus20)
summary(quercus) # makign sure this worked; check date & year columns to make sure they make sense

head(quercus)

# start with just getting QUMA
summary(quercus$Species=="Quercus macrocarpa")
quma <- quercus[quercus$Species=="Quercus macrocarpa",]
summary(quma)

#Checking to make sure it is just Quercus Macrocarpa
head(quma)



#Creating a data frame of all Quercus macrocarpa  with fruit present for Nicole
quma.fp <- quma[quma$fruit.present.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "fruit.present.observed")]

View(quma.fp)

#writing a CSV 
write.csv(quma.fp, file.path(path.out, file = "Macrocarpa_Current_Fruit.csv"), row.names=FALSE)

#I dont know if this is necessary but this seperates out the species column into two seperate columns
#genus and species incase we are seeing all quercus with fruit present then we can just pull macrocarpa
#quma.fp <- quma.fp %>% separate(Species, c("Genus", "Species"), " ", extra="merge")