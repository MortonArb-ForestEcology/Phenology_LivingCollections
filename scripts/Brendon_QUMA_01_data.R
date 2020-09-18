# Attempting to get the min and max for leaves in one Quercus Macrocarpa

# install.packages("devtools")
library('devtools')
# devtools::install_github("usa-npn/rnpn")

library(googlesheets4); library(car); library(lubridate)
library(ggplot2)

#
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

#creating a data frame of just leaves present observed
quma.lp <- quma[quma$leaf.present.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "leaf.present.observed")]
summary(quma.lp)
dim(quma.lp)

# When pulling one tree, don't forget double = to get T/F
quercus.solo <- quma.lp[quma.lp$PlantNumber=="2390-26*2",]
summary(quercus.solo)
quercus.solo
dim(quercus.solo)

#finding the minimimum and maximum of the dates leaves pesent was observed on our tree.
min(quercus.solo$Date.Observed)
max(quercus.solo$Date.Observed)
range(quercus.solo$Date.Observed)
solo.dates <- range(quercus.solo$Date.Observed)

summary(solo.dates)

# How to pull multiple trees for the same species
# quercus.duo <- quma.lp[quma.lp$PlantNumber=="2390-26*2" | quma.lp$PlantNumber=="132-2015*1",]
quercus.duo <- quma.lp[quma.lp$PlantNumber %in% c("2390-26*2", "132-2015*1") & quma.lp$Observer=="Reidy",]
summary(quercus.duo)

#Plotting presence of fruit accoring to date in Quercus Marcocarpa
ggplot(data=quma, aes(x=Date.Observed)) +
  geom_histogram(aes(fill=fruit.present.observed), binwidth=7) +
  ggtitle("Quercus Macrocarpa fruit present")
dev.off()
