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


# Downloading 2019 data
quercus19 <- clean.google(collection="Quercus", dat.yr=2019)
quercus19$Collection <- as.factor("Quercus")
quercus19$Year <- lubridate::year(quercus19$Date.Observed)
summary(quercus19)

# Downloading 2018 data
quercus18 <- clean.google(collection="Quercus", dat.yr=2018)
quercus18$Collection <- as.factor("Quercus")
quercus18$Year <- lubridate::year(quercus18$Date.Observed)
summary(quercus18)

# Putting 2020, 2019 and 2018 into the same data frame to make working with it easier (in the long run; might be hard at first)
quercus.all <- rbind(quercus18, quercus19, quercus)
summary(quercus.all)

# Creating a yday (day of year) column that you'll want to do all of your indexing
quercus.all$yday <- lubridate::yday(quercus.all$Date.Observed)
summary(quercus.all)

#maybe this?
quercus.fr <- subset(quercus.all [,c("PlantNumber","leaf.color.observed")], simplify = TRUE, drop = TRUE)
summary (quercus.fr)

#I can also try this
quercus.lc <- quercus.all[quercus.all$leaf.color.observed=="Yes", c("Observer", "Date.Observed", "Species", "PlantNumber", "leaf.color.observed")]
summary(quercus.lc)
head(quercus.lc)


#finding the minimimum and maximum range and mean of the dates fall color was observed on our trees.
min(quercus.lc$Date.Observed)
max(quercus.lc$Date.Observed)
range(quercus.lc$Date.Observed)
mean(quercus.lc$Date.Observed)

#Now make my Yday
quercus.lc$yday <- lubridate::yday(quercus.lc$Date.Observed)
summary(quercus.lc)
