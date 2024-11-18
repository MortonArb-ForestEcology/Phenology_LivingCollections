# A new script with some combined collections analyses/graphs for the end of the year report
library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)
library(gganimate)
library(dplyr)
library(transformr)

path.google <- "~/Google Drive/My Drive" # Mac
path.dat <- file.path(path.google,"/LivingCollections_Phenology/Data_Observations")
#path.figs <- file.path(path.google, "LivingCollections_Phenology/Reports/2023_02_EndOfYear_Report/figures_2023_end")
# this is for google -># 
path.figs <- "~/Google Drive/My Drive/LivingCollections_Phenology/Reports/2024_01_Mid_Year_Report/figures_2024"
if(!dir.exists("../data")) dir.create("../data/")
if(!dir.exists("../figures/")) dir.create("../figures/")

#Reading in arb data -----------------------------------


#Reading in the historic data for the previous years

dat.22 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2022_FINAL.csv"))
dat.21 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2021_FINAL.csv"))
dat.20<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2020_FINAL.csv"))
dat.19 <- read.csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2019_FINAL.csv"))
dat.18 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_All_2018_FINAL.csv"))
#reading in 2023 data
acer23<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Acer_2023_FINAL.csv"))
quercus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2023_FINAL.csv"))
ulmus23 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Ulmus_2023_FINAL.csv"))
#reading in 2024 data
acer24<- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Acer_2024_sofar.csv"))
quercus24 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Quercus_2024_sofar.csv"))
ulmus24 <- read_csv(file.path(path.dat,"LivingCollectionPhenology_ObservationData_Ulmus_2024_sofar.csv"))
##binding 
dat.23<- rbind(ulmus23, quercus23, acer23)
dat.24<- rbind(ulmus24, quercus24, acer24)
##one line to bind them all
dat.all <- rbind(dat.24,dat.23, dat.22, dat.21, dat.20, dat.19, dat.18)

#getting the correct date format
dat.all$yday <- lubridate::yday(dat.all$Date.Observed)
dat.all$Date <- as.Date(paste0("2018-", dat.all$yday), format="%Y-%j")

##werid 2027 value
summary(dat.all)
#removing the "Q.macrocarpa" collection because that is not necessary for the end of year pheology report 
dat.all <- dat.all[!(dat.all$Collection %in% c("Q. macrocarpa", "Tilia", 'acer')), ]

#Checking what is present in the collection column to see if it's been removed
unique(dat.all$Collection)

#Setting a spring only data frame because we did not make observations in spring 2020
dat.spring <- dat.all[dat.all$Year != "2020", ]
#further excluding all elm observations for 2021 since we did not observe phenology that spring
dat.spring <- dat.spring[!(dat.spring$Collection == "Ulmus" & dat.spring$Year == "2021"), ]
##checking unique values of the Year and Collection column in dat.spring
unique(dat.spring$Year)
unique(dat.spring$Collection)
##somewhere we have a year listed as 2027..come back to this. 
dat.huh <- dat.all[dat.all$Year == "2027", ]
head(dat.huh)


dat.all <- dat.all[dat.all$Collection == 'Quercus', ]

# First, let's create a function to process the flowering data
analyze_fall_flowering <- function(dat) {
  # Convert Date.Observed to Date format if it isn't already
  dat$Date.Observed <- as.Date(dat$Date.Observed)
  
  # Create a flag for observations after August 1st
  dat$fall_observation <- dat$Date.Observed >= as.Date(paste0(dat$Year, "-08-01"))
  
  # Combine flower observations into a single "flowering" flag
  dat$flowering <- dat$flower.buds.observed == "yes" | 
    dat$flower.open.observed == "yes" |
    dat$flower.pollen.observed == "yes"
  
  # Group by Year, Species, and PlantNumber to find trees with fall flowering
  fall_flowering_trees <- dat %>%
    filter(fall_observation == TRUE) %>%
    group_by(Year, Species, PlantNumber) %>%
    summarize(
      fall_flower_obs = sum(flowering, na.rm = TRUE),
      has_fall_flowers = fall_flower_obs >= 2
    ) %>%
    ungroup()
  
  # Calculate proportion of trees per species showing fall flowering
  species_proportions <- fall_flowering_trees %>%
    group_by(Year, Species) %>%
    summarize(
      total_trees = n(),
      flowering_trees = sum(has_fall_flowers, na.rm = TRUE),
      proportion = flowering_trees / total_trees
    ) %>%
    ungroup()
  
  # Identify species that showed fall flowering every year
  species_consistency <- species_proportions %>%
    group_by(Species) %>%
    summarize(
      years_with_flowers = sum(flowering_trees > 0),
      total_years = n_distinct(Year),
      consistent = years_with_flowers == total_years
    )
  
  return(list(
    proportions = species_proportions,
    consistency = species_consistency
  ))
}

# Run the analysis
results <- analyze_fall_flowering(dat.all)

# Create visualization
ggplot(results$proportions, aes(x = Year, y = proportion, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Trees Showing Fall Flowering by Species",
    subtitle = "Based on ≥2 flowering observations after August 1st",
    x = "Year",
    y = "Proportion of Trees",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Create a summary table for consistent fall flowering species
consistent_species <- results$consistency %>%
  arrange(desc(years_with_flowers), Species)               
