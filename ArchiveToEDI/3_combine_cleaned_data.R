# Script for combining archive phenology data 
library(tidyverse)
# 1. Pathing -------------------------------------------------------------------
path.google <- "~/Google Drive/My Drive" # Mac
path.dat <- file.path(path.google,"/LivingCollections_Phenology/Data_Cleaned")
path.write <- "~/Google Drive/My Drive/LivingCollections_Phenology/EDI_Upload/Data"

## Reading in the historic data for the previous years--------------------------
# List of all csv files in the "Data_Cleaned" folder
files.dat <- list.files(path.dat, pattern = "_cleaned\\.csv$", full.names = TRUE)
# Exclude the Quercus-macrocarpa 2022
files.dat <- files.dat[!grepl("Quercus-macrocarpa_2022_cleaned\\.csv$", files.dat)]

#combining into da.all
dat.all <- files.dat %>%
  map_dfr(~ read_csv(.x) %>% mutate(SourceFile = basename(.x)))

## check on data type of columns before writing out ----------------------------
str(dat.all)
sapply(dat.all, class)
table(dat.all$SourceFile)  # confirms how many rows came from each file

#Check on range before writing out 
head(dat.all)
range(dat.all$Timestamp,na.rm = T)

# 2. Write out combined file --------------------------------------------------
write.csv(dat.all, file.path(path.write, "LivingCollectionPhenology_ObservationData_ALL_combined.csv"),
          row.names = FALSE)

