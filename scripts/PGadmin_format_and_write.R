library(DBI)
library(RPostgreSQL)
library(googlesheets4)
library(readr)
library(bit)
library(dplyr)
####Pathing
#BR: Make this work later 

#pgadminconnect
db_host <- "164.92.83.213" #host name/address  
db_port <- 5432 #port
db_name <- "arboretum" #data base name we are interested in querying, for this project, so far there is only one
db_user <- "arboretum" # user name
db_password <- "arboretum1234" # password for the user, we only have one user, and the password has been set to this 

# Create a connection to the database
con <- dbConnect(
  PostgreSQL(),
  host = db_host,
  port = db_port,
  dbname = db_name,
  user = db_user,
  password = db_password
)

# Check for successful connection with these two print statements
if (inherits(con, "PostgreSQLConnection")) 
{
  cat("Connected to PostgreSQL database")
} else {
  cat("Failed to connect to PostgreSQL database")
}

tables <- dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'")

# select the "FormSubmission" and put into the dat.all data fram, this is the table containing all the data entered from the app. 
dat.pg24 <- dbGetQuery(con, 'SELECT * FROM public."FormSubmission"')

###Script to write out PGAdmin phenology data to match existing format of old phenolgy data 
## Renaming and organizing columns 
# Get the column names of the pgadmin data
names(dat.pg24)
# Renaming genus and species columns to create a column "species which has both the genus and species names
# and a column named collection which contains only the genus name
colnames(dat.pg24)[which(colnames(dat.pg24) == "Species")] <- "delete"
colnames(dat.pg24)[which(colnames(dat.pg24) == "Genus")] <- "Collection"
names(dat.pg24)
# Combining these two columns into the new Species column
dat.pg24$Species <- paste(dat.pg24$Collection, dat.pg24$delete)
names(dat.pg24)
# deleteing the extra column titles "delete" here
dat.pg24 <- dat.pg24[, !(names(dat.pg24) == "delete")]
names(dat.pg24)

dat.pg24$Year <- format(as.Date(dat.pg24$DateObserved), "%Y")
names(dat.pg24)


#get the column names of existing phenology data
##  =^.^=  =^.^=  =^.^= BR: Do a better job pathing this =^.^=  =^.^= =^.^=
gs4_auth()
ac19 <- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations/LivingCollectionPhenology_ObservationData_Acer_2019_FINAL.csv")
#View(ac19)
names(ac19)

#renaming the column names of pgadmin data to match those of existing pheno data
colnames(dat.pg24)<- c("PlantNumber","Observer","Collection","Timestamp","Date.Observed","leaf.present.observed",
                       "leaf.breaking.buds.observed","leaf.color.observed","leaf.increasing.observed","leaf.falling.observed",
                       "flower.buds.observed","flower.open.observed","flower.pollen.observed","fruit.present.observed",
                       "fruit.ripe.observed","fruit.drop.observed","leaf.present.intensity","leaf.breaking.buds.intensity",
                       "leaf.color.intensity","flower.buds.intensity","flower.open.intensity","flower.pollen.intensity",
                       "fruit.present.intensity","fruit.ripe.intensity","fruit.drop.intensity","leaf.increasing.intensity",
                       "Notes", "Species", "Year")
#Check
names(dat.pg24)
 # Specify the new order of columns
new_order <- c("Timestamp", "Observer", "Date.Observed", "Species", "PlantNumber",
               "Notes", "leaf.breaking.buds.observed", "leaf.breaking.buds.intensity",
               "leaf.present.observed", "leaf.present.intensity", "leaf.increasing.observed",
               "leaf.increasing.intensity", "leaf.color.observed", "leaf.color.intensity",
               "leaf.falling.observed", "flower.buds.observed", "flower.buds.intensity",
               "flower.open.observed", "flower.open.intensity", "flower.pollen.observed",
               "flower.pollen.intensity", "fruit.present.observed", "fruit.present.intensity",
               "fruit.ripe.observed", "fruit.ripe.intensity", "fruit.drop.observed",
               "fruit.drop.intensity", "Collection", "Year")

# Reorganize the columns
dat.pg24 <- dat.pg24[, new_order]

# Viewing the and checking
dat.pg24
  #View(dat.pg24)
### checking with rbind
#dat.fake <- rbind(ac19,dat.pg24)

table(dat.pg24$leaf.present.observed)
###If this works ensure the values for y and n are changed to Yes and No,  and "d is "Did not look for" 
colupdate <- c("leaf.present.observed", "leaf.breaking.buds.observed", 
                       "leaf.color.observed", "leaf.increasing.observed", 
                       "leaf.falling.observed", "flower.buds.observed", 
                       "flower.open.observed", "flower.pollen.observed", 
                       "fruit.present.observed", "fruit.ripe.observed", 
                       "fruit.drop.observed")

# Update "y" to "Yes" and "n" to "No" in the specified columns
dat.pg24 <- dat.pg24 %>%
  mutate_at(
    vars(colupdate),
    funs(
      case_when(
        . == "y" ~ "Yes",
        . == "n" ~ "No",
        . == "d" ~ "Did not look for",
        TRUE ~ .
      )
    )
  )
### Checking to see with a table of the unique values in the leaf present column
table(dat.pg24$leaf.present.observed)

## If this works write out CSVs by collection

# Using the unique collection names and years in the Collection and Year columns
unique_combinations <- unique(dat.pg24[, c("Collection", "Year")])

for (i in 1:nrow(unique_combinations)) {
  collection_name <- unique_combinations$Collection[i]
  year <- unique_combinations$Year[i]
  
  # Create a subset for the current collection and year
  subset_data <- dat.pg24[dat.pg24$Collection == collection_name & dat.pg24$Year == year, ]
  
  # Generate a CSV file name based on the specified format
  csv_file_name <- paste0("LivingCollectionPhenology_ObservationData_", gsub(" ", "_", collection_name), "_", year, "_sofar.csv")
  
  # Write out the subset to a CSV file
  write.csv(subset_data, csv_file_name, row.names = FALSE)
  
  cat("CSV file", csv_file_name, "created for collection", collection_name, "and year", year, "\n")
}
##
# Disconnect from the pgadming data base
dbDisconnect(con)
