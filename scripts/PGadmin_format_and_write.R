library(DBI)
library(RPostgreSQL)
library(googlesheets4)
library(readr)
library(bit)
####Pathing
#BR: Make this work later 
#pgadminconnect
db_host <- "164.92.83.213" #host name/address e  
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

# Check for successful connection with these two print statments
if (inherits(con, "PostgreSQLConnection")) 
{
  cat("Connected to PostgreSQL database")
} else {
  cat("Failed to connect to PostgreSQL database")
}
# List tables in the public schema
tables <- dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'")

# select the "FormSubmission" and put into the dat.all data fram, this is the table containing all the data entered from the app. 
dat.pg23 <- dbGetQuery(con, 'SELECT * FROM public."FormSubmission"')

###Script to write out PGAdmin phenology data to match existing format of old phenolgy data 
## Renaming and organizing columns 
# Get the column names of the pgadmin data
pgcolumn_names <- names(dat.pg23)
# Print the column names
print(pgcolumn_names)


#get the column names of existing phenology data
##  =^.^=  =^.^=  =^.^= BR: Do a better job pathing this =^.^=  =^.^= =^.^=
gs4_auth()
ac19 <- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations/LivingCollectionPhenology_ObservationData_Acer_2019_FINAL.csv")
View(ac19)

gscolumn_names <- names(ac19)
# Print the column names
print(gscolumn_names)

d

 # Specify the new order of column names
new_order <- c("DateEntered
               
               
             
               
               PlantID", "ObserverID", "Genus", "Species", "DateObserved",
               "LeafObserved", "BreakingLeafBudsObserved", "LeafColorObserved",
               "LeafIncreasingInSizeObserved", "LeafFallingObserved", "FlowerBudsObserved",
               "FlowerOpenObserved", "FlowerPollenObserved", "FruitObserved",
               "FruitRipeObserved", "FruitDropObserved", "LeafIntensity",
               "BreakingLeafBudsIntensity", "LeafColorObservedIntensity",
               "FlowerBudsObservedIntensity", "FlowerOpenObservedIntensity",
               "FlowerPollenObservedIntensity", "FruitObservedIntensity",
               "FruitRipeObservedIntensity", "FruitDropObservedIntensity",
               "LeafIncreasingInSizeIntensity", "Comments")

# Reorder the columns in the dataframe
dat.pg23 <- dat.pg23[, new_order]