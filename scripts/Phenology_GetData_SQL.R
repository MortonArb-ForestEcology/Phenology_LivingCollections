# Establishing a new script to pull data from out SQL server

# Following code from Ross alexander and these resources:
# https://www.r-bloggers.com/2015/05/getting-started-with-postgresql-in-r/
# https://hevodata.com/learn/rpostgresql/
# https://medium.com/geekculture/a-simple-guide-on-connecting-rstudio-to-a-postgresql-database-9e35ccdc08be
# https://www.datacareer.de/blog/connect-to-postgresql-with-r-a-step-by-step-example/

# From Ross
library(DBI)
library(RPostgreSQL)

### CONNECT TO DATABASE (LOG ON ANL VPN IF OUTSIDE THE ANL OFFICE)
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(
  drv,
  host    = "164.92.83.213",
  dbname  = "arboretum",    
  user    = "arboretum", # RETRIEVE FROM ENVIRONMENT VARIABLE
  password = "arboretum1234",  # RETRIEVE FROM ENVIRONMENT VARIABLE
  port    = 5432
)

TEST <- dbReadTable(conn, "FormSubmission")

### DISCONNECT FROM DATABASE
dbDisconnect(conn)

summary(TEST)
for(COL in names(TEST)[!names(TEST) %in% c("DateEntered", "DateObserved")]){
  TEST[,COL] <- as.factor(TEST[,COL])
}

TEST[TEST==""] <- NA
TEST <- droplevels(TEST)
summary(TEST)

# Checking for immediate issues
TEST[!TEST$Genus %in% c("Quercus", "Acer", "Ulmus"), ]
# PlantIDAll <- unique(TEST$PlantID)
TEST[grepl("[/]", TEST$PlantID),]
TEST[!grepl("[-]", TEST$PlantID) | !grepl("[*]", TEST$PlantID),]

summary(TEST$ObserverID)

# Deeper QAQC checks --> PlantIDs not in our list; PlantIDs don't match genus/species in entry; Observers not in our list

# write.csv(TEST, "~/Google Drive/My Drive/LivingCollections_Phenology/LivingCollectionPhenology_ObservationData_LATEST.csv", row.names=F)
