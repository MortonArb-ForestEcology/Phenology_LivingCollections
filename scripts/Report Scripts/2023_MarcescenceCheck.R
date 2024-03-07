# Script to see what trees may be potentially marcescent based on late observations of leaves in our phenology data
# Technically if hte leaves are dried leaves=N, but i'm not sure our vols will remember that

# Establishing a new script to pull data from out SQL server

# From Ross
library(DBI)
library(RPostgreSQL)
library(googlesheets4)

# Get a list of removed trees 

# googledrive::drive_auth()
# googlesheets4::gs4_deauth()

googlesheets4::gs4_auth(email="crollinson@mortonarb.org")
removed <- googlesheets4::read_sheet("16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ", sheet="Removed Trees")
names(removed)

# Following code from Ross Alexander and these resources:
# https://www.r-bloggers.com/2015/05/getting-started-with-postgresql-in-r/
# https://hevodata.com/learn/rpostgresql/
# https://medium.com/geekculture/a-simple-guide-on-connecting-rstudio-to-a-postgresql-database-9e35ccdc08be
# https://www.datacareer.de/blog/connect-to-postgresql-with-r-a-step-by-step-example/

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

observers <- dbReadTable(conn, "Observers")
assignments <- dbReadTable(conn, "ObserverListCollection")
treeLists <- dbReadTable(conn, "ObservingLists")
datAll <- dbReadTable(conn, "FormSubmission")

### DISCONNECT FROM DATABASE
dbDisconnect(conn)


### Cleaning up data
# Removing missing trees from our tree list
treeLists <- treeLists[!treeLists$PlantID %in% removed$PlantNumber,]

summary(datAll)
dim(datAll)

for(COL in names(datAll)[!names(datAll) %in% c("DateEntered", "DateObserved")]){
  datAll[,COL] <- as.factor(datAll[,COL])
}

datAll[datAll==""] <- NA
datAll <- droplevels(datAll)

# Get rid of data for trees that have been removed
datAll <- datAll[!datAll$PlantID %in% removed$PlantNumber,]
datAll$Species <- trimws(datAll$Species)

summary(datAll)


# Checking for potentential marcescence

potMar <- which(datAll$DateObserved>as.Date("2023-12-01") & datAll$LeafObserved %in% c("?", "y"))
summary(datAll[potMar,])
datAll[potMar,]


length(unique(as.factor(paste(datAll$Genus, datAll$Species))))

sppMar <- summary(as.factor(paste(datAll$Genus[potMar], datAll$Species[potMar])))
length(sppMar)
sppMar[sppMar>5]
