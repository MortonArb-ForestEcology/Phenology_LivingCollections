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

# cleaning up some relatively minor issues with the data
# # datAll$Genus <- gsub(" ", "", datAll$Genus)
# datAll$Species <- gsub("  ", " ", datAll$Species)
# datAll$Species <- tolower(datAll$Species)
# summary(datAll)

# Checking for data on some trees reported missing by one observer
# datAll[datAll$PlantID %in% c("19-2012*1", "5-2018*4", "37-2014*1"),]

# Checking for immediate issues all related to bad entry
# Checking for observers not in our existing ID list
datAll[!datAll$ObserverID %in% c(observers$ObserverID, "UNKNOWN"),]
unique(datAll$ObserverID[!datAll$ObserverID %in% c(observers$ObserverID, "UNKNOWN")])
oddObservers <- summary(datAll$ObserverID[!datAll$ObserverID %in% c(observers$ObserverID, "UNKNOWN")])
oddObservers[oddObservers>0]

# treeLists[treeLists$PlantID=="1261-26*2", ]
# datAll[datAll$ObserverID=="Haraf" & datAll$DateObserved=="2023-05-09",c("PlantID", "ObserverID", "Genus", "Species")]

# Checking for trees whose PlantID isn't in our list
# # NOTE: Need to 
datAll[!datAll$PlantID %in% c(treeLists$PlantID, removed$PlantNumber),] 

# Checking for an Observer monitoring something in their normal list (usually a typo!)
datOdd <- data.frame()
for(OBSID in unique(datAll$ObserverID)){
  if(!OBSID %in% assignments$ObserverID) next
  
  datNow <- datAll[datAll$ObserverID==OBSID, ]
  ObsAssign <- assignments[assignments$ObserverID==OBSID,]
  
  # Building the tree list the ugly way, but it'll work
  treesAssign <- data.frame()
  for(i in 1:nrow(ObsAssign)){
    treesAssign <- rbind(treesAssign, treeLists[grepl(ObsAssign$Collection[i], treeLists$Taxon) & treeLists$List==ObsAssign$List[i],])
    
  }
  
  WTF <- datNow[!datNow$PlantID %in% treesAssign$PlantID,]
  
  if(nrow(WTF)>0) datOdd <- rbind(datOdd, WTF)
}
dim(datOdd)
datOdd[,c("PlantID", "ObserverID", "Genus", "Species", "DateEntered", "DateObserved")]




# This may be slow, but now checking for entries where the species doesn't match what it is in our records
datBad <- data.frame()
for(TREEID in unique(datAll$PlantID)){
  datNow <- datAll[datAll$PlantID==TREEID, ]
  WTF <- datNow[!paste(datNow$Genus, datNow$Species) == treeLists$Taxon[treeLists$PlantID==TREEID],]
  
  if(nrow(WTF)>0) datBad <- rbind(datBad, WTF)
}
dim(datBad)
datBad[,c("PlantID", "ObserverID", "Genus", "Species", "DateEntered", "DateObserved")]


# treeLists[treeLists$PlantID=="18-2012*1",]
# treeLists[grep("pumila", treeLists$Taxon),]
# 
# datAll[grep("pumila x rubra", datAll$Species),]


write.csv(datAll, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_All_", lubridate::year(Sys.Date()), "_latest.csv")), row.names=F)
