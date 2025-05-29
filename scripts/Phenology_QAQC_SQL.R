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

# Remove "Test" from datAll off the bat
datAll <- datAll[datAll$ObserverID!="Test",]

### DISCONNECT FROM DATABASE
dbDisconnect(conn)


### Cleaning up data
# Removing missing trees from our tree list
treeLists <- treeLists[!treeLists$PlantID %in% removed$PlantNumber,]

# Subset data to just date entered in the current year
datNow <- datAll[!is.na(datAll$DateEntered) & datAll$DateEntered>=as.Date(paste0(lubridate::year(Sys.Date()), "-01-01")),]
summary(datNow)
dim(datNow)

for(COL in names(datNow)[!names(datNow) %in% c("DateEntered", "DateObserved", "Comments")]){
  datNow[,COL] <- as.factor(datNow[,COL])
}
dim(datNow)
datNow[datNow==""] <- NA
datNow <- droplevels(datNow)
dim(datNow)

# Get rid of data for trees that have been removed
datNow <- datNow[!datNow$PlantID %in% removed$PlantNumber,]
datNow$Species <- trimws(datNow$Species)
summary(datNow)

# cleaning up some relatively minor issues with the data
# # datNow$Genus <- gsub(" ", "", datNow$Genus)
# datNow$Species <- gsub("  ", " ", datNow$Species)
# datNow$Species <- tolower(datNow$Species)
# summary(datNow)

# Checking for data on some trees reported missing by one observer
# datNow[datNow$PlantID %in% c("19-2012*1", "5-2018*4", "37-2014*1"),]

# Checking for immediate issues all related to bad entry
# Checking for observers not in our existing ID list
dim(datNow[!datNow$ObserverID %in% c(observers$ObserverID, "UNKNOWN"),])
unique(datNow$ObserverID[!datNow$ObserverID %in% c(observers$ObserverID, "UNKNOWN")])
oddObservers <- summary(datNow$ObserverID[!datNow$ObserverID %in% c(observers$ObserverID, "UNKNOWN")])
oddObservers[oddObservers>0]

# treeLists[treeLists$PlantID=="1261-26*2", ]
# datNow[datNow$ObserverID=="Haraf" & datNow$DateObserved=="2023-05-09",c("PlantID", "ObserverID", "Genus", "Species")]

# Checking for trees whose PlantID isn't in our list
# # NOTE: Need to 
summary(datNow[!datNow$PlantID %in% c(treeLists$PlantID, removed$PlantNumber),] )
PlantIDsummary <- summary(datNow$PlantID[!datNow$PlantID %in% c(treeLists$PlantID, removed$PlantNumber)] )
PlantIDsummary[PlantIDsummary>0]
treeQ <- datNow[!datNow$PlantID %in% c(treeLists$PlantID, removed$PlantNumber),1:6]
treeQ[order(treeQ$PlantID),]

# Checking for an Observer monitoring something NOT in their normal list (usually a typo!)
datOdd <- data.frame()
for(OBSID in unique(datNow$ObserverID)){
  if(!OBSID %in% assignments$ObserverID) next
  
  datTMP <- datNow[datNow$ObserverID==OBSID, ]
  ObsAssign <- assignments[assignments$ObserverID==OBSID,]
  
  # Building the tree list the ugly way, but it'll work
  treesAssign <- data.frame()
  for(i in 1:nrow(ObsAssign)){
    treesAssign <- rbind(treesAssign, treeLists[grepl(ObsAssign$Collection[i], treeLists$Taxon) & treeLists$List==ObsAssign$List[i],])
    
  }
  
  WTF <- datTMP[!datTMP$PlantID %in% treesAssign$PlantID,]
  
  if(nrow(WTF)>0) datOdd <- rbind(datOdd, WTF)
}
dim(datOdd)
datOdd[,c("PlantID", "ObserverID", "Genus", "Species", "DateEntered", "DateObserved")]

# Saving the list of trees to check
write.csv(datOdd, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("CHECK--LivingCollectionPhenology_ObservationData_All_latest.csv")), row.names=F)

dim(datNow)

# This may be slow, but now checking for entries where the species doesn't match what it is in our records
datBad <- data.frame()
for(TREEID in unique(datNow$PlantID)){
  datTmp <- datNow[datNow$PlantID==TREEID, ]
  WTF <- datTmp[!paste(datTmp$Genus, datTmp$Species) == treeLists$Taxon[treeLists$PlantID==TREEID],]
  
  if(nrow(WTF)>0) datBad <- rbind(datBad, WTF)
}
dim(datBad)
datBad[,c("PlantID", "ObserverID", "Genus", "Species", "DateEntered", "DateObserved")]
summary(droplevels(datBad$ObserverID))

datBad[datBad$ObserverID=="Kozak Miller",c("PlantID", "ObserverID", "Genus", "Species", "DateEntered", "DateObserved")]

write.csv(datBad, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("CHECK--LivingCollectionPhenology_ObservationData_WeirdNames.csv")), row.names=F)

# Doing a summary of observers
length(unique(datNow$ObserverID))
length(unique(assignments$ObserverID))

obsSummary <- data.frame(ObserverID = unique(assignments$ObserverID))
for(i in 1:nrow(obsSummary)){
  OBID <- obsSummary$ObserverID[i]
  datObs <- datNow[datNow$ObserverID==OBID,]
  if(nrow(datObs)==0) next
  
  obsSummary[i, "nObs"] <- nrow(datObs)
  obsSummary[i,"lastObs"] <- max(datObs$DateObserved)
}

summary(obsSummary)
unique(datNow$ObserverID)[!unique(datNow$ObserverID) %in% unique(obsSummary$ObserverID)]


write.csv(obsSummary, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("CHECK--LivingCollectionPhenology_Summary-Observer.csv")), row.names=F)

# Doing a summary by list as well
dim(treeLists)
summary(treeLists)

for(i in 1:nrow(treeLists)){
  TREE <- treeLists$PlantID[i]
  datTree <- datNow[datNow$PlantID==TREE, ]
  treeLists[i,"nObs"] <- nrow(datTree)
  treeLists[i,"lastObs"] <- max(datTree$DateObserved)
  
}
treeLists$Collection <- unlist(lapply(strsplit(treeLists$Taxon, " "), function(x){x[1]}))
head(treeLists)
summary(treeLists)

listCheck <- aggregate(nObs ~ Collection + List, data=treeLists, FUN = sum)
listCheck$meanObs <- round(aggregate(nObs ~ Collection + List, data=treeLists, FUN = mean)[,"nObs"],0)
listCheck$lastObs <- aggregate(lastObs ~ Collection + List, data=treeLists, FUN = max)[,"lastObs"]
listCheck <- listCheck[order(listCheck$Collection, listCheck$List),]
write.csv(obsSummary, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("CHECK--LivingCollectionPhenology_Summary-List.csv")), row.names=F)

# treeLists[treeLists$PlantID=="18-2012*1",]
# treeLists[grep("pumila", treeLists$Taxon),]
# 
# datNow[grep("pumila x rubra", datNow$Species),]
summary(datNow)

write.csv(datNow, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_All_", lubridate::year(Sys.Date()), "_latest.csv")), row.names=F)

