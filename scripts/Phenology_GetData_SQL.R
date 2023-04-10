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

observers <- dbReadTable(conn, "Observers")
treeLists <- dbReadTable(conn, "ObservingLists")
datAll <- dbReadTable(conn, "FormSubmission")

# observers[order(observers$ObserverID),]

### DISCONNECT FROM DATABASE
dbDisconnect(conn)

summary(datAll)
dim(datAll)

for(COL in names(datAll)[!names(datAll) %in% c("DateEntered", "DateObserved")]){
  datAll[,COL] <- as.factor(datAll[,COL])
}

datAll[datAll==""] <- NA
datAll <- droplevels(datAll)

# cleaning up some relatively minor issues with the data
# # datAll$Genus <- gsub(" ", "", datAll$Genus)
# datAll$Species <- gsub("  ", " ", datAll$Species)
# datAll$Species <- tolower(datAll$Species)
# summary(datAll)

# Checking for immediate issues all related to bad entry
# Checking for observers not in our existing ID list
datAll[!datAll$ObserverID %in% c(observers$ObserverID, "UNKNOWN"),]

# Checking for trees whose PlantID isn't in our list
datAll[!datAll$PlantID %in% c(treeLists$PlantID),] 

# Genus not in the three we're focusing on
datAll[!datAll$Genus %in% c("Quercus", "Acer", "Ulmus"), ]

# Finding specific epithets that don't match att all
epithetDat <- unique(datAll$Species)
epithetDat[!sapply(epithetDat, FUN=function(x){any(grepl(x, sppReal))})]

treeLists[treeLists$PlantID=="185-92*2",]
listProb <- treeLists[grepl("Quercus", treeLists$Taxon) & treeLists$List==4,] # Replace 4 with whatever list the trouble maker is assigned to; then search by that observer's name & possible a date
listProb[order(listProb$PlantID),]


# Checking for trees whose genus + species don't match our list
sppDat <- paste(datAll$Genus, datAll$Species)
sppReal <- unique(treeLists$Taxon)


# Need to find a way to remove leading spaces from specific epithets
unique((datAll[!paste(datAll$Genus, datAll$Species) %in% c(unique(treeLists$Taxon)),c("Species")]))
# treeLists[grep("humidicola", treeLists$Taxon),]
# datAll[datAll$Species=="humidicola",]
unique(paste(datAll$Genus[datAll$Species=="humidicola"], datAll$Species[datAll$Species=="humidicola"])) %in% treeLists$Taxon
grep("microcarpa", treeLists$Taxon)
treeLists[treeLists$PlantID=="15-2008*1",]


unique(sppDat)
unique(treeLists$Taxon)
head(treeLists)
summary(datAll[!paste(datAll$Genus, datAll$Species) %in% c(treeLists$Taxon),c("PlantID", "ObserverID", "Genus", "Species", "DateEntered")])
length(unique((datAll[!paste(datAll$Genus, datAll$Species) %in% c(treeLists$Taxon),c("PlantID")])))
length(unique((datAll[!paste(datAll$Genus, datAll$Species) %in% c(treeLists$Taxon),c("Species")])))
length(unique((datAll[!paste(datAll$Genus, datAll$Species) %in% c(treeLists$Taxon),c("ObserverID")])))

unique
unique(paste(datAll$Genus, datAll$Species, sep=""))


# PlantIDAll <- unique(datAll$PlantID)
datAll[grepl("[/]", datAll$PlantID),]
datAll[!grepl("[-]", datAll$PlantID) | !grepl("[*]", datAll$PlantID),]

summary(datAll$ObserverID)

# Deeper QAQC checks --> PlantIDs not in our list; PlantIDs don't match genus/species in entry; Observers not in our list


# write.csv(datAll, "~/Google Drive/My Drive/LivingCollections_Phenology/LivingCollectionPhenology_ObservationData_LAdatAll.csv", row.names=F)
