# Establishing a new script to pull data from out SQL server

# From Ross
library(DBI)
library(RPostgreSQL)

# Get a list of removed trees 
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
treeLists <- dbReadTable(conn, "ObservingLists")
datAll <- dbReadTable(conn, "FormSubmission")

### DISCONNECT FROM DATABASE
dbDisconnect(conn)

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


# Checking for trees whose PlantID isn't in our list
# # NOTE: Need to 
datAll[!datAll$PlantID %in% c(treeLists$PlantID, removed$PlantNumber),] 

# Genus not in the three we're focusing on
datAll[!datAll$Genus %in% c("Quercus", "Acer", "Ulmus"), ]

# Finding specific epithets that don't match at all
sppReal <- unique(treeLists$Taxon)

epithetDat <- unique(datAll$Species)
epiprob <- epithetDat[!sapply(epithetDat, FUN=function(x){any(grepl(x, sppReal))})]

summary(datAll[datAll$Species %in% epiprob, ])

datAll[datAll$Species %in% epiprob, ]


treeLists[treeLists$PlantID=="15-2008*1",]

# Checking for genus-species combos
unique((datAll[!paste(datAll$Genus, datAll$Species) %in% c(unique(treeLists$Taxon)),c("Species")]))


# This may be slow, but now checking for entries where the species doesn't match what it shoudl
datBad <- data.frame()
for(TREEID in unique(datAll$PlantID)){
  datNow <- datAll[datAll$PlantID==TREEID, ]
  WTF <- datNow[!paste(datNow$Genus, datNow$Species) == treeLists$Taxon[treeLists$PlantID==TREEID],]
  
  if(nrow(WTF)>0) datBad <- rbind(datBad, WTF)
}
dim(WTF)

treeLists[treeLists$PlantID=="262-2017*2",]


# listProb <- treeLists[grepl("Quercus", treeLists$Taxon) & treeLists$List==4,] # Replace 4 with whatever list the trouble maker is assigned to; then search by that observer's name & possible a date
# listProb[order(listProb$PlantID),]
# 
# treeLists[treeLists$PlantID=="400-2013*1",]


# Checking for trees whose genus + species don't match our list
sppDat <- paste(datAll$Genus, datAll$Species)


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
