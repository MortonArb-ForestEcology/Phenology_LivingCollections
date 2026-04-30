library(readr)
library(readxl)
# set path
path.google <- "~/Google Drive/My Drive/LivingCollections_Phenology/"

#---- 1. Read in and format data

#read infull list of 2026 trees trees 
q26<- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/Quercus/ObservingList_Quercus_2026.csv")
u26<- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/Ulmus/ObservingList_Ulmus_2026.csv")
a26<- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists//Acer/ObservingList_Acer_2026.csv")

#read infull list of 2023 trees trees 
q23 <- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/OLD/Quercus/ObservingList_Quercus_2023.csv")
u23 <- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/OLD/Ulmus/ObservingList_Ulmus_2023.csv")
a23 <- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/OLD/Acer/ObservingList_Acer_2023.csv")

nrow(q23)
nrow(a23)
nrow(u23)


# Pull our existing removed trees list 
googlesheets4::gs4_auth(email="breidy@mortonarb.org")
removed <- googlesheets4::read_sheet("16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ", sheet="Removed Trees")
names(removed)

#changing names for easier comparison 
names(removed)[names(removed) == "PlantNumber"] <- "PlantID"

#seperate out by taxon 
qrem <- removed[grepl("Quercus", removed$Taxon, ignore.case = TRUE), ]
urem <- removed[grepl("Ulmus", removed$Taxon, ignore.case = TRUE), ]
arem <- removed[grepl("Acer", removed$Taxon, ignore.case = TRUE), ]


# reading in brahms pulls
Ubrahms <- read_excel("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/BRAHMS_Export20260414_UlmusAll-CR.xlsx")
Abrahms <- read_excel("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/BRAHMS_Export20260414_AcerAll-CR.xlsx")
Qbrahms <- read_excel("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/BRAHMS_Export20260414_QuercusAll-CR.xlsx")

#renaming columns
colnames(Ubrahms)[c(5, 15, 16, 18, 19, 28)] <- c("PlantID", "GardenGrid", "GardenSubgrid", "BgLatitude", "BgLongitude", "Taxon")
colnames(Abrahms)[c(5, 15, 16, 18, 19, 28)] <- c("PlantID", "GardenGrid", "GardenSubgrid", "BgLatitude", "BgLongitude", "Taxon")
colnames(Qbrahms)[c(5, 15, 16, 18, 19, 28)] <- c("PlantID", "GardenGrid", "GardenSubgrid", "BgLatitude", "BgLongitude", "Taxon")

#Selecting everything brahms says is alive and subsetting 
Ubrahms26 <- Ubrahms[Ubrahms$LivingStatus != "I", c("PlantID","GardenGrid", "GardenSubgrid","BgLatitude" ,"BgLongitude","Taxon")]
Abrahms26 <- Abrahms[Abrahms$LivingStatus != "I", c("PlantID","GardenGrid", "GardenSubgrid","BgLatitude" ,"BgLongitude","Taxon")]
Qbrahms26 <- Qbrahms[Qbrahms$LivingStatus != "I", c("PlantID","GardenGrid", "GardenSubgrid","BgLatitude" ,"BgLongitude","Taxon")]


# Trees in querucs 2023, and not in removed
 qnotrm <- anti_join(q23, qrem, by = "PlantID")
nrow(qnotrm)

# Trees in ulumus 2023, and not in removed
unotrm <- anti_join(u23, urem, by = "PlantID")
nrow(unotrm)

# Trees in acer 2023, and not in removed
anotrm <- anti_join(a23, arem, by = "PlantID")
nrow(anotrm)

#fortmatting for analysis

anotrm <- select(anotrm, -List, -Vernacular)
unotrm <- select(unotrm, -List, -Vernacular)
qnotrm <- select(qnotrm, -List, -Vernacular)

#Comparing our 2023 list to brahms
umiss <-anti_join(unotrm,Ubrahms26, by = "PlantID")
amiss <-anti_join(anotrm,Abrahms26, by = "PlantID")
qmiss <-anti_join(qnotrm,Qbrahms26, by = "PlantID")

#Comparing our brahm to our 2023 list
umiss1 <-anti_join(Ubrahms26,unotrm, by = "PlantID")
amiss1 <-anti_join(Abrahms26,anotrm, by = "PlantID")
qmiss1 <-anti_join(Qbrahms26,qnotrm, by = "PlantID")

#Print whats alive in our 2023 list but not brahms
umiss
amiss
qmiss


umiss1
amiss1
qmiss1

 
#Find trees on our removed treelist  that still appear in BRAHMS as alive
qinbrahms <- semi_join(qrem, Qbrahms26, by = "PlantID")
uinbrahms <- semi_join(urem, Ubrahms26, by = "PlantID")
ainbrahms <- semi_join(arem, Abrahms26, by = "PlantID")

# Print 
qinbrahms
uinbrahms
ainbrahms


nrow(qinbrahms)
nrow(uinbrahms)
nrow(ainbrahms)

#Finding trees on our removed trees list that appear as dead on brahms 
#Selecting everything brahms says is alive and subsetting 
Ubrahms26d <- Ubrahms[Ubrahms$LivingStatus != "A", c("PlantID","GardenGrid", "GardenSubgrid","BgLatitude" ,"BgLongitude","Taxon")]
Abrahms26d <- Abrahms[Abrahms$LivingStatus != "A",c("PlantID","GardenGrid", "GardenSubgrid","BgLatitude" ,"BgLongitude","Taxon")]
Qbrahms26d <- Qbrahms[Qbrahms$LivingStatus != "A",c("PlantID","GardenGrid", "GardenSubgrid","BgLatitude" ,"BgLongitude","Taxon")]


#Find trees on our removed treelist  that still appear in BRAHMS as dead
dqinbrahms <- semi_join(qrem, Qbrahms26d, by = "PlantID")
duinbrahms <- semi_join(urem, Ubrahms26d, by = "PlantID")
dainbrahms <- semi_join(arem, Abrahms26d, by = "PlantID")

# Print 
dqinbrahms
duinbrahms
dainbrahms


nrow(dqinbrahms)
nrow(duinbrahms)
nrow(dainbrahms)

#trees that don't appear in brhams at all but are on our removed trees list
qhuh <- anti_join(qrem, Qbrahms, by = "PlantID")
uhuh <- anti_join(urem, Ubrahms, by = "PlantID")
ahuh <- anti_join(arem, Abrahms, by = "PlantID")


#print
qhuh
uhuh
ahuh

nrow(qhuh)
nrow(uhuh)
nrow(ahuh)
