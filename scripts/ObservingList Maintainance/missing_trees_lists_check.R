library(ggplot2)
library(readr)
# set path
path.google <- "~/Google Drive/My Drive/LivingCollections_Phenology/"

#read infull list of 2025 trees trees 
q26<- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/Quercus/ObservingList_Quercus_2026.csv")
u26<- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/Ulmus/ObservingList_Ulmus_2026.csv")
a26<- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists//Acer/ObservingList_Acer_2026.csv")

#read infull list of 2023 trees trees 
q23 <- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/OLD/Quercus/ObservingList_Quercus_2023.csv")
u23 <- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/OLD/Ulmus/ObservingList_Ulmus_2023.csv")
a23 <- read_csv("~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/OLD/Acer/ObservingList_Acer_2023.csv")

# Trees in q23 NOT in q26
notq26<- anti_join(q23, q26, by = "PlantID")
notq26
nrow(notq26) #70 trees

# Checking for Trees in q26 NOT in q23- there should be none
notq23 <- anti_join(q26, q23, by = "PlantID")
notq23
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Trees in u23 NOT in u26
notu26<- anti_join(u23, u26, by = "PlantID")
notu26
nrow(notu26) # 9 trees

# Checking for Trees in u26 NOT in u23- there should be none
notu23 <- anti_join(u26, u23, by = "PlantID")
notu23
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Trees in a23 NOT in a26
nota26<- anti_join(a23, a26, by = "PlantID")
nota26
nrow(nota26)# 33 trees

#Checking for Trees in a26 NOT in a23- there should be none
nota23<- anti_join(a26,a23, by = "PlantID")
nota23


# collecting which trees are in BOL 
qbol <- readxl::read_xlsx(file.path(path.google, "Observing Lists", "2026-02-20_BRAHMSOnlineData_Quercus.xlsx"))
abol <- readxl::read_xlsx(file.path(path.google, "Observing Lists", "2026-02-20_BRAHMSOnlineData_Acer.xlsx"))
ubol <- readxl::read_xlsx(file.path(path.google, "Observing Lists", "2026-02-20_BRAHMSOnlineData_Ulmus.xlsx"))

#changing the "PlantNumber" Column titld to "PlantID" for comparisons
names(qbol)[names(qbol) == "PlantNumber"] <- "PlantID"
names(abol)[names(abol) == "PlantNumber"] <- "PlantID"
names(ubol)[names(ubol) == "PlantNumber"] <- "PlantID"

head(qbol)

#compairing list of trees not in the 25 list, that are not in bol
#quercus
notqb26<- anti_join(notq26,qbol, by ="PlantID")
notqb26
nrow(notqb26)#54 tree

#ulmus
notub26<- anti_join(notu26,ubol, by ="PlantID")
notub26
nrow(notub26) # 7 trees
#acer
notab26<- anti_join(nota26,abol, by ="PlantID")
notab26
nrow(notab26) # 22 trees

# Check for removed trees from our list
# Pull our existing removed trees list 
googlesheets4::gs4_auth(email="breidy@mortonarb.org")
removed <- googlesheets4::read_sheet("16xMa6MyJlh3zKkELrDToyoPk_GfoN1NSCVji_ttOCoQ", sheet="Removed Trees")
names(removed)

#seperate out by taxon 
qrem <- removed[grepl("Quercus", removed$Taxon, ignore.case = TRUE), ]
urem <- removed[grepl("Ulmus", removed$Taxon, ignore.case = TRUE), ]
arem <- removed[grepl("Acer", removed$Taxon, ignore.case = TRUE), ]

#change "PlantNumber" to "PlantID"
names(qrem)[names(qrem) == "PlantNumber"] <- "PlantID"
names(urem)[names(urem) == "PlantNumber"] <- "PlantID"
names(arem)[names(arem) == "PlantNumber"] <- "PlantID"

# Trees not in q26, not in BOL, and not in removed
notqbr26 <- anti_join(notqb26, qrem, by = "PlantID")
nrow(notqbr26)#52 trees

# Trees not in u26, not in BOL, and not in removed
notubr26 <- anti_join(notub26, urem, by = "PlantID")
nrow(notubr26)#4 trees

# Trees not in a26, not in BOL, and not in removed
notabr26 <- anti_join(notab26, arem, by = "PlantID")
nrow(notabr26)#16 trees

#binding 
not26<- rbind(notqbr26,notubr26,notabr26)

# binding
not26 <- rbind(notqbr26, notubr26, notabr26)
#
write_csv(not26, file = file.path(path.google, "Observing Lists", "trees_present_23_not_removed_brahms.csv"))

