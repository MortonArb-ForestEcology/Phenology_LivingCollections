# Getting and putting flattened CSVs of past years' data on our Google Dive for our sanity!
source("clean_google_form.R")


# 2018 -- Quercus only!
quercus18 <- clean.google(collection="Quercus", dat.yr=2018)
quercus18$Collection <- as.factor("Quercus")
quercus18$Year <- lubridate::year(quercus18$Date.Observed)
summary(quercus18)

write.csv(quercus18, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Quercus_2018_FINAL.csv")), row.names=F)

# write.csv(quercus18, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_All_2018_FINAL.csv")), row.names=F)



# 2019 -- Quercus + Acer ---- 
quercus19 <- clean.google(collection="Quercus", dat.yr=2019)
quercus19$Collection <- as.factor("Quercus")
quercus19$Year <- lubridate::year(quercus19$Date.Observed)
summary(quercus19)

write.csv(quercus19, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Quercus_2019_FINAL.csv")), row.names=F)



acer19 <- clean.google(collection="Acer", dat.yr=2019)
acer19$Collection <- as.factor("Acer")
acer19$Year <- lubridate::year(acer19$Date.Observed)
summary(acer19)

write.csv(acer19, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Acer_2019_FINAL.csv")), row.names=F)


write.csv(rbind(quercus19, acer19), file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_All_2019_FINAL.csv")), row.names=F)



# 2020 -- Quercus, Acer, Ulmus ----
quercus20 <- clean.google(collection="Quercus", dat.yr=2020)
quercus20$Collection <- as.factor("Quercus")
quercus20$Year <- lubridate::year(quercus20$Date.Observed)
summary(quercus20)

write.csv(quercus20, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Quercus_2020_FINAL.csv")), row.names=F)


acer20 <- clean.google(collection="Acer", dat.yr=2020)
acer20$Collection <- as.factor("Acer")
acer20$Year <- lubridate::year(acer20$Date.Observed)
summary(acer20)
write.csv(acer20, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Acer_2020_FINAL.csv")), row.names=F)


ulmus20 <- clean.google(collection="Ulmus", dat.yr=2020)
ulmus20$Collection <- as.factor("Ulmus")
ulmus20$Year <- lubridate::year(ulmus20$Date.Observed)
summary(ulmus20)

write.csv(rbind(quercus20, acer20, ulmus20), file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_All_2020_FINAL.csv")), row.names=F)




# 2021 -- Quercus, Acer, Ulmus ----
quercus21 <- clean.google(collection="Quercus", dat.yr=2021)
quercus21$Collection <- as.factor("Quercus")
quercus21$Year <- lubridate::year(quercus21$Date.Observed)
summary(quercus21)

write.csv(quercus21, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Quercus_2021_FINAL.csv")), row.names=F)


acer21 <- clean.google(collection="Acer", dat.yr=2021)
acer21$Collection <- as.factor("Acer")
acer21$Year <- lubridate::year(acer21$Date.Observed)
summary(acer21)

write.csv(acer21, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Acer_2021_FINAL.csv")), row.names=F)


ulmus21 <- clean.google(collection="Ulmus", dat.yr=2021)
ulmus21$Collection <- as.factor("Ulmus")
ulmus21$Year <- lubridate::year(ulmus21$Date.Observed)
summary(ulmus21)

write.csv(ulmus21, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Ulmus_2021_FINAL.csv")), row.names=F)



write.csv(rbind(quercus21, acer21, ulmus21), file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_All_2021_FINAL.csv")), row.names=F)



# 2022 -- Quercus, Acer, Ulmus, Tilia, Q. macrocarpa ---- 
quercus22 <- clean.google(collection="Quercus", dat.yr=2022)
quercus22$Collection <- as.factor("Quercus")
quercus22$Year <- lubridate::year(quercus22$Date.Observed)
summary(quercus22)
write.csv(quercus22, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Quercus_2022_FINAL.csv")), row.names=F)


acer22 <- clean.google(collection="Acer", dat.yr=2022)
acer22$Collection <- as.factor("Acer")
acer22$Year <- lubridate::year(acer22$Date.Observed)
summary(acer22)
write.csv(acer22, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Acer_2022_FINAL.csv")), row.names=F)


ulmus22 <- clean.google(collection="Ulmus", dat.yr=2022)
ulmus22$Collection <- as.factor("Ulmus")
ulmus22$Year <- lubridate::year(ulmus22$Date.Observed)
summary(ulmus22)

write.csv(ulmus22, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Ulmus_2022_FINAL.csv")), row.names=F)


tilia22 <- clean.google(collection="Tilia", dat.yr=2022)
tilia22$Collection <- as.factor("Tilia")
tilia22$Year <- lubridate::year(tilia22$Date.Observed)
summary(tilia22)
write.csv(tilia22, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Tilia_2022_FINAL.csv")), row.names=F)


quma22 <- clean.google(collection="Q.macrocarpa", dat.yr=2022)
quma22$Collection <- as.factor("Q. macrocarpa")
quma22$Year <- lubridate::year(quma22$Date.Observed)
summary(quma22)

write.csv(quma22, file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_Quercus-macrocarpa_2022_FINAL.csv")), row.names=F)


write.csv(rbind(quercus22, acer22, ulmus22, tilia22, quma22), file.path("~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations", paste0("LivingCollectionPhenology_ObservationData_All_2022_FINAL.csv")), row.names=F)
