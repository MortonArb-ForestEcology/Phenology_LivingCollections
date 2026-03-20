# Script to get some stats on our phenology observing to date
path.google <- "~/Google Drive/My Drive/"
path.pheno <- "~/Google Drive/My Drive/LivingCollections_Phenology/Data_Observations/"
dat.pheno <- data.frame()


files.acer.root <- dir(path.google, "LivingCollectionPhenology_ObservationData_Acer")
files.quercus.root <- dir(path.google, "LivingCollectionPhenology_ObservationData_Quercus")
files.tilia.root <- dir(path.google, "LivingCollectionPhenology_ObservationData_Tilia")
files.ulmus.root <- dir(path.google, "LivingCollectionPhenology_ObservationData_Ulmus")

for(FILE in c(files.acer.root, files.quercus.root, files.tilia.root, files.ulmus.root)){
  tmp <- read.csv(file.path(path.google, FILE))
  
  dat.pheno <- rbind(dat.pheno, tmp)
}
summary(dat.pheno)


files.acer.recent <- dir(path.pheno, "LivingCollectionPhenology_ObservationData_Acer")
files.quercus.recent <- dir(path.pheno, "LivingCollectionPhenology_ObservationData_Quercus")
files.tilia.recent <- dir(path.pheno, "LivingCollectionPhenology_ObservationData_Tilia")
files.ulmus.recent <- dir(path.pheno, "LivingCollectionPhenology_ObservationData_Ulmus")

for(FILE in c(files.acer.recent, files.quercus.recent, files.tilia.recent, files.ulmus.recent)){
  tmp <- read.csv(file.path(path.pheno, FILE))
  
  dat.pheno <- rbind(dat.pheno, tmp)
}

summary(dat.pheno)
dat.pheno$Species <- gsub("  ", " ", dat.pheno$Species)
dat.pheno$Species <- trimws(dat.pheno$Species, which="both")

length(unique(dat.pheno$Observer))
length(unique(dat.pheno$PlantNumber))
length(unique(dat.pheno$Species))
summary(as.factor(dat.pheno$Species))

observer.agg <- aggregate(dat.pheno$Observer, by=list(dat.pheno$Observer), FUN=length)

species.agg <- aggregate(dat.pheno$Species, by=list(dat.pheno$Species), FUN=length)
nrow(species.agg[species.agg$x>10,])
