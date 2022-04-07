# Getting some quick stats on monitoring goals
path.google <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"

list.quercus <- read.csv(file.path(path.google, "Observing Lists", "Quercus/ObservingList_Quercus.csv"))
list.acer <- read.csv(file.path(path.google, "Observing Lists", "Acer/ObservingList_Acer.csv"))
list.ulmus <- read.csv(file.path(path.google, "Observing Lists", "Ulmus/ObservingList_Ulmus.csv"))
list.tilia <- read.csv(file.path(path.google, "Observing Lists", "Tilia/ObservingList_Tilia.csv"))

list.stats <- data.frame(Collection=c("Quercus", "Acer", "Ulmus", "Tilia"),
                         Start.Year=c(2018, 2019, 2020, 2022),
                         n.trees = c(nrow(list.quercus), nrow(list.acer), nrow(list.ulmus), nrow(list.tilia)),
                         n.Taxa = c(length(unique(list.quercus$Taxon)), length(unique(list.acer$Taxon)), length(unique(list.ulmus$Taxon)), length(unique(list.tilia$Taxon))),
                         n.lists = c(length(unique(list.quercus$Obs.List)), length(unique(list.acer$Obs.List)), length(unique(list.ulmus$Obs.List)), length(unique(list.tilia$Obs.List))))

list.stats
sum(list.stats$n.trees)
sum(list.stats$n.Taxa)
sum(list.stats$n.lists)

