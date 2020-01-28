# Getting some quick stats on monitoring goals
path.google <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"

list.quercus <- read.csv(file.path(path.google, "Observing Lists", "Quercus/ObservingLists_Quercus.csv"))
list.acer <- read.csv(file.path(path.google, "Observing Lists", "Acer/ObservingLists_Acer.csv"))
list.ulmus <- read.csv(file.path(path.google, "Observing Lists", "Ulmus/ObservingLists_Ulmus.csv"))

list.stats <- data.frame(Collection=c("Quercus", "Acer", "Ulmus"),
                         Start.Year=c(2018, 2019, 2020),
                         n.trees = c(nrow(list.quercus), nrow(list.acer), nrow(list.ulmus)),
                         n.Taxa = c(length(unique(list.quercus$Taxon)), length(unique(list.acer$Taxon)), length(unique(list.ulmus$Taxon))),
                         n.lists = c(length(unique(list.quercus$Obs.List)), length(unique(list.acer$Obs.List)), length(unique(list.ulmus$Obs.List))))

list.stats
sum(list.stats$n.trees)
sum(list.stats$n.Taxa)
sum(list.stats$n.lists)

