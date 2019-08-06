clean.google <- function(pheno.title = "Phenology_Observations_GoogleForm", collection="Quercus", dat.yr=lubridate::year(Sys.Date())){
  pheno.lc <- gs_title(pheno.title)
  pheno.lc
  
  # get the data from a particular sheet
  dat.raw <- data.frame(gs_read(pheno.lc, ws=paste(collection, "Raw Observations", dat.yr, sep=" ")))

  # Renaming some columns
  names(dat.raw)[grep("OPTIONAL", names(dat.raw))] <- "Notes"
  names(dat.raw)[grep("species", names(dat.raw))] <- "Species"
  
  # Recoding for easier names -- there must be a better way to do this, but I can't figure it out right now
  names(dat.raw) <- car::recode(names(dat.raw),
                                  "'Breaking.leaf.buds..Observed.'='leaf.buds.observed';
                                  'Leaf.breaking.bud.intensity..Intensity.'='leaf.buds.intensity';
                                  'Leaf.observed..Observed.'='leaf.present.observed';
                                  'Leaf.intensity..Intensity.'='leaf.present.intensity';
                                  'Leaf.increasing.in.size..Observed.'='leaf.increasing.observed';
                                  'Leaf.increasing.in.size.intensity..Intensity.'='leaf.increasing.intensity';
                                  'Leaf.color.observed..Observed.'='leaf.color.observed';
                                  'Leaf.color.intensity..Intensity.'='leaf.color.intensity';
                                  'Leaf.falling.observed..Observed.'='leaf.falling.observed';
                                  'Flower.buds.observed..Observed.'='flower.buds.observed';
                                  'Flower.buds.intensity..Intensity.'='flower.buds.intensity';
                                  'Flower.open.observed..Observed.'='flower.open.observed';
                                  'Flower.open.intensity..Intensity.'='flower.open.intensity';
                                  'Flower.pollen.release.observed..Observed.'='flower.pollen.observed';
                                  'Flower.pollen.release.intensity..Intensity.'='flower.pollen.intensity';
                                  'Fruit.observed..Observed.'='fruit.present.observed';
                                  'Fruit.intensity..Intensity.'='fruit.present.intensity';
                                  'Fruit.ripe.observed..Observed.'='fruit.ripe.observed';
                                  'Fruit.ripe.intensity..Intensity.'='fruit.ripe.intensity';
                                  'Fruit.drop.observed..Observed.'='fruit.drop.observed';
                                  'Fruit.drop.intensity..Intensity.'='fruit.drop.intensity'")

  # Coming up with handy groups for our columns
  cols.meta <- c("Timestamp", "Observer", "Date.Observed", "Species", "PlantNumber", "Notes")
  pheno.leaf <- names(dat.raw)[grep("leaf", tolower(names(dat.raw)))]
  pheno.flower <- names(dat.raw)[grep("flower", tolower(names(dat.raw)))]
  pheno.fruit <- names(dat.raw)[grep("fruit", tolower(names(dat.raw)))]
  
  # Setting things to factors
  for(i in 1:ncol(dat.raw)){
    if(class(dat.raw[,i])=="character") dat.raw[,i] <- as.factor(dat.raw[,i])
  }
  summary(dat.raw)
  cols.id <- grep("accession", names(dat.raw))
  
  dat.clean <- dat.raw[,c(cols.meta[!cols.meta=="PlantNumber"], pheno.leaf, pheno.flower, pheno.fruit)]
  dat.clean$PlantNumber <- as.factor(apply(dat.raw[,cols.id], 1, FUN=function(x) {x[which(!is.na(x))][1]})) # Get the PlantNumber
  dat.clean$Timestamp <- strptime(dat.clean$Timestamp, format="%m/%d/%Y %H:%M:%S")
  dat.clean$Date.Observed <- as.Date(dat.clean$Date.Observed, format="%m/%d/%Y")

  dat.clean <- dat.clean[,c(cols.meta, pheno.leaf, pheno.flower, pheno.fruit)] # Just re-organizing to how I like to see things
  summary(dat.clean)
  
  # Get rid of observations that have TEST in them or are before our last phenology training
  rows.remove <- c(which(is.na(dat.clean$Species)), grep("TEST", toupper(dat.clean$NOTES)), grep("TEST", toupper(dat.clean$Observer)) )
  if(length(rows.remove)>0) dat.clean <- dat.clean[!(1:nrow(dat.clean) %in% rows.remove),] # 
  
  dat.clean <- droplevels(dat.clean) # Get rid of unused levels
  summary(dat.clean)
  
  
  return(dat.clean)
}