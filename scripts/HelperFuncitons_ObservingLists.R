
# Setting a script 
# dataRaw=treesCollections[treesCollections$GardenLocalityName=="Ulmus",]
subsetTrees <- function(dataRaw, SPP, nTreeSpp=9, stratVar=T, nTreeVar=4, dataOut=NULL){
  # dataRaw = the input data from Brahms; don't mess with the header names when you get it from Brahms!
  # SPP = the species you're working with from column "SpeciesName"
  # nTreeSpp = the max number of trees from a species with no varieties you want
  # stratVar = T/F do you want to consider varieties in your selection
  # nTreeVar = the max number of trees from any one variety 
  
  # If we don't have an existing dataset, create it
  if(is.null(dataOut)) dataOut <- data.frame()
  # SPP=ulmusSppBIG[1]
  datSpp <- dataRaw[dataRaw$SpeciesName==SPP,c("PlantId", "GenusName", "SpeciesName", "CalcFullName", "PastObserving")]
  
  varList <- unique(datSpp$CalcFullName) 
  # Check how many trees we have
  nNow <- nrow(datSpp)
  
  if(length(varList) > 1 & stratVar){
    for(VAR in varList){
      treeVar <- which(datSpp$CalcFullName==VAR)
      nTree <- length(treeVar)
      
      if(length(treeVar)<=nTreeVar){
        dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[treeVar],])
      } else {
        obsPast <- which(datSpp$CalcFullName==VAR & datSpp$PastObserving) 
        if(length(obsPast)>nTreeVar){
          # If we have an excess, randomly subset from the past
          set.seed(1153)
          dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[sample(obsPast, nTreeVar, replace = F)],])
        } else {
          # If we don't have an excess, but could add a few, randomly pick
          # Go ahead and add what we want to keep
          dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[obsPast],])
          
          # Find some new trees to add as well
          set.seed(1153)
          obsNew <- sample(which(datSpp$CalcFullName==VAR & !datSpp$PastObserving), nTreeVar-length(obsPast), replace=F)
          dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[obsNew],])
        } # End if/else clause for whether you need to add new trees or not
      } # End allocating varieties
    } # End looping through the different varieties
  } else { # there aren't any varieties
    if(!stratVar){
      obsPast <- which(datSpp$SpeciesName==SPP & datSpp$PastObserving)
      obsNew <- which(datSpp$SpeciesName==SPP & !datSpp$PastObserving)
      
    } else {
      VAR <- varList # Just to make the coding easier!
      obsPast <- which(datSpp$CalcFullName==VAR & datSpp$PastObserving)
      obsNew <- which(datSpp$CalcFullName==VAR & !datSpp$PastObserving)
      
    }
    
    if(length(obsPast)>nTreeSpp){
      # If we have an excess, randomly subset from the past
      set.seed(1153)
      dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[sample(obsPast, nTreeSpp, replace = F)],])
    } else {
      # If we don't have an excess, but could add a few, randomly pick
      # Go ahead and add what we want to keep
      dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[obsPast],])
      
      # Find some new trees to add as well
      set.seed(1153)
      obsNew <- sample(obsNew, nTreeSpp-length(obsPast), replace=F)
      dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[obsNew],])
    } # End if/else clause for whether you need to add new trees or not
  } # End case of dealing with many variety versus not
  
  return(dataOut)
} # End function


# datIn <- ulmus2023
clusterTrees <- function(datIn, clusterMin=20, clusterMax=30, nTry=10, seed=sample.int(1e6, 1)){
  # datIn <- datIn # Add a dub here just to be safe for now
  datIn$List <- NA

  # Doing 5 times more clusters than we actually want
  set.seed(seed)
  listKmeans <- kmeans(datIn[,c("BgLongitude", "BgLatitude")], centers=round(nrow(datIn)/clusterMax)*nTry) # 
  summary(as.factor(listKmeans$cluster))
  datIn$kClust <- listKmeans$cluster
  
  # Next step is doing clustering trying to balance number + distance
  dist.kmeans <- as.matrix(dist(listKmeans$centers))
  summary(dist.kmeans)
  
  # Finding a way to pair clusters that balances number and distance
  listGroups <- data.frame(clust1 = as.factor(1:nrow(listKmeans$centers)),
                           listKmeans$centers)
  
  for(i in 1:nrow(listGroups)){
    listGroups[i, "n.clust1"] <- length(which(listKmeans$cluster==i))
  }
  summary(listGroups)
  dim(listGroups)
  
  # Sort our groupings from most to least
  listGroups <- listGroups[order(listGroups$n.clust1, listGroups$BgLongitude, listGroups$BgLatitude, decreasing=T),]
  head(listGroups)

  clust.new = 1
  listGroups[1, "clust2"] <- clust.new
  
  pb <- txtProgressBar(min=0, max=nrow(listGroups), style=3)
  for(i in 1:nrow(listGroups)){
    set.seed(seed)
    
    setTxtProgressBar(pb, i)
    if(i > 1 & !is.na(listGroups[i, "clust2"])) next # Skip over already assigned groups
    
    # find how many are in this group
    group1 <- listGroups[i, "clust1"]
    listGroups[i,"clust2"] <- clust.new
    
    n.group <- sum(listGroups$n.clust1[which(listGroups$clust2==clust.new)])
    
    # Find the unassigned rows
    groups.open <- listGroups[is.na(listGroups$clust2), "clust1"]
    
    # When we have less than our minimum numebr of trees in the group, add the next closest group
    while(n.group<clusterMin & length(groups.open)>0){
      groups.open <- listGroups[is.na(listGroups$clust2), "clust1"] # Update this in our loop
      
      # Find the closest trees(s); if open groups check, if not stick it with whatever's closest no matter what
      if(length(groups.open)>0){
        group.closest <- groups.open[which(dist.kmeans[groups.open,group1]==min(dist.kmeans[groups.open,group1], na.rm=T))]
      } else {
        group.closest <- groups.open[which(dist.kmeans[,group1]==min(dist.kmeans[,group1], na.rm=T))]
      }
      
      # If: 
      #  1. adding all groups of closest trees keeps us under 15, add them all
      # #  2a. if adding the group with the most trees keeps us under 15, add that one
      #  2b. add the first one in the list
      if(sum(n.group, listGroups[listGroups$clust1 %in% group.closest, "n.clust1"]) <= clusterMax) {
        listGroups[listGroups$clust1 %in% group.closest, "clust2"] <- clust.new
      } else {
        listGroups[listGroups$clust1 %in% group.closest[1], "clust2"] <- clust.new
      }
      n.group <- sum(listGroups$n.clust1[which(listGroups$clust2==clust.new)])
    } # end while
    
    # If we have a less than full group, add the component groups to the closest group
    if(n.group < clusterMin*0.75 & length(groups.open)==0){
      # Find the groups we're lumping
      clust.redo <- listGroups[listGroups$clust2==clust.new, "clust1"]
      
      for(j in clust.redo){
        groups.open <- listGroups[listGroups$clust1!=j & listGroups$clust2!=clust.new, "clust1"]
        # if this is our last group, add it to the closest cluster
        group.closest <- groups.open[which(dist.kmeans[groups.open, j]==min(dist.kmeans[groups.open,j], na.rm=T))]
        listGroups[listGroups$clust1==j, "clust2"] <- listGroups[listGroups$clust1==group.closest, "clust2"]
      }
      
      
    }
    
    clust.new=clust.new+1
  } # End loop
  listGroups$clust2 <- as.factor(listGroups$clust2)
  head(listGroups)  
  
  
  listGroup2 <- aggregate(listGroups$n.clust1, by=list(listGroups$clust2), sum)
  names(listGroup2) <- c("group1", "n")
  summary(listGroup2)
  listGroup2
  # clust.kmeans <- hclust(oak.dist)
  # plot(clust.kmeans)
  
  # Assigning groups from our re-clustering
  for(GRP in unique(datIn$kClust)){
    datIn$List[datIn$kClust==GRP] <- listGroups$clust2[listGroups$clust1==GRP]
  } # End group renumbering  
  
  return(datIn[,names(datIn)[names(datIn)!="kClust"]])
  
} # End clustering function

