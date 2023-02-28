
# Setting a script 
# dataRaw=treesCollections[treesCollections$GardenLocalityName=="Ulmus",]
subsetTrees <- function(dataRaw, SPP, nTreeSpp=9, stratVar=T, nTreeVar=4, dataOut=NULL, seed=1153){
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
          set.seed(seed)
          dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[sample(obsPast, nTreeVar, replace = F)],])
        } else {
          # If we don't have an excess, but could add a few, randomly pick
          # Go ahead and add what we want to keep
          dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[obsPast],])
          
          # Find some new trees to add as well
          set.seed(seed)
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
      set.seed(seed)
      dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[sample(obsPast, nTreeSpp, replace = F)],])
    } else {
      # If we don't have an excess, but could add a few, randomly pick
      # Go ahead and add what we want to keep
      dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[obsPast],])
      
      # Find some new trees to add as well
      set.seed(seed)
      obsNew <- sample(obsNew, nTreeSpp-length(obsPast), replace=F)
      dataOut <- rbind(dataOut, dataRaw[dataRaw$PlantId %in% datSpp$PlantId[obsNew],])
    } # End if/else clause for whether you need to add new trees or not
  } # End case of dealing with many variety versus not
  
  return(dataOut)
} # End function


# datIn <- ulmus2023
clusterTreesOLD <- function(datIn, clusterMin=20, clusterMax=30, nTry=10, seed=sample.int(1e6, 1)){
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


clusterTreesNew <- function(datIn, clusterMin=15, clusterMax=25, seed=sample.int(1e6, 1)){
  # datIn <- datIn # Add a dub here just to be safe for now
  datIn$List <- NA
  
  # Doing 5 times more clusters than we actually want
  set.seed(seed)
  listKmeans <- kmeans(datIn[,c("BgLongitude", "BgLatitude")], centers=round(nrow(datIn)/mean(c(clusterMin, clusterMax)))) # 
  summary(as.factor(listKmeans$cluster))
  datIn$List <- listKmeans$cluster
  nList <- max(listKmeans$cluster)
  
  # Next step is doing clustering trying to balance number + distance
  # Note: the first n rows will be our centers; the rest will be trees
  distPts2Grps <- as.matrix(dist(rbind(listKmeans$centers, datIn[,c("BgLongitude", "BgLatitude")]), method="euclidean"))
  rownames(distPts2Grps) <- c(paste0("GRP", 1:nList), 1:nrow(datIn))
  colnames(distPts2Grps) <- c(paste0("GRP", 1:nList), 1:nrow(datIn))
  summary(distPts2Grps)
  
  # IFF we need to rebalance:
  # if(any(listKmeans$size>clusterMax | listKmeans$size<clusterMin)){
    # Stratify our groups into "givers" (too many trees) and "takers" (need some trees to balance):
    # 1. Start with the takers since this is where the cascade happens: take the closest tree from another group
    # 2. If that group now needs a tree, take the nearest tree from another group (that isn't the first one)
    
    # Set up our starters
    listSize <- summary(as.factor(datIn$List))
    takersOrig <- which(listSize<clusterMin) 
    
    takersNew <- takersOrig
    takersTot <- takersOrig
    while(length(takersNew)>0){ # As long as there are still groups needing a tree, keep going
      print("Rebalancing Lists: Takers Group")
      for(i in 1:length(takersNew)){
        # print(as.numeric(takersNew[i]))
        while(listSize[takersNew[i]]<clusterMin){
          rows.avail <- which(!datIn$List %in% c(takersNew[1:max(i-1,i)], takersTot[1:(length(takersTot)-length(takersNew))]))
          dist.avail <- distPts2Grps[paste(rows.avail),paste0("GRP", takersNew[i])]
          datIn$List[as.numeric(names(dist.avail[which(dist.avail==min(dist.avail))]))] <- takersNew[i]
          
          listSize <- summary(as.factor(datIn$List))
        } # Finish while loop
      } # Finish takersNew Loop
      
      takersNew <- which(listSize<clusterMin) 
      takersTot <- c(takersTot, takersNew)
      
    } # Finish working with the "takers" group
    # listSize

    # 3. If we still have trees groups with too many trees, give on to the nearest group
    # 4. If this results in the receiving group have too many trees, give it to the nearest etc.
    listSize <- summary(as.factor(datIn$List))
    giversOrig <- which(listSize>clusterMax) 
    
    giversNew <- giversOrig
    giversTot <- giversOrig
    while(length(giversNew)>0){ # As long as there are still groups needing a tree, keep going
      print("Rebalancing Lists: Givers Group")
      
      for(i in 1:length(giversNew)){
        print(as.numeric(giversNew[i]))
        while(listSize[giversNew[i]]>clusterMax){
          ### NOTE: ALTERNATE LOGIC AVAILABLE
          ### Find the closet group to your group that could accept trees & give it the closest trees that won't make it overload
          
          # I want to find the tree furthest from the center and give it to its closest cluster
          rows.avail <- which(datIn$List %in% giversNew[i])
          dist.avail <- distPts2Grps[paste(rows.avail),paste0("GRP", giversNew[i])]
          tree.give <- names(dist.avail[which(dist.avail==max(dist.avail))])
          grpsout <- paste0("GRP",c(giversNew[1:max(i-1,i)], giversTot[1:(length(giversTot)-length(giversNew))]))
          groupDist <- distPts2Grps[tree.give, colnames(distPts2Grps)[grepl("GRP", colnames(distPts2Grps)) & !colnames(distPts2Grps) %in% grpsout]]
          groupNew <- names(groupDist)[which(groupDist==min(groupDist))]
          datIn$List[as.numeric(tree.give)] <- as.numeric(gsub("GRP", "", groupNew))
          
          listSize <- summary(as.factor(datIn$List))
        } # Finish while loop
      } # Finish giversNew Loop
      
      giversNew <- which(listSize>clusterMax) 
      giversTot <- c(giversTot, giversNew)
      
    } # Finish with the "givers" group
    listSize
  # } # end the rebalance bit; may no longer be necessary to have this
  

  
  return(datIn)
  
} # End clustering function
