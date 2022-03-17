library(rgdal); library(dplyr); library(factoextra); # spatial analysis packages
library(ggplot2); library(grid);library(gridExtra); library(FactoMineR); library(factoextra);library(cluster) # graphing packages


dir.base <- "/Volumes/GoogleDrive/My Drive/LivingCollections_Phenology/"
#setwd(dir.base)

path.dat <- file.path(dir.base, "Observing Lists")
path.out <- "~/Google Drive/My Drive/LivingCollections_Phenology/Observing Lists/Tilia/"
dir.create(path.dat, recursive = T, showWarnings = F)

# Species in the NPN database
npn <- c("americana", "cordata", "tomentosa")
length(npn)
spp.keep <- c("amurensis", "dasystyla", "euchlora", "europaea", "flavescens", "japonica", "mandshurica","mongolica", "oliveri", "orbicularis","paucicostata","platyphyllos", "zamoyskiana")


# ----------------------------
# Narrowing down the phenology observering lists
# ----------------------------
tilia <- read.csv("../data/ObservingLists/Tilia_v_1.csv")

head(tilia)

# Removing for double entries

for(PLANT in unique(tilia$PlantNumber)){
  tilia <- tilia[c(which(tilia$PlantNumber!=PLANT), which(tilia$PlantNumber==PLANT)[1]), ]
}
summary(tilia)

unique(tilia$Taxon)
length(unique(tilia$Taxon)) # 29 unique entries

# Looping through to add genus and species to the tilia form
taxa.split <- strsplit(paste(tilia$Taxon), split=" ")
for(i in 1:nrow(tilia)){
  tilia[i,"genus"] <- taxa.split[[i]][1]
  tilia[i,"species"] <- taxa.split[[i]][2]
}
summary(as.factor(tilia$species))
dim(tilia)

# Checking for how many species overlap with NPN
length(which(unique(tilia$species) %in% npn))

# Getting rid of trees that are may be difficult to ovserve  

trees.exclude1 <- tilia[tilia$BgLongitude<=-88.068438001,] 

trees.exclude2 <- tilia[tilia$BgLatitude>=41.818238,]

trees.exclude <- rbind(trees.exclude1,trees.exclude2)

#Creating a new list composed of all trees except those listed in tree.exclude
tilia <- anti_join(tilia, trees.exclude)

# Go through and add in the closest group so that groups are between 20 & 30 trees
max.trees <- 17
min.trees <-12 
n.groups <- round(nrow(tilia)/mean(max.trees + min.trees))
n.groups

# ---------
# Option 1: Group groups based on distance
# ---------
# k-means clustering
nrow(tilia)/17
set.seed(1234)

# Compute a euclidean distance matrix
tilia.dist <- dist(tilia[,c("BgLongitude", "BgLatitude")])
summary(tilia.dist)

#Visualizing distance
fviz_dist(tilia.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#getting the kmean for 4 centers with 50 starts
tilia.kmeans4 <- kmeans(tilia[,c("BgLongitude", "BgLatitude")], centers=4, nstart=50)

#viewing the structure of the kmeans
#str(tilia.kmeans)

fviz_cluster(tilia.kmeans4, data = tilia.dist)

fviz_cluster(tilia.kmeans4, data =tilia.dist, choose.vars = NULL, stand = TRUE,
             axes = c(1, 2), geom = c("text"), repel = FALSE,
             show.clust.cent =FALSE, ellipse = FALSE, ellipse.type = "convex",
             ellipse.level = 0.95, ellipse.alpha = 0.2, shape = NULL,
             pointsize = 1.5, labelsize = 12, main = "Cluster plot",)

#compairing different clusters with differnt center sizes
tilia.kmeans3 <- kmeans(tilia[,c("BgLongitude", "BgLatitude")], centers=3, nstart=50)
tilia.kmeans5 <- kmeans(tilia[,c("BgLongitude", "BgLatitude")], centers=5, nstart=50)
tilia.kmeans6 <- kmeans(tilia[,c("BgLongitude", "BgLatitude")], centers=6, nstart=50)

# plots to compare
p1 <- fviz_cluster(tilia.kmeans3, geom = "point", data = tilia.dist) + ggtitle("k = 3")
p2 <- fviz_cluster(tilia.kmeans4, geom = "point",  data = tilia.dist) + ggtitle("k = 4")
p3 <- fviz_cluster(tilia.kmeans5, geom = "point",  data = tilia.dist) + ggtitle("k = 4")
p4 <- fviz_cluster(tilia.kmeans6, geom = "point",  data = tilia.dist) + ggtitle("k = 6")

grid.arrange(p1, p2, p3, p4, nrow = 2)
# after this we are picking a cluster with k= 3

tilia.kmeans3$cluster
#### adding an obs.list row, placing it as the first row, and then organizing our list by the obs.list row
tilia$Obs.List <- tilia.kmeans3$cluster
tilia <- tilia %>% relocate(Obs.List, .before = PlantNumber)
tilia <- tilia[order(tilia$Obs.List),]
tilia <- subset(tilia,select=-c(5,10,11))
#View(tilia)

#Writing a csv of the total  list

write.csv(tilia, paste0(path.out, "ObservingList_Tilia.csv"), row.names = F)

# Splitting the tilia dataframe by the the "Obs.List" column, splits the data into
# the updated observations lists, 
split_new.dat <- split(tilia, list(tilia$Obs.List))

# Loop to write out new .csv files based upon the splits created in the split_new.dat 

for (Obs.List in names(split_new.dat)) {
  write.csv(split_new.dat[[Obs.List]], paste0(path.out,"ObservingList_tilia_",Obs.List,".csv"), row.names = F,)}




###Looking at the best possible cluster within sum of squares in our groups

wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(tilia.dist)-1)*sum(sapply(tilia.dist,var))
  for (i in 2:nc){
    set.seed(123)
    wss[i] <- sum(kmeans(tilia.dist, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(tilia.dist, nc = 15)

#This shows that 4 is the ideal # of clusters for this group of trees I believe
#Doing more clustering analysis to be sure

kmeans.tilia <- kmeans(scale(tilia.dist), 4, nstart = 100)
kmeans.tilia

# plot the clusters
fviz_cluster(kmeans.tilia, data = scale(tilia.dist), geom = c("point"),ellipse.type = "euclid")

#Hierachical dendrogram
Hierar_cl <- hclust(tilia.dist, method = "average")
#checking # objects in call
Hierar_cl

# Plotting dendrogram
plot(Hierar_cl)
abline(h = 110, col = "green")
fit <- cutree(Hierar_cl, k = 4 )
table(fit)
rect.hclust(Hierar_cl, k = 4, border = "green")

#so perhaps 4 isn't my ideal# of clusters? there is a very small group 
#in the second branch Looking at possibly 3
kmeans.tilia <- kmeans(scale(tilia.dist), 3, nstart = 100)
kmeans.tilia

# plot the clusters
fviz_cluster(kmeans.tilia, data = scale(tilia.dist), geom = c("point"),ellipse.type = "euclid")
# validating with gap statistics, wss, and silhouette

fviz_nbclust(scale(tilia.dist), kmeans, nstart=50, method = "gap_stat")

fviz_nbclust(scale(tilia.dist), kmeans, nstart=50, method = "wss")

fviz_nbclust(scale(tilia.dist), kmeans, nstart=50, method = "silhouette")

tilisil<-silhouette(kmeans.tilia$cluster,dist(tilia.dist))
fviz_silhouette(tilisil)

#This doesn't also look like the perfect way for clusters to be arranged for
#an average, however, since this is real world observation I think 3 would work
#checking 3 in heirarchical dedrogram
Hierar_cl <- hclust(tilia.dist, method = "average")
#checking # objects in call
Hierar_cl

# Plotting dendrogram
plot(Hierar_cl)
abline(h = 110, col = "green")
fit <- cutree(Hierar_cl, k = 3 )
table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")
ellipse.type = "euclid")

#doing this according centroid
Hierar_cl <- hclust(tilia.dist, method = "centroid")
#checking # objects in call
Hierar_cl

# Plotting dendrogram
plot(Hierar_cl)
abline(h = 110, col = "green")
fit <- cutree(Hierar_cl, k = 3 )
table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")

######