# Ian Hedges
# NBA Clustering Personal Project


#### Data Loading ####
getwd()
setwd("C:/Users/ihedg/Documents/Personal Analytics Projects/R and Excel Files for NBA Project")
d <- read.csv("database_24_25.csv")

#### Data Summary ####
summary(d)

#### Setting Up Objects for Data Preparation and Iteration ####
playerList <- as.list(unique(d$Player))

clusterStats <- as.list(c("MP", "FGA", "FG.", "X3PA", "X3P.", "FTA", "FT.", "ORB", "DRB",
                  "AST", "STL", "BLK", "TOV", "PF", "PTS"))

playerSeasonStats <- data.frame(c("player", clusterStats))
colnames(playerSeasonStats) <- c("player", clusterStats)


#### Creating Records for Player Season Averages via Iteration ####
for(player in playerList) {
  playerSubset <- subset(d, d$Player == player)
  gamesPlayed <- nrow(playerSubset)
  statsAvg <- as.vector(c(player))
  for(stat in clusterStats){
    statsAvg <- append(statsAvg, mean(playerSubset[,stat]))
  }
  playerSeasonStats <- rbind(playerSeasonStats, statsAvg)
}

#### Fixing Data Frame ####

# Removing the placeholder row
playerSeasonStats <- playerSeasonStats[2:nrow(playerSeasonStats),]

# setting all numeric features as numeric columns
playerSeasonStats[,2] <- as.numeric(playerSeasonStats[,2])
playerSeasonStats[,3] <- as.numeric(playerSeasonStats[,3])
playerSeasonStats[,4] <- as.numeric(playerSeasonStats[,4])
playerSeasonStats[,5] <- as.numeric(playerSeasonStats[,5])
playerSeasonStats[,6] <- as.numeric(playerSeasonStats[,6])
playerSeasonStats[,7] <- as.numeric(playerSeasonStats[,7])
playerSeasonStats[,8] <- as.numeric(playerSeasonStats[,8])
playerSeasonStats[,9] <- as.numeric(playerSeasonStats[,9])
playerSeasonStats[,10] <- as.numeric(playerSeasonStats[,10])
playerSeasonStats[,11] <- as.numeric(playerSeasonStats[,11])
playerSeasonStats[,12] <- as.numeric(playerSeasonStats[,12])
playerSeasonStats[,13] <- as.numeric(playerSeasonStats[,13])
playerSeasonStats[,14] <- as.numeric(playerSeasonStats[,14])
playerSeasonStats[,15] <- as.numeric(playerSeasonStats[,15])
playerSeasonStats[,16] <- as.numeric(playerSeasonStats[,16])

# Check column types
summary(playerSeasonStats)


#### Clustering ####
library(cluster)

par(mfrow=c(1,3))
# single-linkage clustering
agn <- agnes(playerSeasonStats, diss=F, stand=F, metric="euclidean", method="single")

dend_agn <- as.dendrogram(agn)
plot(dend_agn, xlab="Index of data points", ylab="Steps"
     , main="Dendrogram (Single-linkage)")

# Complete-linkage clustering
agn_complete <- agnes(playerSeasonStats, diss=F, stand=F, metric="euclidean", method="complete")

dend_agn_complete <- as.dendrogram(agn_complete)
plot(dend_agn_complete, xlab="Index of data points", ylab="Steps"
     , main="Dendrogram (Complete-linkage)")

# Average-linkage clustering
agn_avg <- agnes(playerSeasonStats, diss=F, stand=F, metric="euclidean", method="average")

dend_agn_avg <- as.dendrogram(agn_avg)
plot(dend_agn_avg, xlab="Index of data points", ylab="Steps"
     , main="Dendrogram (Average-linkage)")

# Plots including the agglomerative coefficient
par(mfrow=c(1,3))
plot(agn, which.plot=2)
plot(agn_complete, which.plot=2)
plot(agn_avg, which.plot=2)


#### Standardization before clustering ####
playerSeasonStats_z <- data.frame((playerSeasonStats[,1]))
playerSeasonStats_z <- cbind(playerSeasonStats_z, scale(playerSeasonStats[2:15]))
playerSeasonStats_z <- cbind(playerSeasonStats_z, playerSeasonStats[16])
set.seed(1234)  # set this to replicate results

# create a training and testing. This one is 50%/50% train/test
# here I'll just identify the rows that will be used for train
rows = sample(1:nrow(playerSeasonStats_z), round(nrow(playerSeasonStats_z)*.5,0))

#run kmeans for diff values of k so we can identify performance by # of clusters
cost_df <- data.frame() #accumulator for cost results
cost_df
for(k in 1:15){
  #allow up to 50 iterations to obtain convergence, and do 20 random starts
  # train set
  kmeans_tr <- kmeans(x=playerSeasonStats_z[rows,2:16], centers=k, nstart=20, iter.max=100)
  # test set
  kmeans_te <- kmeans(x=playerSeasonStats_z[-rows,2:16], centers=k, nstart=20, iter.max=100)
  
  #Combine cluster number and cost together, write to df
  cost_df <- rbind(cost_df, cbind(k, kmeans_tr$tot.withinss
                                  , kmeans_te$tot.withinss))
}

# the cost_df data.frame contains the # of clusters k and the MSE for each cluster
names(cost_df) <- c("cluster", "tr_cost", "te_cost")
cost_df

# create an elbow plot
par(mfrow=c(1,1))
cost_df[,2:3] <- cost_df[,2:3]/1000
plot(x=cost_df$cluster, y=cost_df$tr_cost, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="MSE (in 1000s)")
points(x=cost_df$cluster, y=cost_df$te_cost, col="green")

################################################################################
# Looking at the information from your k-means clustering
# Lets just generate a k-means clustering for k=4
################################################################################
kmeans_tr <- kmeans(x=playerSeasonStats_z[rows,2:16], centers=10, nstart=20, iter.max=50)
kmeans_te <- kmeans(x=playerSeasonStats_z[-rows,2:16], centers=10, nstart=20, iter.max=50)

# You can look at the fit object to to see how many records are in each cluster,
# the final centroids, the final cluster assignments, statistics of within and
# between clusters
kmeans_tr
traininglabels <- kmeans_tr[1]
# get cluster means (couple different ways)
(centroids <- aggregate(playerSeasonStats_z[rows,2:16], by=list(kmeans_tr$cluster), FUN=mean))
kmeans_te$centers

getwd()

write.table(kmeans_te$centers, file="10clusters_updated_PTS.csv", sep=",", quote=T
            , col.names=T, row.names=T)
1:length(kmeans_tr$cluster)
for(i in 1:length(kmeans_tr$cluster)) {
  player_num <- NULL
  #print(kmeans$cluster)
  #print(as.numeric((kmeans_tr$cluster[i])))
  #print(names(kmeans_tr$cluster[i]))
  player_num <- as.numeric(names(kmeans_tr$cluster[i]))
  playerSeasonStats[player_num, "cluster"] <- as.numeric(kmeans_tr$cluster[i])
}
for(i in 1:length(kmeans_te$cluster)) {
  player_num <- NULL
  #print(as.numeric((kmeans_te$cluster[i])))
  #print(names(kmeans_te$cluster[i]))
  player_num <- as.numeric(names(kmeans_te$cluster[i]))
  playerSeasonStats[player_num, "cluster"] <- as.numeric(kmeans_te$cluster[i])
}
write.table(playerSeasonStats, file="labeled_players_PTS.csv", sep=",", quote=T
            , col.names=T, row.names=T)
# see the number of obs within each cluster
kmeans_tr$size 
kmeans_te$size

################################################################################
## k-Means clustering (evaluation by visually inspecting cluster formations)
# Does a PCA analysis and plots the clusters among the first two PCs
# This takes about 5 minutes to run on my laptop
library(useful)
plot(kmeans_tr, data=playerSeasonStats_z[rows,2:16])

################################################################################
# visualizing clusters using plot3D package
install.packages("XQuartz")
library(plot3D)
par(mfrow=c(1,2))
scatter3D(x = playerSeasonStats[rows,2], y = playerSeasonStats[rows,3], z = playerSeasonStats[rows,4]
          , surface=F, gridlines=26, grid=T, pch=19, point.col = "blue"
          , colvar=kmeans_tr$cluster, colkey = F, border = "black"
          , ticktype = "detailed", bty = "g", lwd = 2, phi = 20
          , main = "k-Means clustering", xlab="Minutes", ylab="FG Made"
          , zlab="FG Attempts")

# see centroids of each cluster
scatter3D(x = centroids[,"MP"], y = centroids[,"FGA"]
          , z = centroids[,"FG."]
          , surface=F, gridlines=26, grid=T, pch=19, point.col = "blue"
          , colvar=centroids$Group.1, colkey = F, border = "black"
          , ticktype = "detailed", bty = "g", lwd = 2, phi = 20
          , main = "Cluster centroids", xlab="Minutes", ylab="FG Made"
          , zlab="FG Attempts")

################################################################################
# visualizing clusters using scatterplot3d package
library(scatterplot3d)
par(mfrow=c(1,1))
scatterplot3d(x = playerSeasonStats[rows,2], y = playerSeasonStats[rows,3], z = playerSeasonStats[rows,4]
              , color = "black", angle=40, cex.symbols=1.3, pch=21
              , main="k-Means clustering", xlab="lat", ylab="long"
              , zlab="bedrooms", bg=kmeans_tr$cluster)
# see centroids of each cluster
scatterplot3d(x = centroids[,"latitude_z"], y = centroids[,"longitude_z"]
              , z = centroids[,"total_bedrooms_z"]
              , color = "black", angle=40, cex.symbols=1.3, pch=21
              , main="Cluster centroids", xlab="lat", ylab="long"
              , zlab="bedrooms", bg=centroids$Group.1)

################################################################################
# visualizing clusters using rgl package
library(rgl)
plot3d(x = playerSeasonStats[rows,2], y = playerSeasonStats[rows,3], z = playerSeasonStats[rows,4], col=kmeans_tr$cluster
       ,xlab="lat", ylab="long", zlab="bedrooms")

################################################################################
# Hierarchcial -linkage clustering using hclust() function
# Uses Euclidean distance as the dissimilarity measure
# the dist() function computes a 50x50 inter-observation Euclidean distance matrix
################################################################################
# trained clusterings
hc.single_tr <- hclust(dist(playerSeasonStats[rows,2:16]), method="single")
hc.complete_tr <- hclust(dist(playerSeasonStats[rows,2:16]), method="complete")
hc.average_tr <- hclust(dist(playerSeasonStats[rows,2:16]), method="average")
# tested clusterings
hc.single_te <- hclust(dist(playerSeasonStats[rows,2:16]), method="single")
hc.complete_te <- hclust(dist(playerSeasonStats[rows,2:16]), method="complete")
hc.average_te <- hclust(dist(playerSeasonStats[rows,2:16]), method="average")

# plot the single-linkage dendogram to show messy view with typical datasets
par(mfrow=c(1,1))
dend_agn <- as.dendrogram(hc.single_tr)  
plot(dend_agn, xlab="data points", ylab="Steps", main="Single-linkage")

# cut the tree so the labels correspond to the rows appropriately
# all the clusterings look sparse for single-linkage
table(cutree(hc.single_tr, k=2))
table(cutree(hc.single_tr, k=3))
table(cutree(hc.single_tr, k=4))
table(cutree(hc.single_tr, k=5))

# it appears that maybe k=4 might be clustering to consider
table(cutree(hc.complete_tr, k=2))
table(cutree(hc.complete_tr, k=3))
table(cutree(hc.complete_tr, k=4))
table(cutree(hc.complete_tr, k=5))

# all the clusterings look sparse for average-linkage
table(cutree(hc.average_tr, k=2))
table(cutree(hc.average_tr, k=3))
table(cutree(hc.average_tr, k=4))
table(cutree(hc.average_tr, k=5))

# create k=4 clusters for the complete-linkage approach
c4_tr <- cutree(tree=hc.complete_tr, k=4)
c4_te <- cutree(tree=hc.complete_te, k=4)

################################################################################
# Lets evaluate our clusters geospatially using Tableau by first saving our
# generated clusters to our dataset (with our original data), then write out
# the results to file for Tableau to use
################################################################################
# create dataset with original features and new cluterings called 'results'
playerSeasonStats$set <- NA
playerSeasonStats[rows,"set"] <- "Train"
playerSeasonStats[-rows,"set"] <- "Test"
playerSeasonStats$kmeans <- NA
playerSeasonStats[rows,"kmeans"] <- kmeans_tr$cluster
playerSeasonStats[-rows,"kmeans"] <- kmeans_te$cluster
playerSeasonStats$comp_link <- NA
playerSeasonStats[rows,"comp_link"] <- c4_tr
playerSeasonStats[-rows,"comp_link"] <- c4_te
results <- cbind(df,playerSeasonStats)

# write out clusters.dlm file (this will be written to your working directory)
write.table(results, file="clusters.dlm", sep="|", quote=T
            , col.names=T, row.names=F)
getwd()

