library(tidyverse)
library(plyr)
library(dplyr)
library(cluster)
library(clustertend)
library(dbscan)
# 3.	Read the file: data <- 
#fread("hospital_ortho.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

data <- read.csv("hospital_ortho.csv", sep=",", header=T, 
                  strip.white = T, na.strings = c("NA","NaN","","?")) 


# 4.The original data includes hospitals across the US. However, we can only sell our products in NC and the 
# nearby states of SC, VA, GA, and TN. Create a dataset that only contains data from those states. Name this data nc_data.

nc_states <- c("NC", "SC", "VA", "GA", "TN")

nc_data <- data %>%
  filter(state %in% nc_states)


# 4.1.(6 points) Look at each individual variable and decide if it should be included in cluster analysis. 
# For those variables that you decide not to include, give your reasons for exclusion.

colnames(nc_data)
summary(nc_data)

# I recommend to remove the city, state, and hid for the data as these aren't numerical data types and wouldn't allow the data
# to be properly scaled. zip is unnecessary for model and th, trauma ,rehab are all categorical so they would also be removed.
# Other than this, I think most of the data will work for what were looking for.

#nc_data <- subset(nc_data ,select = -c(hid, city, state))

nc_data <- subset(nc_data ,select = -c(hid, city, state, th, trauma ,rehab))
# 4.2.(6 points) Do you need to scale this data? Why? 

# Yes We do this to normalize the data and make then closer together when its already spread across the variables.
df <- scale(nc_data[-1])
   
# 5.	Perform k-means clustering:
set.seed(100)

# 5.1.(6 points) Use “Within Groups SSE” to determine the number of clusters for k-means. 
# How many clusters you would like to create? 

withinssplot <- function(nc_data, nc = 30, seed = 1234){
  wss <- 1
  for(i in 1:nc){
  wss[i] <- sum(kmeans(nc_data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of square")
}

withinssplot(df, nc = 10)

#4

# 5.2.(3 points) Paste the “Within Groups SSE” plot in the space below:
withinssplot(df, nc = 10)

# 5.3.(3 points) Perform k-means clustering using the number of clusters you recommended in 5.1. 
# How many hospitals fall in each cluster? 

k.mean.fit <- kmeans(df, 4, nstart = 25)
k.mean.fit$cluster
k.mean.fit$centers
k.mean.fit$size

attributes(k.mean.fit)

# 1 124 341  43

# 5.4.(3 points) Create a two-dimensional representation of the clusters and paste it below:
clusplot(df, k.mean.fit$cluster, main = "2D representation of the cluster solution", 
         color = TRUE, shade = TRUE, labels = 2, lines = 0)

# 6.	Perform Hierarchical clustering.

# 6.1.	(10 points) Try different hierarchical clustering (“single”, “complete”, “average”, and “ward.D2”) 
# and paste the dendrograms in the space below:

distance <- dist(df, method = "euclidean")

H.single <- hclust(distance, method = "single")
plot(H.single)

H.complete <- hclust(distance, method = "complete")
plot(H.complete)

H.average <- hclust(distance, method = "average")
plot(H.average)

H.ward <- hclust(distance, method = "ward.D2")
plot(H.ward)


# 6.2.	(6 points) Determine which hierarchical clustering method would be more appropriate for this data. Why? 
#If I had to choose from the 4 that are given it would be eiter complete of wards method.
#Both produce the most complete result and groups it better than the others, but Wards is cleaner.
#I would choose Ward before complete.

# 6.3.	(6 points) Based on hierarchical clustering results, how many clusters do you find in this data? 
group <- cutree(H.ward, k = 3)
plot(H.ward)

# 3 clusters

# 6.4.	(3 points) Paste the dendrogram that you chose with the red borders around the clusters in the space below:

par(mfrow = c(2,2))
plot(H.single)
plot(H.complete)
plot(H.average)
plot(H.ward)
par(mfrow = c(1,1))

plot(H.ward)
rect.hclust(H.ward, k = 3, border = "red" )
# 7.	Perform DBSCAN cluster analysis: 


# 7.1.	(3 points) First, you need to determine minPts. The rule of thumb for minPts is the number of 
# dimensions (variables) of the data + 1. 

minPts <- ncol(df) + 1

# 7.2.	(6 points) Based on your suggested minPts, determine the eps. 
knn <- kNNdist(df, k=minPts)
kNNdistplot(df, k = 4, minPts = minPts)
abline(h = eps, col = "red")

eps <- sort(knn)[length(knn) - minPts+1]
# 7.3.	(6 points) Perform DBSCAN clustering using the minPts and eps that you recommended. 
# How many clusters DBSCAN returns?
db <- dbscan(df, eps = eps, minPts = minPts)

# 1 cluster

# 7.4.	(6 points) How many noise points it returns? 

# 4 noise points

# 7.5.	(6 points) Create a two-dimensional representation of DBSCAN cluster(s) and paste it in the space below:
clusplot(df, db$cluster, main = "2D representation of the cluster solution", 
         color = TRUE, shade = TRUE, labels = 2, lines = 0)
hullplot(df, db)

# 8.	Calculate the silhouette score for cluster analysis:


# 8.1.	(7 points) What is the silhouette score for k-means? Paste the silhouette plot in the space below:
plot(silhouette(k.mean.fit$cluster, distance))

#Score: .46

# 8.2.	(7 points) What is the silhouette score for hierarchical “Ward”? Paste the silhouette plot in the space below:
plot(silhouette(group, distance))

#Score: .45
 
# 8.3.	(7 points) What is the silhouette score for DBSCAN? Paste the silhouette plot in the space below:
plot(silhouette(db$cluster, distance))

#Score: .74