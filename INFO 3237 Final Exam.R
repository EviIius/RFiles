install.packages("data.table")
install.packages("tidytext")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("tm") # text mining 
install.packages("topicmodels") 
install.packages("slam")
install.packages("SnowballC")
install.packages("reshape2")

library(tidyverse)
library(plyr)
library(dplyr)
library(cluster)
library(clustertend)
library(dbscan)
library(data.table)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(tm)
library(topicmodels)
library(slam)
library(tidyr)
library(tidytext)
library(SnowballC)

#(PART A: 35 points) Before analyzing this data using cluster analysis, we want 
#to learn more about the names of colleges and universities in US. 
#Using column “College.Name”, which stands for the name of the institution:

data <- read.csv("universities.csv", sep=",", header=T, 
                 strip.white = T, na.strings = c("NA","NaN","","?"))
data <- na.omit(data)
colnames(data)

#a)	(10 points) Tokenize and remove the stop words from the column “College.Name”, and 
#then determine the top 10 most frequent words in “College.Name”:
#Answer: 

tidy_text <- data %>%
  unnest_tokens(word, College.Name)

data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)

tidy_text %>%
  count(word, sort = TRUE)%>%
  head(10)

# 1     college 327
# 2  university 301
# 3       saint  20
# 4        suny  15
# 5          st  13
# 6       north  11
# 7    southern  11
# 8       texas  11
# 9    carolina  10
# 10   wesleyan  10

#b)	(5 points) Which one is more frequent in the names of institutions, “College” or “University”?
#Answer: 
college_count <- sum(tidy_text$word == "college")
university_count <- sum(tidy_text$word == "university")

if (college_count > university_count) {
  answer <- "College"
} else if (university_count > college_count) {
  answer <- "University"
} else {
  answer <- "Equal frequency"
}

answer

#College was more frequent than University
  
#c)	(10 points) Show the visualization (i.e., bar chart) for the top 18 most 
#frequent words in institution names. Please paste the visualization in the space below:
#Answer: 
top_18_words <- tidy_text %>%
  count(word, sort = TRUE) %>%
  head(18)

ggplot(top_18_words, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Word", y = "Frequency") +
  coord_flip()
  
#d)	(10 points) Which state names (such as “North Carolina”) are among the top 10 
#words in institution names?
#Answer: 
  
top_10_states <- tidy_text %>%
  filter(word %in% c("alabama", "alaska", "arizona", "arkansas", "california", "colorado",
                     "connecticut", "delaware", "florida", "georgia", "hawaii", "idaho", "illinois",
                     "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland",
                     "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana",
                     "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york",
                     "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania",
                     "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah",
                     "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming")) %>%
  count(word, sort = TRUE)%>%
  head(10)
  

top_10_states
# 1         texas 11
# 2    washington  8
# 3      illinois  7
# 4      missouri  7
# 5  pennsylvania  6
# 6    california  5
# 7      colorado  5
# 8       florida  5
# 9       georgia  5
# 10     oklahoma  5


#(PART B: 65 points) Use cluster analysis to identify the groups of characteristically 
#similar schools in universities.csv. 


#a)	First, determine the best clustering method (k-means, hierarchical, DBSCAN) using 
#Silhouette measure. That is, you need to try all three methods and measure their 
#Silhouette scores to determine which method (k-means, hierarchical, or DBSCAN) is best. 
#Then, use the best clustering method to perform cluster analysis and answer the following 
#questions. 

data2 <- subset(data, select = -c(Public..1...Private..2., College.Name, State))
data2 <- na.omit(data2)
data2.sd <- scale(data2) 

#a.	(10 points) What is the best k in the k-means analysis? 
withinssplot <- function(data2, nc=15, seed=1234){
  wss <- 1
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data2, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
withinssplot(data2.sd, nc=10) 

k.mean.fit <- kmeans(data2.sd, 4, nstart = 25)
k.mean.fit$cluster
k.mean.fit$centers
k.mean.fit$size

attributes(k.mean.fit)

clusplot(data2.sd, k.means.fit$cluster, main='2D representation of the Cluster solution', color=TRUE,
         shade=TRUE, labels=2, lines=0)

#b.	(10 points) How many clusters seems reasonable to describe the data from the 
#hierarchical clustering? Show the corresponding dendrograpm. 

withinssplot(data2.sd, nc=10) 
#Id say from 3-5 clusters would be the best based on the sum of squares

#Hierarchical Clustering
d <- dist(data2.sd, method = "euclidean")
H.single <- hclust(d, method = "single")
plot(H.single)

H.complete <- hclust(d, method = "complete")
plot(H.complete)

H.average <- hclust(d, method = "average")
plot(H.average)

H.ward <- hclust(d, method = "ward.D2")
plot(H.ward)

group <- cutree(H.ward, k = 3)
plot(H.ward)
par(mfrow = c(1,1))
rect.hclust(H.ward, k = 3, border = "red" )

#DBSCAN
minPts <- ncol(data2.sd) + 1
knn <- kNNdist(data2.sd, k=minPts)
kNNdistplot(data2.sd, k = 4, minPts = minPts)
eps <- sort(knn)[length(knn) - minPts+1]
abline(h = eps, col = "red")
db <- dbscan(data2.sd, eps = eps, minPts = minPts)
db$cluster

clusplot(data2.sd, db$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


#c.	(10 points) Provide the silhouette measure for all three cluster methods. 
plot(silhouette(k.mean.fit$cluster, d)) #Measure: .16
plot(silhouette(group, d)) #Measure: .15
plot(silhouette(db$cluster, d)) #Measure: .64

#b)	(10 points) Based on the best clustering method can we put UNC- Charlotte, 
#Georgia State University, and University of Arizona in the same cluster? 
#(TIP: You can first create the “cluster” variable which contains the clustering results 
#in the main data set and and use the following script to check the cluster number 
#for each institution: 
#data[,c("College.Name","cluster")] %>% filter(College.Name == "Georgia State University"); Any other methods are also welcome. )
#Answer: 
data$cluster <- db$cluster
cluster <- data[, c("College.Name", "cluster")] %>%
  filter(College.Name %in% c("University of North Carolina at Charlotte", "Georgia State University", "University of Arizona"))
cluster

#Based on the DBSCAN method which had the highest measure of the clustering methods, we can put them under the same cluster.
#c)	(10 points) How about UNC- Chapel Hill and University of Texas at Austin? 
#Answer: 
cluster <- data[, c("College.Name", "cluster")] %>%
  filter(College.Name %in% c("University of North Carolina at Charlotte", "Georgia State University", "University of Arizona", 
                             "University of Texas at Austin", "University of North Carolina at Chapel Hill"))
cluster

#They also fall under the same cluster

#d)	(15 points) Compare the center points (i.e., the mean of all the numeric university characteristics) 
#of each cluster you generated and try to provide a label for each cluster 
#(e.g., universities with high graduation rate, low tuition, etc.). 

mean_values <- aggregate(data, by = list(cluster = cluster), FUN = mean)
mean_values
