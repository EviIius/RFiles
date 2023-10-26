library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
library(rattle)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(pROC)
library(corrplot)
library(forecast)

#3.	Read the file: 
#data <- read.csv("election_campaign_data.csv", sep=",", 
#header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

data <- read.csv("election_campaign_data-1.csv", sep=",", 
                 header=T, strip.white = T, 
                 na.strings = c("NA","NaN","","?")) 

#4.	Drop the following variables from the data: "cand_id", "last_name", 
#"first_name", "twitterbirth", "facebookdate", "facebookjan", "youtubebirth".
data2 <- subset(data, select = -c(cand_id, last_name,first_name, 
                                  twitterbirth, facebookdate, facebookjan, 
                                  youtubebirth))
#5.	Convert the following variables into factor variables: 
#“twitter”, “facebook”, “youtube”, “cand_ici”, and “gen_election”.
data2$gen_election <- as.factor(ifelse(data2$gen_election == "W", 1, 0))
cols <- c("twitter", "facebook", 
           "youtube","cand_ici", "gen_election")
data2[,cols] <- lapply(data2[,cols], factor)
str(data2)

# 7. Remove all of the observations with any missing values
data2 <- na.omit(data2)

#8.	Answer the following questions:

#8.1.	(4 points) How many attributes this data set has? 
ncol(data2)
#21

#8.2.	(4 points) How many records this data set has? 
nrow(data2)
#929

#8.3.	(4 points) How many classes the class attribute (gen_election) has?
levels(data2$gen_election)
#2

#8.4.	(4 points) How many candidates had a Facebook campaign account (column facebook=1)?

data2 %>%
  group_by(facebook)%>%
  count(1)
length(which(data2$facebook == 1))

#242

#8.5.	(4 points) How many candidates who had a twitter campaign account won the election? 

data2 %>%
  group_by(twitter)%>%
  filter(gen_election == 1)%>%
  count()
length(which(data2$twitter == 1 & data2$gen_election == 1))
#185

#8.6.	(4 points) How many incumbent candidates won the election?
data2 %>%
  group_by(cand_ici)%>%
  filter(gen_election == 1)%>%
  count("I")

length(which(data2$gen_election == 1 & data2$cand_ici == "I"))
#339

#9.	Use “rpart” and “rpart.plot” packages to create decisions trees and answer 
#the questions below (please set.seed = 100).
set.seed(100)
set.seed = 100

#9.1.	(8 points) Create a decision tree using variable gen_election as the target (class) variable 
#and the variables twitter, facebook, and youtube as the predictors. Paste the tree in the space below.
trainIndex <- createDataPartition(data2$gen_election, times = 1, p = 0.7, 
                                  list = FALSE)
train_data <- data2[trainIndex, ]
validation_data <- data2[-trainIndex, ]

tree1 <- rpart(gen_election ~ twitter + facebook + youtube, 
               data =  train_data)
printcp(tree1)
plotcp(tree1)
fancyRpartPlot(tree1)

#9.2.	(4 points) Use “summary” to check which variable had the highest importance in the decision tree. 
#List the variables based on the highest to lowest importance.

summary(tree1)
varImp(tree1)

#youtube = 42, facebook = 33, twitter = 25

#9.3.	(8 points) How many of the candidates were correctly classified in the decision tree?
(.77 * 651 * .66) + (.81 * 651 * .02) + (.92 * 651 * .06) + (.95 * 651 * .26)

#538

#9.4.	(4 points) Which leaf/terminal node is the purest node in the decision tree? Use GINI Index.


#Node  with a probability of .05 and .95 = .95


#10.	Now add ttl_receipts and cand_ici to the list of predictors. 

#10.1.	(8 points) Create and paste the decision tree in the space below.
tree2 <- rpart(gen_election ~ twitter + facebook + youtube + ttl_receipts + cand_ici, 
               data =  train_data)

printcp(tree2)
plotcp(tree2)
fancyRpartPlot(tree2)



#10.2.	(4 points) Use “summary” to check which variable had the highest importance in the decision tree. 
#List the variables based on the highest to lowest importance.
summary(tree2)
varImp(tree2)

# cand_ici = 29, ttl_receipts = 25, youtube = 18, facebook = 16, twitter = 13

#10.3.	(6 points) Find the min error tree. Please report the cross validation set error 
#and paste the tree in the space below. 
ptree2 <- prune(tree2, cp = tree2$cptable[which.min(tree2$cptable[, "xerror"]), "CP"])
fancyRpartPlot(ptree2)
summary(ptree2)
#xerror: 0.2393443\\ from split 5


#11.	Use packages “caret” and “e1071” to compare the two decision trees 
#(the first one from 9.1 and the second one from 10.3) that you’ve created to 
#answer the following questions. Use threshold <- 0.5.(Notes: Please use set.seed = 100 
#for all the following answers, otherwise you will lose 10 points)
set.seed = 100
set.seed(100)
threshold <- .5

#11.1.	(4 points) What is the accuracy of the first decision tree?
predicted_values1 <- predict(tree1, validation_data, type = "prob")
head(predicted_values1)
pred1 <- factor(ifelse(predicted_values1[,2] > 0.5, 1, 0))
levels(pred1)

(141+90)/(141+40+7+90)=0.8309353
#0.8309
#11.2.	(8 points) Fill out the confusion matrix below using the first decision tree:

cm1 <- confusionMatrix(pred1, # predicted class
                      validation_data$gen_election, # actual class
                      positive = levels(validation_data$gen_election)[2])
cm1
cm1$table
cm1$byClass
#11.3.	(4 points) What is the accuracy of the second decision tree?

(133+115)/(133+115+15+15)= 0.8920863
#.885
#11.4.	(8 points) Fill out the confusion matrix below using the second decision tree:
predicted_values2 <- predict(ptree2, validation_data, type = "prob")
pred2 <- factor(ifelse(predicted_values2[,2] > 0.5, 1, 0))
cm2 <- confusionMatrix(pred2, # predicted class
                       validation_data$gen_election, # actual class
                       positive = levels(validation_data$gen_election)[2])
cm2
cm2$table
cm2$byClass

#12.	(10 points) Based on your findings, which one is more important? 
#Having social media accounts for the campaign or raising more funds for 
#the campaign? Explain your answer. 

#I would say the more important one would be the second one based on the accuracy
#rate for the second one being higher. Based on the testing data and validation data,
#raising more funds has been more successful than the social media accounts.
  


