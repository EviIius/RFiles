library(mlbench) 
library(randomForest)
library(tidyverse)
library(plyr)
library(dplyr)
library(rpart.plot)
library(caret) # for general model fitting 
library(rpart) # for decision tree
library(rattle) # for decision tree plot library(rpart.plot)
library(e1071)
library(pROC)
library(ipred)
library(devtools)

#1.	In Canvas, navigate to Assignments and then Assignment3

#2.	Download and save the data set election_campaign_data.csv

#3.	Read the file: data <- read.csv("election_campaign_data.csv", 
#sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 

data <- read.csv("election_campaign_data-1.csv", sep=",", header=T, 
                 strip.white = T, na.strings = c("NA","NaN","","?")) 

#4.	Drop the following variables from the data: "cand_id", "last_name", "first_name", 
#"twitterbirth", "facebookdate", "facebookjan", "youtubebirth".

data2 <- subset(data, select = -c(cand_id, last_name,first_name, 
                                  twitterbirth, facebookdate, facebookjan, 
                                  youtubebirth))

#5.	Convert the following variables into factor variables using function mutate_at(): 
#"twitter","facebook","youtube","cand_ici","cand_pty_affiliation", "gender", "gen_election".
data2$gen_election <- as.factor(ifelse(data2$gen_election == "W", 1, 0))
data2 <- data2%>%
  mutate_at(c("twitter","facebook","youtube","cand_ici","cand_pty_affiliation", "gender", "gen_election"), factor)

#6.	Bear in mind that “twitter” equals 1 if the candidate had a Twitter campaign during the election 
#and zero otherwise. The same would apply for “facebook” and “youtube”. “opp_fund” is the total campaign fund 
#of the opposing candidate. “gen_election” is our target variable which takes value of “L” when the candidate lost 
#the election and “W” when the candidate won the election. More descriptions about the variables can be found in 
#Table 1.


#7.	Remove all of the observations with any missing values using function drop_na(). Also use the appropriate code 
#to create a train_data and validation_data (70% of original data in the training data and 30% in the testing data).
data2 <- drop_na(data2)

trainIndex <- createDataPartition(data2$gen_election, p = 0.7, list = FALSE, times =1)

train_data <- data2[trainIndex, ] 
validation_data <- data2[-trainIndex, ]

#8.	Use package “caret” and train_data to create a random forest classifier and answer the questions below. 
#Create a random forest using variable gen_election as the target (class) variable and all 
#of the other variables as predictors (do not include variables such as ID that you 
#think should be excluded from the analysis.).set.seed = 100
set.seed = 100
set.seed(100)

#8.1.	(4 points) Use 5 as the value for ntree and use 2 as the value for mtry. What is the accuracy of the model?
control <- trainControl(method = "cv", number = 10)
rf1 <- train(gen_election~., data = train_data, method = "rf", ntree = 5, 
             tuneGrid = expand.grid(.mtry = 2), trControl = control)
print(rf1)
#0.8941353


#8.2.	(8 points) Keep ntree as 5, but change mtry to 7. What is the accuracy of the model? Between 2 and 7, 
#which value of mtry would you recommend? 
rf2 <- train(gen_election~., data = train_data, method = "rf", ntree = 5, 
             tuneGrid = expand.grid(.mtry = 2:7), trControl = control)
print(rf2)

#I would recommend mtry 7 as it has the highest accuracy

#8.3.	(4 points) Now use the recommended value of mtry but change ntree to 10. What is the accuracy of the model? 

rf3 <- train(gen_election~., data = train_data, method = "rf", ntree = 10, 
             tuneGrid = expand.grid(.mtry = 7), trControl = control)

print(rf3)
plot(varImp(rf3))
#0.9278234

#8.4.	(8 points) Now use the recommended value of mtry but change ntree to 50. What is the accuracy of 
#the model? Between 10 and 50, which value of ntree would you recommend?
rf4 <- train(gen_election~., data = train_data, method = "rf", ntree = 50, 
             tuneGrid = expand.grid(.mtry = 7), trControl = control)

print(rf4)
plot(rf4)
plot(varImp(rf4))

#0.9462689 
#I would choose 50 as the data looks more distributed and the accuracy is higher than rf3

#8.5.	(10 points) Use your recommended ntree and mtry to build the random forest classifier based on train_data.
#Now make predictions over validation_data. Set threshold at 0.5. Create the confusion matrix and 
#paste it in the space below:
threshold = .5
predicted_values1 <- predict(rf4, validation_data, type = "prob")
head(predicted_values1)
pred1 <- factor(ifelse(predicted_values1[,2] > 0.5, 1, 0))

cm1 <- confusionMatrix(pred1, # predicted class
                       validation_data$gen_election, # actual class
                       positive = levels(validation_data$gen_election)[2])
cm1$table
#8.6. (10 points) Now create the ROC curve and paste in the space below:

roc1 <- roc(predictor = predicted_values1[,2], 
            response = validation_data$gen_election, 
            levels = levels(validation_data$gen_election))
plot(roc1)

#8.7. (6 points) Use varImp(rf) to determine the top three variables in predicting gen_election.
varImp(rf4)
#other_pol_cmte_contrib  100.000
#coh_cop                  66.814
#ttl_disb                 44.883

#9.	Now use boosting method to build a classifier using train_data. Try the following values of nIter (5,10,50).
devtools::install_github("souravc83/fastAdaboost")
boosting5 <- train(gen_election~., data = train_data, 
                    method = "adaboost", tuneGrid = expand.grid(nIter = 5, method = "adaboost"), 
                    trControl = control)
print(boosting5)

boosting10 <- train(gen_election~., data = train_data, 
                   method = "adaboost", tuneGrid = expand.grid(nIter = 10, method = "adaboost"), 
                   trControl = control)
print(boosting10)


boosting50 <- train(gen_election~., data = train_data, 
                   method = "adaboost", tuneGrid = expand.grid(nIter = 50, method = "adaboost"), 
                   trControl = control)
print(boosting50)

boostingall <- train(gen_election~., data = train_data, 
                    method = "adaboost", tuneGrid = expand.grid(nIter = c(5,10,50), method = "adaboost"), 
                    trControl = control)
print(boostingall)

#9.1.	(10 points) What is the best number of iterations according to the output of boosting model? 
#Show the plot of accuracy vs. iteration and include it below: 
#AdaBoost Classification Trees 

#651 samples
#20 predictor
#2 classes: '0', '1' 
  
#  nIter  Accuracy   Kappa    
#5     0.9232372  0.8460622
#10     0.9186436  0.8372197
#50     0.9324672  0.8647268

#Tuning parameter 'method' was held constant at a value of adaboost
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were nIter = 50 and method = adaboost.

  
#9.2.	(10 points) Create the confusion matrix based on validation data:
predicted_boosting <- predict(boosting50, validation_data, type = "prob")
predb <- factor(ifelse(predicted_boosting[,2] > 0.5, 1, 0))
cmb <- confusionMatrix(predb, validation_data$gen_election,
                       positive = levels(validation_data$gen_election)[2])

#9.3.	(10 points) Create the ROC curve and paste in the space below:
rocb <- roc(predictor = predicted_boosting[,2], 
            response = validation_data$gen_election, 
            levels = levels(validation_data$gen_election))
plot(rocb)
  
#10.	 (20 points) If you were a political consultant, between random forest and boosting method, 
#which one would have you used to make predictions about the elections outcome? Why? 
#If you were giving advice to politicians, you would have encouraged them to take 
#what actions (list at least 2 actions) to help them win the election?
varImp(rf4)

#There are two reasons I would choose the random forest over the roc curve for making my predictions.
#The first is that the roc curve for boosting has less sensitivity than the one for random forest.
#The second is that the accuracy of predictions for random forest is higher than it is on boosting
#The two actions that I advise are increasing the campaigners digital footprint so youtube, facebook, and
#twitter can be higher in the prediction. Increase the contributions of the candidates themselves as to
#portray them better while taking less from other committees as it could make assumptions that harm the 
#candidates brand

  