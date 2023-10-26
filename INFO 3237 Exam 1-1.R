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

#Part 1
#a)	[4 points] Import the data set into R. Drop all the missing values (i.e., “N/A” and blank) 
#and report the number of observations and variables you have.

data <- read.csv("Airbnb_NC_spring2023.csv", sep=",", header=T, 
                 strip.white = T, na.strings = c("", "N/A", "NA", "?")) 
data <- na.omit(data)
sum(is.na(data))
#1910 observations and 25 variables



#b)	[8 points] Which types of room ("Entire home/apt”, "Private room", "Shared room", "Hotel room") 
#receive the highest number of reviews on average? Please provide evidence below. 

data %>%
  group_by(room_type) %>%
  summarise(count = n(),
            mean_num_review = mean(number_of_reviews, na.rm = TRUE)) %>%
  arrange(desc(mean_num_review))

#although the count of it is low, the highest number of review on average is the shared room at 141 and then after that
#is the private room at 96.7, Entire home/apt at 88.5, and hotel room at 16.3




#c)	[10 points] Do in-state hosts (i.e., from North Carolina) receive more reviews than out-of-state hosts? 
# Please provide evidence.


data %>%
  group_by(host_in_state) %>%
  summarise(count=n(), sum_reviews = sum(number_of_reviews, na.rm = TRUE))

#In-state host count for sum of reviews is 151,658 which is significantly higher than out of state which is 19100.
#This means the in-state hosts receive more than out of state.




#d)	[15 points] You would like to explore the relationship between listing price and number of reviews. 
#Please select the appropriate graph type to show the relationship between these two variables 
#(Notes: please use ggplot2 package). Attach the plot below and then interpret the relationship. 

ggplot(data = data) + geom_point(mapping = aes(x = price, y = number_of_reviews))

#as the price increases, the number of reviews decreases drastically, meaning after a certain point the reviews will
#decrease in volume

#a)	[10 points] Some people say that a super host tends to receive a greater number of reviews on average. 
#Is this statement true? Use box plot to show the visualization of number of reviews for super host and regular host. 
#Attach the plot below and then summarize what you observe.
super_hosts <- data %>% 
  filter(host_is_superhost == "t")
normal_hosts <- data %>% 
  filter(host_is_superhost == "f")

reviews <- data.frame(
  host_type = c(rep("Super Host", nrow(super_hosts)), rep("Regular Host", nrow(normal_hosts))),
  number_of_reviews = c(super_hosts$number_of_reviews, normal_hosts$number_of_reviews))

ggplot(data = reviews) + geom_boxplot(aes(x = host_type, y = number_of_reviews, fill = host_type))

#This is true as the margins for each percentile of the box is vastly larger along with the number of reviews being higher on
#the super host side of the box and whisker plot. The larger box size indicates more reviews based on the y-axis.

#Part 2

#b)	[5 points] Generate a new target variable “popularity” which equals to 1 (i.e., high popularity) 
#if the number of reviews is larger than 52, otherwise equals to 0 (i.e., low popularity). 
#(Note: use ifelse function to generate the popularity measure and then exclude the number of reviews from your model).

data_popularity <- data %>%
  mutate(popularity = factor(ifelse(number_of_reviews > 52, 1, 0)))

data_popularity <- select(data_popularity, -number_of_reviews)

#c)	[5 points] Any variables you want to exclude from your model (besides number of reviews)? 
#Please mention at least two variables and explain reasons. 
data_popularity <- select(data_popularity, c(-listing_url, -host_has_profile_pic, 
                                             -description, -name, -host_name,-host_id))

#for the variables I removed, I chose the ones that I thought least impacted the data when it was decided to choose the 
#predictor for the models, trees, rocs and random forest I'm making. The listing_url was the first to go as that doesn't
#correlate to popularity metric as its just a URL to the booking link. The host profile pic is irrelevant as that only
#matters if were looking for information for the host, not the popularity. The description was just a description of the 
#property for the listing so on a normal scale, it doesn't affect the popularity. The name was something I was iffy on, but
#people at the at an airbnb typically focus more on the house type, size, etc. rather than the name of the property.
#host_name is another one where it would only matter if we were looking for information on the host rather than popularity.
#host_id is the same as host_name, where it doesn't fit with popularity.

#d)	[5 points] Any variables you want change the data type? Please provide explanation below.
data_popularity$host_in_state <- as.factor(ifelse(data_popularity$host_in_state == T, 1, 0))
data_popularity$host_is_superhost <- as.factor(ifelse(data_popularity$host_is_superhost == "t", 1, 0))
data_popularity$host_identity_verified <- as.factor(ifelse(data_popularity$host_identity_verified == "t", 1, 0))
data_popularity$instant_bookable <- as.factor(ifelse(data_popularity$instant_bookable == "t", 1, 0))
cols <- c("price", "review_scores_rating", "accommodates", "property_type")

data_popularity[,cols] <- lapply(data_popularity[,cols],factor)

#I decided to recode host_in_state, host_is_superhost, host_identity_verified, instant_bookable as factors with the levels
#being 0 = False, and 1 = True to keep it consistent all along the dataset. I also applied factors to variables that could
#potentially be predictors for popularity

#e)	[5 points] Build a decision tree model to predict the popularity of the listings 
#(Notes: please use set.seed = 100 and set the cp of the decision tree model equals to 0.001). 
#Then show the cp table below. 
set.seed = 100
set.seed(100)
trainIndex <- createDataPartition(data_popularity$popularity, times = 1, p = 0.7, 
                                  list = FALSE)
train_data <- data_popularity[trainIndex, ]
validation_data <- data_popularity[-trainIndex, ]

tree1 <- rpart(popularity ~.,
               data =  train_data, cp = .001)
fancyRpartPlot(tree1)
print(tree1$cptable)

#f)	[5 points] Based on the cp table, does the current tree have overfitting problem? 
#If yes, please prune the tree and select the min error tree as your final model. 
#If no, please explain why not and use the model in question f as your final model. 

ptree1 <- prune(tree1, cp = tree1$cptable[which.min(tree1$cptable[, "xerror"]), "CP"])
fancyRpartPlot(ptree1)

#The tree has many predictors which caused over fitting so we prune it here

#g)	[10 points] Report the confusion matrix, accuracy, and ROC curve of your final model below.

predicted_values1 <- predict(ptree1, validation_data, type = "prob")

pred1 <- factor(ifelse(predicted_values1[,2] > 0.5, 1, 0))

cm1 <- confusionMatrix(pred1, # predicted class
                       validation_data$popularity, # actual class
                       positive = levels(validation_data$popularity)[2])
cm1
#Accuracy: 0.7413 
roc1 <- roc(predictor = predicted_values1[,2], response = validation_data$popularity, 
            levels = levels(validation_data$popularity))

plot(roc1)
#h)	[10 points] Build a random forest model and set ntree = 30. Please select mtry equal to 5 
#the random forest model.And then report the predictive performance confusion matrix and accuracy of the model.
control <- trainControl(method = "cv", number = 10)
rf1 <- train(popularity~., data = train_data, method = "rf", ntree = 30, 
             tuneGrid = expand.grid(.mtry = 5), trControl = control)

print(rf1)
#rf1 Accuracy:  0.7488834 
predicted_values2 <- predict(rf1,validation_data, type = "prob")
pred2 <- factor(ifelse(predicted_values2[,2] > 0.5, 1, 0))

cm2 <- confusionMatrix(pred2, # predicted class
                       validation_data$popularity, # actual class
                       positive = levels(validation_data$popularity)[2])

cm2

#cm2 Accuracy: Accuracy : 0.7605

#i)	[10 points] Based on your analysis, which model do you recommend? Can we achieve a high level 
#of predictive performance in predicting high popularity listings using this data set? 
#Then discuss if the data is good for this task or not. 

#I think the random forest model is the most accurate out of all the matrices and trees created. I think it would be difficult
#to properly use this data set without cleaning up the variables from the start as there are many outlines that focus outside
#the scope of popularity for listings. Once refined I think its possible to find it with limited categories, otherwise I don't
#think it would be that helpful.

#j)	[5 points] Based on the results from the optimal model, identify the most important predictors in this task. 
#Please mention at least three variables.
varImp(rf1)
#id                                             100.000
#host_listings_count                             30.542
#review_scores_rating5                           28.569
#bedrooms                                        14.998
#review_scores_rating4.97                        14.640
#host_is_superhost1                               7.855


#The top three ones a characterized as the most important, but id made the whole listing able to be popular so it makes sense 
#why its 100. The next three are in my opinion the true predictors of why people chose the listing as a repeat of listings 
#creates a footprint/reputation to incur other potential customers. Other peoples reviews are a huge indicator of whether 
#this place was good for others and potential be a good fit for the browsing customer. Bedrooms is another thing for people 
#to be able to sleep at the very least comfortable in their own margin.
