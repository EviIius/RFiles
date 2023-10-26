library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
Census <- read.csv("Census_2018-2.csv", na.strings = "not reported")
#Question 6.1: How many attributes this data set has?
ncol(Census)
#25

#Question 6.2: How many records this data set has?
nrow(Census)
#164610

#Question 6.3: What is the range of values for each of these attributes: 
#educ_num: 11-1 = 10, citypop: 83987-965 = 83022, incwage: 718000 - 0 = 718000
summary(Census$educ_num)
summary(Census$citypop)
summary(Census$incwage)

#Question 6.4: The missing values in this data set are marked with 
#“not reported”. Which attributes have missing values?
notreported <- Census%>%
  select_if(function(x) any(is.na(x)))
print(notreported)
#empstat, looking, availble, incwage

#Question 7.1: What is the type of each of these attributes: 
#met2013: character, educ: Character, classwkr:Character, inctot:integer
met2013type <- typeof(Census$met2013)
eductype <- typeof(Census$educ)
classwkrtype <- typeof(Census$classwkr)
inctottype <- typeof(Census$inctot)

#Question 7.2: What are the categories in variable classwkrd?
CategoriesClasswkrd <- unique(Census$classwkr)
numberofcategoriesclasswkrd <- length(CategoriesClasswkrd)
# works for wages and self-employed

#Question 7.3: What is the mean and standard deviation for inctot?
meanincot <- mean(Census$inctot)
SDincot <- sd(Census$inctot)
#mean: 65805.12 SD: 83775.61

#Question 8.1: Create a Histogram graph for attribute incwage. Insert the graph 
#in the space below and answer how this attribute is distributed in the data set
incwageint <- as.integer(Census$incwage)
hist(incwageint)
ggplot(data = Census)+geom_histogram(aes(x= incwageint))
#Its skewed to the right of the set mostly because the x-axis is exponentially high

#Question 8.2: Generate a new attribute log_wage = log(wage). Create a Histogram graph for 
#attribute log_wage. Insert the graph in the space below and explain what the differences 
#between this histogram and the histogram are from 8.1. 
log_wage <- log(incwageint)
hist(log_wage)
ggplot(data = Census)+geom_histogram(aes(x= log_wage))
#after logging the wages, the data looks more normal with it being skewed to the left
#and the variables distributed more normally

#Question 9 :You want to explore the relationship between education_num and 
#log_wage. Choose one appropriate graph type and create a graph using ggplot2. By
#observing the graphs you created, please interpret the relationship between 
#education and wage. Insert the graph in the space below: 

ggplot(data = Census) + geom_point(aes(x = log_wage, y = educ_num))

# based on the graph, the higher the wage number based on log_wage, is in
# correlation the the education. The wages are higher for those with higher
#education.

#Question 10: Create a graph shows the number of working hours (uhrswork) by 
#detailed worker’s class (classwkrd). Choose one appropriate graph type and 
#create a graph using ggplot2. Explain why this graph fit the data. 
#Insert the graph in the space below:
ggplot(data = Census) + geom_col(aes(x = uhrswork, y= classwkrd))

#Geom_col creates it in a bar graph format that can display the amount of hours
#each workers class without cluttering it like in most other graphs




