#Load packages

library(dplyr)
library(ggplot2)
library(stargazer)
library(ggcorrplot)
library(AER)
library(car)    
library(cem)
library(sandwich)
library(nortest)
library(cowplot)
library(jtools)
library(ggstance)
library(haven)

#load heart.csv

heart <- read.csv("C:/Users/Jake Brulato/Desktop/R Studio Data Sets/heart.csv")
View(heart)

varnames <- tolower(colnames(heart))
colnames(heart) <- varnames

#Problem Set 3

#1 Produce summary statistics for all variables (use summary(datasetname)), and write a few sentences inside R script about these statistics
summary(heart)
#the statistics seems saturated, with resting bp being min 0, and cholesterol being normal average of around 200, ages have a low to high range
#causing the study to have a lot of variabvility. May be a random sample as it test a large amount of people.

#2 Produce a histogram of cholesterol
hist(heart$cholesterol)

#3 Produce a histogram of restingbp (this is resting blood pressure)
hist(heart$restingbp)
#3a Do you notice anything about these two variables above that is weird? Can someone have a resting heart rate of zero or cholesterol of zero?
# a person cannot have a resting heart rate of 0, if they did they would be dead, along with having a 0 cholesterol, indicates inaccurate results.

#4 Create a new dataset where you drop observations with zero cholesterol and zero resting blood pressure (*from this point on use this new dataset for all problems that follow*)
newheart <- subset(heart, cholesterol > 0, restingbp > 0)

#5 Reproduce histogram of cholesterol and resting blood pressure with new dataset and make sure you actually dropped the zeros
hist(newheart$cholesterol)
hist(newheart$restingbp)

#6 this one is too long to put in 1= yes 0 = no
heartdis <- group_by(newheart, heartdisease)
averheart <- summarise(heartdis,
                       count = n(),
                       avgchol = mean(cholesterol, na.rm = TRUE),
                       avgage = mean(age, na.rm = TRUE),
                       avgrestingbp = mean(restingbp, na.rm = TRUE),
                       avgmaxhr = mean(maxhr, na.rm = TRUE),
                       
#7 the graph has .025 density between 120 to 150 on resting bp

newheart$heartdisease <- as.factor(newheart$heartdisease)
ggplot(newheart, aes(x=restingbp, fill=heartdisease)) + geom_density(alpha=.3)

                       
#8 Estimate resting blood pressure as a function of cholesterol, age, maxhr, and heart disease and interpret all estimates and pvalues
mod1 <- lm(restingbp ~ cholesterol + age + maxhr + heartdisease, data = newheart)
summary(mod1)

#9 Interpret rsquared = .07781
summary(mod1)

#10 Use car package and linearHypothesis command to test the hypothesis that the effect of heart disease is equal to the effect of 6 years of additional age
linearHypothesis(mod1, "heartdisease = 0*age") #heartdisease = 0
linearHypothesis(mod1, "heartdisease = 6*age") #heartdisease = 0
# both have no heart disease that affects the resting bp and is similar to a heart that just aged for 6 years

#11 Investigate classical linear assumptions
#1 linear in parameters = yes
#2 Random Sampling = yes
#3 No perfect multicollinearity = yes
#4 Zero conditional mean 
mean(mod1$residuals) #zero
#lets look at plots of residuals, should appear random, there are some patterns there, not ideal
par(mfrow = c(2, 2))
plot(mod1)
#5 No heteroskedasticity 
lmtest::bptest(mod1) # good
#6 population error normally distributed - does not matter because n is large enough for asymptotics to have kicked in, probably