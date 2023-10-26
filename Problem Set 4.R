#load packages
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
library(margins)
library(haven)
library(lmtest)

#load data
union85 <- read_dta("C:/Users/Jake Brulato/Desktop/R Studio Data Sets/union85.dta")
View(union85)
union85$union <- ifelse(union85$unionmme==1, 1, 0)

#create log variable
ggplot(union85, aes(x=wage)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity") +
  geom_density(alpha=.1)

union85$lnwage <- log(union85$wage)

ggplot(union85, aes(x=lnwage)) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity") +
  geom_density(alpha=.1)

ggplot(union85, aes(x=lnwage, color=factor(union), fill=factor(union))) + 
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha=.1)

#polynomial transformation and interaction
union85$exper2 <- (union85$experience)^2

union85$unionexper <- (union85$unionmme)*(union85$experience)

#Problem Set 4
#1 Run the model below and interpret estimates, pvalues, and Rsquared
summary(lm(lnwage ~ experience + I(experience*experience) + factor(union), data = union85))
#P< 2.2e-16 (Which is low), Rsquared: .2201 (is the percent in variation in lnwages that can be explained by experience and union)
#as experience increase one year, wage should increase at a decreasing rate,
#Union members should have .01408 (because its squared, move the decimal over one more) higher percent on average than non union members

#2 Run the model below and interpret estimates, pvalues, and Rsquared
summary(lm(lnwage ~ experience + factor(union) + factor(union)*experience, data = union85))
#for every one year of additonal experience we expect their wage to increase 1.5% for each additional year of experience they have
#experience:factor(union)if their a union member then the 1.5% gets decrease by .01/1% for every year of additonal experience they have
#this is negated though as factor(union)1 is already increasing wages by 38%
#P< 2.2e-16 (Still low), Rsquared: .1203 (is the percent in variation in lnwages that can be explained by experience and union)



#3 Produce AMEs for the model below and interpret them. Then use the estimates and sample averages to produce the AMEs again but by hand.
mod1 <- lm(lnwage ~ experience + factor(union) + factor(union)*experience, data = union85)
summary(margins(mod1))
#experience has increased peoples wages 1.3% on average based on each additional year of experience they have
#Union members make 21.7% on average more than non-union members
#both P-values are 0 meaning both values are having significance on the average marginal effects of the model
AMEunion1 <- .3891900-0.0101699*(mean(union85$union))


#4 What is heteroskedasticity and does mod1 exhibit heteroskedasticity? One quick fix for heteroskedasticity is to use robust standard errors
lmtest::bptest(mod1)
# P-value is low (2.2e-16) if HO must go= heteroskasticity 
coeftest(mod1, vcov = vcovHC(mod1, type = "HC0"))
#Estimates don't change, meaning the estimates are unbiased, however the standard error and P-value are unreliable