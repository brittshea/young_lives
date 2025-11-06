# File: Synthetic test for mixed effects model
# Author: Brittany Shea
# Date: 6-12-24

#-------------------------

#install libraries:
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(readxl)
library(janitor)
library(knitr)
library(glmmTMB)
library(lme4)
library(GGally)
library(reshape2)
library(nnet)

#-------------------------

#load data:
r1_pe_ych = read_dta("./data/raw/ych_main/r1_pe_ych.dta")
r2_pe_ych = read_dta("./data/raw/ych_main/r2_pe_ych.dta") 

#-------------------------
#find number of unique children in r1_pe_ych (=1818)
#join_rs_pe_ych_comm %>% 
  #summarise(count = n_distinct(childid)) %>%
  #pull(count)

#-------------------------
#synthetic test 

#generate dataset
n <- 12000
effect_x1 <- 2
effect_x2 <- 2.5

#make predictors
x1 <- rbinom(n, 1, 0.5)
x2 <- rnorm(n)

#create groups/random effects:
#assign observations to groups
group1 <- factor(rep(1:82, length.out = n))
group2 <- factor(rep(1:1800, length.out = n))
group3 <- factor(rep(1:7, length.out = n))
group4 <- factor(rep(1:2, length.out = n))

#make random effects
random_effects_group1 <- rnorm(82, sd = sqrt(.1))
random_effects_group2 <- rnorm(1800, sd = sqrt(.8))
random_effects_group3 <- rnorm(7, sd = sqrt(4))
random_effects_group4 <- rnorm(2, sd = sqrt(5))

#apply random effects based on group membership (mapping each observation to its group's random effect)
random_effect <- random_effects_group1[as.integer(group1)] +
  random_effects_group2[as.integer(group2)] +
  random_effects_group3[as.integer(group3)] +
  random_effects_group4[as.integer(group4)]

#calculate y and transform into a binary outcome
y <- effect_x1*x1 + effect_x2*x2 + random_effect
probabilities <- 1 / (1 + exp(-y))
y <- rbinom(n, 1, probabilities)

#create df
df <- data.frame(x1, x2, group1, group2, group3, group4, y)

#fit model
model <- glmer(y ~ x1 + x2 + 
                 (1 | group1) + (1 | group2) + (1| group3) + (1| group4),
                 data = df, family = binomial, nAGQ=1)

#print summary of model
summary(model)


#plot y versus x, add title 
ggplot(df, aes(x=factor(x1), fill=factor(y))) + 
  geom_bar(position="dodge") + 
  labs(x="X", y="Count", fill="Y") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  ggtitle("Y vs X")
