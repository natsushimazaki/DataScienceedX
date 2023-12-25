options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
library(dplyr)
library(ggplot2)
library(dslabs)

titanic <- titanic_train %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#Question 1: Variable Types 
#Inspect the data and also use ?titanic_train to learn more about the variables in the dataset.
?titanic_train

#Question 2: Demographics of Titanic Passengers 
#Make density plots of age grouped by sex. Try experimenting with combinations of faceting, alpha blending, stacking and using variable counts on the y-axis to answer the following questions. Some questions may be easier to answer with different versions of the density plot.
#Females and males had the same general shape of age distribution
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, bw = 2, position = "stack") #+
  #facet_grid(Sex ~ .)
#There were more females than males
table(titanic$Sex)

#The count of males of age 40 was higher than the count of females of age 40
titanic %>% filter(Age == 40) %>% count(Sex)
titanic %>% filter(Age == 40) %>% .$Sex %>% table()

#The proportion of males age 18-35 was higher than the proportion of females age 18-35
titanic %>% filter(18 <= Age & Age <= 35) %>% .$Sex %>% table()
133/length(titanic$Sex)
251/length(titanic$Sex)

#The proportion of females under age 17 was higher than the proportion of males under age 17
titanic %>% filter(Age < 17) %>% .$Sex %>% table()
49/length(titanic$Sex)
51/length(titanic$Sex)

#The oldest passengers were female 
titanic$Sex[which.max(titanic$Age)]

titanic %>% ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2, bw = 2, position = "stack") +
  facet_grid(. ~ Sex)

#Q3: QQ-plot of Age Distribution
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
head(params)
titanic %>% filter(!is.na(Age)) %>% 
  ggplot(aes(sample=Age)) + 
  geom_qq(dparams = params) + 
  geom_abline()

#Question 4: Survival by Sex 
titanic %>% ggplot(aes(Survived, fill = Sex)) + 
  #geom_bar()
  geom_bar(position = position_dodge())

#Question 5: Survival by Age
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age, ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, bw = 2, position = "stack") #+
#facet_grid(Sex ~ .)

#Question 6: Survival by Fare
titanic %>% filter(!Fare == 0) %>% ggplot(aes(Survived, Fare, fill = Survived)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(width = 0.1, alpha = 0.2)
  
#Question 7: Survival by Passenger Class
titanic %>% ggplot(aes(Pclass, fill = Survived)) + 
  geom_bar()
  

titanic %>% ggplot(aes(Pclass, fill = Survived)) + 
  geom_bar(position = position_fill())

titanic %>% ggplot(aes(Survived, fill = Pclass)) + 
  geom_bar()

#Question 8: Survival by Age, Sex, and Passenger Class
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,y = ..count..,fill = Survived)) + 
  geom_density(alpha = 0.2, position = "stack") + 
  facet_grid(Sex~Pclass)
