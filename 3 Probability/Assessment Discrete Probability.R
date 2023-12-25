options(digits = 3)
library(gtools)
library(tidyverse)
#create possible medals
medal <- c(1,2,3)
# Question 1a 
#How many different ways can the 3 medals be distributed across 8 runners?
nrow(permutations(8,3))

#How many different ways can the three medals be distributed among the 3 runners from Jamaica?
nrow(permutations(3,3))

#What is the probability that all 3 medals are won by Jamaica?
nrow(permutations(3,3))/nrow(permutations(8,3))

#Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

#For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 runners representing the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
#Calculate the probability that all the runners are from Jamaica.

B <- 10000
set.seed(1)
prob_jamaican <- replicate(B, {
  sim_run <- sample(runners, 3, replace = FALSE)
  is_jamaican <- all(sim_run == "Jamaica")
  is_jamaican
})
mean(prob_jamaican)

#Question 2: Restaurant management
#How many meal combinations are possible with the current menu?
nrow(combinations(6,1)) * 
  nrow(combinations(6,2)) * 
  nrow(combinations(2,1))
#Total combinations=Ways to choose entree×Ways to choose sides×Ways to choose drink

#How many combinations are possible if he expands his original special to 3 drink options?
nrow(combinations(6,1)) * 
  nrow(combinations(6,2)) * 
  nrow(combinations(3,1))

#How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
nrow(combinations(6,1)) * 
  nrow(combinations(6,3)) * 
  nrow(combinations(3,1))

#- Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.

#- Use sapply() to apply the function to entree option counts ranging from 1 to 12.
#What is the minimum number of entree options required in order to generate more than 365 combinations?
howmany <- function(x){
  choices <- nrow(combinations(x,1)) * 
    nrow(combinations(6,2)) * 
    nrow(combinations(3,1))
  choices
}

entree_no <- c(1:12)
combos <- sapply(entree_no, howmany)
data.frame(entrees = entree_no, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)

#Alternative code
entree_choices <- function(x){
  x * nrow(combinations(6,2)) * 3
}

combos <- sapply(1:12, entree_choices)

data.frame(entrees = 1:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)

#The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.

#- Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.

#- Use sapply() to apply the function to side counts ranging from 2 to 12.
#What is the minimum number of side options required in order to generate more than 365 combinations?
howmanysides <- function(x){
  choices <- nrow(combinations(6,1)) * 
    nrow(combinations(x,2)) * 
    nrow(combinations(3,1))
  choices
}

sides_no <- c(2:12)
comboside <- sapply(sides_no, howmanysides)
data.frame(sides = sides_no, comboside = comboside) %>%
  filter(comboside > 365) %>%
  min(.$sides)

#Alternative code
side_choices <- function(x){
  6 * nrow(combinations(x, 2)) * 3
}

combos <- sapply(2:12, side_choices)

data.frame(sides = 2:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$sides)

#Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1
#How many groups are in the study?
#Each row contains one group of the experiment
nrow(esoph)
