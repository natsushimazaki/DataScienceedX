library(tidyverse)
library(dslabs)
library(ggplot2)
library(ggrepel)
library(ggthemes)
data(stars)
options(digits = 3)   # report 3 significant digits

#Question 1 
#What is the mean magnitude?
summary(stars$magnitude)

#What is the standard deviation of magnitude?
sd(stars$magnitude)

#Question 2 
#Make a density plot of the magnitude. How many peaks are there in the data?
stars %>% filter(!is.na(magnitude)) %>% ggplot(aes(magnitude)) +
  geom_density(alpha = 0.2, bw = 2, position = "stack")

#Question 3
#Examine the distribution of star temperature. Which of these statements best characterizes the temperature distribution?
stars %>% filter(!is.na(temp)) %>% ggplot(aes(temp)) +
  geom_histogram() 
  #Ans. the majority of stars have a low temperature

#Question 4
#Make a scatter plot of the data with temperature on the x-axis and magnitude on the y-axis and examine the relationship between the variables. Recall that lower magnitude means a more luminous (brighter) star.
stars %>% filter(!is.na(temp)) %>% ggplot(aes(temp, magnitude)) +
  geom_point()

#Question 5 
#For various reasons, scientists do not always follow straight conventions when making plots, and astronomers usually transform values of star luminosity and temperature before plotting. Flip the y-axis so that lower values of magnitude are at the top of the axis (recall that more luminous stars have lower magnitude) using scale_y_reverse(). Take the log base 10 of temperature and then also flip the x-axis.
stars %>% filter(!is.na(temp)) %>% 
  ggplot(aes(temp, magnitude, label = star)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous(trans = "log10") +
  scale_x_reverse() +
  geom_label()

#Question 6 
#How many white dwarfs are there in our sample?
#These stars tend to be hotter stars, but also have low luminosity, and are known as white dwarfs.

#Question 7 
#Consider stars which are not part of the Main Group but are not old/evolved (white dwarf) stars. These stars must also be unique in certain ways and are known as giants. Use the plot from Question 5 to estimate the average temperature of a giant.
#Which of these temperatures is closest to the average temperature of a giant?:

#Question 9
#Remove the text labels and color the points by star type. This classification describes the properties of the star's spectrum, the amount of light produced at various wavelengths.
stars %>% filter(!is.na(temp)) %>% 
  ggplot(aes(temp, magnitude, color = type)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous(trans = "log10") +
  scale_x_reverse() +
  scale_colour_manual(values = c("#000000", "#AAAAAA", "#0022BB", "#22BB00", "#CCCCCC", "#CC00CC", "#CCCC00","#56B4E9", "#009E73", "#F0E442"))

