library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#Question 1
#Which of these code blocks return the latest year for which carbon emissions are reported?
temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

#not sure why this one does not work:
temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>%
  max(.$year)

#Question 2
#What is the first year for which carbon emissions (carbon_emissions) data are available?
temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  min()

#What is the last year for which carbon emissions data are available?
temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

#How many times larger were carbon emissions in the last year relative to the first year?
temp_carbon %>% 
  filter(!is.na(carbon_emissions), year %in% c(1751,2014)) %>%
  .$carbon_emissions

9855/3

#Question 3
#What is the first year for which global temperature anomaly (temp_anomaly) data are available?
temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>%
  .$year %>%
  min()

#What is the last year for which global temperature anomaly data are available?
temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>%
  .$year %>%
  max()

#How many degrees Celsius has temperature increased over the date range? Compare the temperatures in the most recent year versus the oldest year.
temp_carbon %>% 
  filter(!is.na(temp_anomaly), year %in% c(1880,2018)) %>%
  .$temp_anomaly

0.82+0.11

#Question 4
#Create a time series line plot of the temperature anomaly. Only include years where temperatures are reported. Save this plot to the object p.
#Which command adds a blue horizontal line indicating the 20th century mean temperature?
temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_hline(aes(yintercept=0), col = "blue")

#Question 5
#Continue working with p, the plot created in the previous question.
#Change the y-axis label to be "Temperature anomaly (degrees C)". Add a title, "Temperature anomaly relative to 20th century mean, 1880-2018". Also add a text layer to the plot: the x-coordinate should be 2000, the y-coordinate should be 0.05, the text should be "20th century mean", and the text color should be blue.
temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_hline(aes(yintercept=0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")


#Question 6
#When was the earliest year with a temperature above the 20th century mean?

#When was the last year with an average temperature below the 20th century mean?

#In what year did the temperature anomaly exceed 0.5 degrees Celsius for the first time?
temp_carbon %>% 
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_line(aes(y = ocean_anomaly), col = "red") +  # Land temperature anomaly
  geom_line(aes(y = land_anomaly), col = "orange") +  # Land temperature anomaly
  geom_hline(aes(yintercept=0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

temp_carbon %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_line(aes(year, land_anomaly), col = "red") +
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  xlim(c(1880, 2018)) +
  ggtitle("Temperature anomaly on land and ocean")

#Question 8
head(greenhouse_gases)

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(aes(xintercept=1850), col = "blue") +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#Question 10 
temp_carbon %>% 
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  geom_vline(aes(xintercept=1850), col = "blue")

temp_carbon %>% filter(year %in% c(1960,1970,2014)) %>%
  .$carbon_emissions
9855/2569
9855/4053

#Question 11 
historic_co2 %>%
  ggplot(aes(year, co2, col = source)) +
  geom_line() +
  ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
  ylab("co2 (ppmv)")

#Question 12 
historic_co2 %>%
  ggplot(aes(year, co2, col = source)) +
  geom_line() +
  xlim(c(-300, 2018)) +
  ggtitle("Atmospheric CO2 concentration, -800,000 BC to today") +
  ylab("co2 (ppmv)") +
  geom_hline(aes(yintercept=275), col = "blue") +
  geom_hline(aes(yintercept=400), col = "blue")

