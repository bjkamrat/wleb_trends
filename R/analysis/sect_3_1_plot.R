# find attributes for soil data to add to results section

# load in libraries
library(tidyverse)
library(rio)
library(ggplot2)


#load in data
data <- import("data/process/frozen_soil/frost_thaw_warm.csv")

# determine average frost in periods per water year
data %>%
  group_by(waterYear,season)%>%
  summarise(n = n()) %>%
  group_by(season) %>%
  summarise(mean = mean(n),
            min = min(n),
            max = max(n))

# determine frost in and out days
data %>%
  filter(frost == "frost_in")

data %>%
  filter(frost == "frost_out")


# plot frost days over time
data %>%
  ggplot()+
    geom_bar(aes(x = waterYear, fill = season))


