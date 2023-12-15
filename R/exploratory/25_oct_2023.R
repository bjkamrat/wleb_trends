## ---------------------------
##
## Script name: winter_analysis.R
##
## Purpose of script: This script will include a comparison of precip and Q, along with
##    an analysis of concentrations in the winter.
##
## Author: Dr. Brock Kamrath
##
## Date Created: 2023-10-20
##
## Copyright (c) Brock Kamrath, 2023
## Email: kamrath.brock@epa.gov
##
## ---------------------------
##
## Notes: This script will join the weather and water quality files and provide analysis
##
##
## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

library(tidyverse)
library(data.table)
library(rio)
library(lubridate)

## ---------------------------

#load in raw data
r.data <- import("data/raw/precip.csv")
# view(r.data)

# clip to only TMIN data and add julian date
temp.data <- r.data %>%
  select(STATION,DATE,TMIN,SNWD) %>% # clip to station, date and minimum temperature
  mutate(month = month(DATE),
         jdate = yday(DATE))

sum.data <- temp.data %>%
  group_by(jdate) %>%
  summarise(median = median(TMIN,na.rm = TRUE))

# plot TMIN data
temp.data %>%
  ggplot(aes(x = jdate, y = TMIN))+
  geom_point(color = "red")+
  geom_smooth()+
  geom_hline(yintercept = 24)+
  geom_vline(xintercept = 60)+
  geom_vline(xintercept = 355)

# plot TMIN data
sum.data %>%
  ggplot(aes(x = jdate, y = median))+
  geom_line()+
  geom_hline(yintercept = 24)+
  geom_vline(xintercept = 66)+
  geom_vline(xintercept = 341)


# lets try mean monthly temperature and see where that takes us.
sum.data <- temp.data %>%
  group_by(month) %>%
  summarise(mean = mean(TMIN,na.rm = TRUE))

################################################################################
# What about snow

temp.data %>%
