## ---------------------------
##
## Script name: weather_data.R
##
## Purpose of script:  weather data temperature analysis - when are soils frozen?
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
## Notes:
##
##
## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(lubridate)
require(rio)

## ---------------------------

#load in raw data
r.data <- import("data/raw/precip.csv")
view(r.data)

DATE <- seq(as.Date("2007-10-01"), as.Date("2022-09-30"), by="days")
date.data <- as.data.frame(DATE)

################################################################################
# Compare estimated average vs observed average
temp.avg.data <- r.data %>%
  mutate(TAVG_EST = (TMAX+TMIN)/2) %>%
  drop_na(TAVG)

temp.avg.data %>%
  ggplot()+
  geom_point(aes(x = TAVG,y = TAVG_EST))+
  geom_abline(slope = 1, intercept = 0,color = "red")


slr <- lm(TAVG_EST ~ TAVG, data = temp.avg.data)
summary(slr)
################################################################################

t.data <- r.data %>%
  mutate(TAVG_EST = (TMAX+TMIN)/2,
         day = yday(DATE)) %>%
  group_by(day) %>%
  summarise(mean = median(TAVG_EST,na.rm = TRUE))



