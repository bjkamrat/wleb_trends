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

## ---------------------------

# read in weather data
w.data <-  import("data/process/defiance_weather.csv") %>%
  rename("Date" = "DATE")

#read in water quality (wrtds_k) daily data
wq.data <- import("data/wrtds/output/eList_k/daily/srp/Maumee.csv")

# join dataframes
data <- left_join(wq.data,w.data, by = c("Date"))

#write.csv(data, file = "data/process/blanch_wq_weather.csv", row.names = FALSE)

# low flow
q30 <- quantile(data$Q, 0.3)

# moderate flow
q60 <- quantile(data$Q, 0.6)

#high flow
q90 <- quantile(data$Q, 0.9)


# Step 1: plot relationship between Q and Precipitation
data <- data %>%
  mutate(period = ifelse(waterYear >= 2016,"post","pre"), # period
         fc = ifelse(Q >= q90, "v.high",ifelse(Q >= q60, "high", ifelse(Q>=q30,"moderate", "low"))),
         rainfall = ifelse(PRCP >= 0.5,"above","below"), # rainfall
         frozen_soils = ifelse(TMAX <= 35, "F","NF"),
         delta_snow = SNWD)

for(i in 2:nrow(data)){
  data$delta_snow[i] <- data$SNWD[i] - data$SNWD[i-1]
}

################################################################################
# check frozen soils
data %>%
  ggplot(aes(Day, TMIN))+
  geom_point()+
  geom_smooth()+
  geom_hline(yintercept = 28)+
  geom_vline(xintercept = 75)+
  geom_vline(xintercept = 331)


data %>%
  group_by(Day) %>%
  summarise(tmin = median(TMIN,na.rm = TRUE),
            tmax = median(TMAX, na.rm = TRUE)) %>%
  ggplot()+
  geom_line(aes(Day, tmin))+
  geom_line(aes(Day, tmax))+
  geom_hline(yintercept = 28)+
  geom_vline(xintercept = 60)+
  geom_vline(xintercept = 336)

################################################################################
# when does snowmelt/snow occur?
#snowmelt
data %>%
  mutate(snowmelt = ifelse(delta_snow < 0, "yes","no")) %>%
  filter(snowmelt == "yes") %>%
  ggplot()+
  geom_histogram(aes(Day))+
  geom_vline(xintercept = 75)+
  geom_vline(xintercept = 320) +
  facet_wrap(~period)


#snow
data %>%
  mutate(snowday = ifelse(SNWD > 0, 1,0)) %>%
  group_by(Day) %>%
  summarise(sum = sum(snowday,na.rm = TRUE)) %>%
  ggplot()+
    geom_line(aes(x = Day, y = sum))+
    geom_hline(aes(yintercept = 6))+
    geom_vline(xintercept = 58)+
    geom_vline(xintercept = 351)+
  labs(x = "Day", y = "Number of years with snow-covered soils")

# average snow depth on days
data %>%
  group_by(Day) %>%
  summarise(sum = mean(SNWD,na.rm = TRUE)) %>%
  ggplot()+
  geom_line(aes(x = Day, y = sum))+
  geom_vline(xintercept = 75)+
  geom_vline(xintercept = 335)


# how does snow match flow?
data %>%
  mutate(snowmelt = ifelse(delta_snow < 0, "yes","no")) %>%
  #filter(Month  == 12 | Month == 1 | Month == 2 | Month == 3) %>%
  filter(PRCP > 0 | snowmelt == "yes") %>%
  ggplot(aes(period,log(GenConc), fill = period))+
  geom_boxplot()+
  facet_wrap(~fc)


data %>%
  filter(Month  == 12 | Month == 1 | Month == 2 | Month == 3) %>%
  ggplot(aes(period,log(GenConc), fill = period))+
  geom_boxplot()+
  facet_wrap(~fc)



data %>%
  filter(delta_snow < 0) %>%
  ggplot()+
  geom_boxplot(aes(period,GenConc))+
  facet_wrap(~fc)


data %>%
  group_by(period,fc) %>%
  summarise(mean_conc = mean(GenConc,na.rm = TRUE))



