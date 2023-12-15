## ---------------------------
##
## Script name: clean_weather_data.R
##
## Purpose of script: clean download weather data for use in further analysis
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
require(data.table)
require(rio)

## ---------------------------

#load in raw data
r.data <- import("data/raw/precip.csv")
view(r.data)

DATE <- seq(as.Date("2007-10-01"), as.Date("2022-09-30"), by="days")
date.data <- as.data.frame(DATE)

################################################################################
# plot to see if all three stations have snowmelt data
r.data %>%
  ggplot(aes(x= DATE, y = SNOW, color = STATION))+
  geom_line()

# snow data only available for USC00332098 (i.e., defiance, OH)

# lets clip to just snowmelt data
snwd.data <- r.data %>%
  select(STATION,DATE,PRCP,SNOW,SNWD) %>%
  filter(STATION == "USC00332098"| STATION == "USW00094830") %>%
  #pivot_wider(names_from = STATION, values_from = SNWD)%>%
  mutate(DATE = as.Date(DATE))

snwd.data <- left_join(date.data,snwd.data, by = c("DATE"))

rain_on_snow <- snwd.data %>%
  filter(PRCP>0 & SNWD >0 & SNOW == 0)

# load in Tiffin
wq.data <- import("data/wrtds/output/eList_k/daily/tp/Maumee.csv")%>%
  mutate(period = ifelse(Date >= "2015-07-03","post","pre"),
         period = as.factor(period),
         Date = as.Date(wq.data$Date))

# dates with rain on snow
dates <- as.data.frame(unique(rain_on_snow$DATE))
colnames(dates) <- c("Date")


row_data <- left_join(dates,wq.data, by = "Date")

row_data %>%
  ggplot(aes(x = period, y = GenConc,color = period))+
  geom_boxplot()+
  geom_point()


row_data %>%
  ggplot(aes(x = log(Q), y = GenConc,color = period))+
  geom_point()+
  geom_smooth(method = "lm")


