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
  select(STATION,DATE,SNWD) %>%
  filter(STATION == "USC00332098"| STATION == "USW00094830") %>%
  #pivot_wider(names_from = STATION, values_from = SNWD)%>%
  mutate(DATE = as.Date(DATE))

snwd.data <- left_join(date.data,snwd.data, by = c("DATE"))

#plot general snow depth
snwd.data %>%
  mutate(day = yday(DATE),
         snow = ifelse(SNWD>0,1,0)) %>%
  group_by(day,STATION) %>%
  summarise(snow = sum(snow, na.rm = TRUE)) %>%
  ggplot(aes(x= day, y = snow,color = STATION))+
  geom_line()+
  geom_vline(xintercept = 65)+
  geom_vline(xintercept = 340)+
  geom_hline(yintercept = 4)

#find na values
snwd.data[is.na(snwd.data$SNWD), ]

# save as csv (SNOW)
write.csv(snwd.data, file = "data/process/snow_depth.csv", row.names = FALSE)


################################################################################

# what about precipitation data
r.data %>%
  ggplot(aes(x= DATE, y = PRCP))+
  geom_line()+
  facet_wrap(~STATION)

# station USC00338534 doesn't have precip or snow data after 2014, remove from analysis
r.data <- r.data %>%
  filter(STATION != "USC00338534")

################################################################################

# check on temperature data - both have temperature data.
r.data %>%
  ggplot(aes(x= DATE, y = TMAX,color = STATION))+
  geom_line()

# look at min and max
r.data %>%
  ggplot()+
  geom_line(aes(x= DATE, y = TMAX),color = "red")+
  geom_smooth(aes(x= DATE, y = TMAX))+
  geom_line(aes(x= DATE, y = TMIN),color = "blue")+
  geom_smooth(aes(x= DATE, y = TMIN))

################################################################################
# lets save defiance, oh data
# lets clip to just snowmelt data
def.data <- r.data %>%
  select(STATION,DATE,PRCP,SNWD,TMAX,TMIN) %>%
  filter(STATION == "USC00332098") %>%
  mutate(DATE = as.Date(DATE))

def.data <- left_join(date.data,def.data, by = c("DATE"))

# save as csv (SNOW)
write.csv(def.data, file = "data/process/defiance_weather.csv", row.names = FALSE)

