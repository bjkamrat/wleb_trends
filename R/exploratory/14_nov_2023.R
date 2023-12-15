# ## ---------------------------
##
## Script name: temp_Q_conc.R
##
## Purpose of script: evaluate relationship between daily concentration, temperature, and flow
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
## Notes: This requires precip data and wq (wrtds-k) data
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
  ggplot(aes(x= DATE, y = TMAX, color = STATION))+
  geom_line()

# snow data only available for USC00332098 (i.e., defiance, OH)

# lets clip to just snowmelt data
tmp.data <- r.data %>%
  select(STATION,DATE,PRCP,TMIN,TMAX) %>%
  mutate(DATE = as.Date(DATE)) %>%
  group_by(DATE) %>%
  summarise(TMIN = mean(TMIN, na.rm = TRUE),
            TMAX = mean(TMAX,na.rm = TRUE),
            PRCP = mean(PRCP, na.rm = TRUE)) %>%
  rename("Date" = "DATE")


# load in Tiffin
wq.data <- import("data/wrtds/output/eList_k/daily/srp/Maumee.csv") %>%
  mutate(Date = as.Date(Date))


# low flow
q30 <- quantile(wq.data$Q, 0.33)

# moderate flow
q60 <- quantile(wq.data$Q, 0.66)

# #high flow
# q90 <- quantile(wq.data$Q, 0.9)

# Step 1: plot relationship between Q and Precipitation
data <- wq.data %>%
  mutate(period = ifelse(waterYear >= 2016,"post","pre"), # period
         #season = ifelse(Day > 335 | Day < 75, "winter","grow"),
         season = ifelse(Month == 12 |Month == 1| Month == 2 | Month == 3, "winter","grow"),
         fc = ifelse(Q >= q60, "high", ifelse(Q>=q30,"moderate", "low")))

data <- left_join(data,tmp.data, by=c("Date"))


data %>%
  mutate(melt = ifelse(TMAX >= 33,"non-freezing","freezing")) %>%
  ggplot(aes(x = period, y = GenConc))+
  geom_boxplot()+
  lims(y = c(0,0.2))+
  facet_wrap(~fc, ncol = 1)

data %>%
  mutate(melt = ifelse(TMAX >= 33,"non-freezing","freezing")) %>%
  ggplot(aes(x = fc, y = GenConc,color = period))+
  geom_boxplot()+
  geom_smooth(method = "lm")+
  facet_wrap(~season)


data %>%
  filter(waterYear == 2018) %>%
  mutate(melt = ifelse(TMAX >= 33,"non-freezing","freezing"),
         load = Q*GenConc*86.4) %>%
  ggplot()+
  geom_point(aes(x = Date, y = log(Q)))+
  geom_line(aes(x = Date, y = 10*PRCP), color = "blue")+
  geom_line(aes(x= Date, y = 100*GenConc), color = "red")+
  geom_line(aes(x= Date, y = GenFlux/1000), color = "green")
