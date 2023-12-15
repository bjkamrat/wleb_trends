# I am going to try to determine when the soil is frozen at the soil surface using
# air temperature, soil temperature at 5 cm depth, and soil temperature at 10 cm depth


# load in libraries
library(tidyverse)
library(rio)
library(lubridate)
library(zoo)
library(data.table)



# read in weather data from Ohio State University north west station located northest of Hoytville, Ohio
r.data <- import("data/process/frost_data_final.xlsx") %>%
  mutate(Date = as.Date(Date,format = "%m/%d/%Y"))

dpy <- r.data%>%
  mutate(year = lubridate::year(Date)) %>%
  group_by(year,frost) %>%
  summarise(n = n())

# load in Maumee River data
wq.data <- import("data/wrtds/output/eList_k/daily/srp/Maumee.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Julian, Month, Day, waterYear, Q, GenConc, GenFlux)

# join data
j.data <- left_join(r.data,wq.data,by = c("Date"))

# with 5 day breaks the thaw period is -5 through 19 days
# create breaks in
# agebreaks <- c(-25,-20,-15,-10,-5,0,5,10,15,20,25,30,35)
# agelabels <- c("1","2","3","4","5","6","7","8","9","10","11","12")
#
# setDT(j.data)[ , groups := cut(frost_out_days,
#                                 breaks = agebreaks,
#                                 right = FALSE,
#                                 labels = agelabels)]

# with 10 day breaks the thaw period is -5 through day 24 or -10 through day 20
agebreaks <- c(-45,-30,-20,-10,0,10,20,30,45)
agelabels <- c("1","2","3","4","5","6","7","8")

setDT(j.data)[ , groups := cut(frost_out_days,
                               breaks = agebreaks,
                               right = FALSE,
                               labels = agelabels)]

# from this we chose a negative thaw period from -5 days from frost out to 20 days
# after frost_in


j.data %>%
  group_by(groups) %>%
  summarize(mean = mean(Q))

j.data %>%
  ggplot()+
  geom_point(aes(x = frost_out_days,y = Q))+
  geom_smooth(aes(x = frost_out_days,y = Q))+
  geom_vline(xintercept = 20)+
  geom_vline(xintercept = -5)+
  labs(x = "Days before and after frost out", y = "Avg Daily Q, in cms")+
  theme_bw()
