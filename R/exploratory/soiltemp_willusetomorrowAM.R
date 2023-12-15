# I am going to try to determine when the soil is frozen at the soil surface using
# air temperature, soil temperature at 5 cm depth, and soil temperature at 10 cm depth


# load in libraries
library(tidyverse)
library(rio)
library(lubridate)
library(zoo)

library(RmarineHeatWaves)



# read in weaterh data from OHio State University north west station located north of Hoytville, Ohio
r.data <- import("data/raw/weatherdata_nw.csv") %>%
  mutate(Date = as.Date(Date,format = "%m/%d/%Y"))

r.data %>%
  filter(MaxAirTemp <= 32) %>%
  ggplot()+
  geom_point(aes(x = MaxAirTemp,y = MinSoilTemp2), color = "blue")+
  geom_point(aes(x = MaxAirTemp,y = AvgSoilTemp4), color = "red")+
  geom_hline(yintercept = 32)


r.data %>%
  mutate(diff = AvgSoilTemp2 - AvgSoilTemp4,
         Date = as.Date(Date,format = "%m/%d/%Y"),
         month = month(Date))%>%
  group_by(month) %>%
  summarise(mean = mean(diff,na.rm = TRUE))


r.data %>%
  filter(AvgSoilTemp2 < 33) %>%
  ggplot()+
  geom_point(aes(x=AvgSoilTemp2, y = MinAirTemp))

date.data <- as.data.frame(seq(ymd('2007-10-01'),ymd('2022-09-30'), by = 'day'))
colnames(date.data) <- c("Date")

r.data <- left_join(date.data,r.data,by = c("Date"))

# linearly interpolate the missing soil temperature values and determine frost periods
# create a new "filled" dataframe (f.data)
f.data <- r.data %>%
  select(Date,MaxAirTemp,MinAirTemp,AvgSoilTemp2,MinSoilTemp2, AvgSoilTemp4) %>%
  mutate(approx_avg = na.approx(AvgSoilTemp2),
         approx_min = na.approx(MinSoilTemp2))

# write this to a data file for manual editing
write.csv(f.data, "data/process/frost_data.csv",row.names = FALSE)

# this was edited by hand into frost_data_final.csv

################################################################################

# view a summary of the data to ensure NA values from AvgSoilTemp2 have been removed
summary(f.data)

# create a 7 day rolling average of approx (i.e., 2" soil temps)
f.data <- f.data %>%
  dplyr::mutate(temp_07da = zoo::rollmean(approx, k = 7, fill = NA, align = "left"),
                temp_15da = zoo::rollmean(approx, k = 15, fill = NA, align = "right"),
                frost = ifelse(temp_07da <= 34, 1,-1))


in.list <- exceedance(f.data, x = Date, y = approx, threshold = 34, below = FALSE,
           min_duration = 7, join_across_gaps = TRUE, max_gap = 5,
           max_pad_length = 3)

in.thresh.data <- in.list[[1]]
in.det.data <- in.list[[2]]

# load in Tiffin River data
wq.data <- import("data/wrtds/output/eList_k/daily/srp/Tiffin.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Julian, Month, Day, waterYear, Q, GenConc, GenFlux)

# join wq and weather data
f.data <- left_join(f.data,wq.data, by = c("Date"))

# create a plot of Q relative to frost and thaw
f.data %>%
  ggplot()+
  geom_point(aes(x = Day, y = Q, color = frost))+
  facet_wrap(~waterYear)
