# Objective: I want to investigate the Hysteresis and Flushing indices
# of SRP in the Sandusky River.

# Date: November 2, 2023

# Author: Brock Kamrath
################################################################################

# First I need to load in my libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(rio)
library(FlowScreen)
library(lubridate)
library(grwat)
library(dataRetrieval)

# Create monthly abbreviations if necessary
month.abb
wy.month.abb <- c("Oct", "Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")

#import wrtds_kalman daily values for Sandusky
wrtds <- import("data/wrtds/output/eList_k/daily/srp/sandusky.csv")


#############################################################

#import raw data from NCWQR for Sandusky River and SRP
r.data <- read_excel("data/raw/srp/2023_10_26_data_download.xlsx",
                     sheet = "Sandusky_samples")

# rename columns for easier use
colnames(r.data) <- c("datetime","q_qual","q_cfs","c_qual","c_mgl")


# start creating filters for
r.data %>%
  mutate(year = year(datetime),
         slope = diff(q_cfs)/diff(datetime))%>%
  filter(year == 2017) %>%
  ggplot(aes(datetime,q_cfs))+
  #geom_point()+
  geom_line()


r.data %>%
  select(datetime,q_cfs,c_mgl) %>%

  mutate(baseQ = bf_eckhardt(Q,0.84,0.8),
         ratio = baseQ/Q,
         year = floor(DecYear)) %>%
  rename("month" = "Month")

# Separate into a baseflow file, summarise by month
b_data <- data %>%
  filter(ratio < 0.9) %>%
  group_by(year,month) %>%
  summarise(mc = mean(GenConc,na.rm = TRUE)) %>%
  drop_na() %>%
  mutate(yearmon = zoo::as.yearmon(paste(year, " ", month), "%Y %m")) #%>%
filter(year >= 2015)

b_data %>%
  ggplot(aes(x = yearmon, y = mc))+
  geom_point()+
  geom_smooth(method = "lm")
