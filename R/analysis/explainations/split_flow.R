# Author: Brock Kamrath
# Date: 8 Aug 2023
# Objective: Determine baseflow and determine monthly

# Libraries
library(tidyverse)
library(ggplot2)
library(rio)
library(FlowScreen)
library(lubridate)


# set poi and station
poi <- "srp"
station <- "Maumee"

# # read in general data
# g_data <- read_excel("data/wrtds_input/gen_dat.xlsx")

wy.month.abb <- c("Oct", "Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")


#import dataset
#low flow dataset
lf_data <- import(paste("data/wrtds_output/eList_k/daily/",poi,"/",station,".csv",sep = "")) %>%
  mutate(q33 = quantile(Q,0.33)) %>%
  filter(Q < q33) %>%
  group_by(waterYear) %>%
  summarise(mc = mean(GenConc))

#moderate flow dataset
mf_data <- import(paste("data/wrtds_output/eList_k/daily/",poi,"/",station,".csv",sep = "")) %>%
  mutate(q33 = quantile(Q,0.33),
         q66 = quantile(Q,0.66)) %>%
  filter(Q < q66 & Q >= q33) %>%
  group_by(waterYear) %>%
  summarise(mc = mean(GenConc))

#high flow dataset
hf_data <- import(paste("data/wrtds_output/eList_k/daily/",poi,"/",station,".csv",sep = "")) %>%
  mutate(q66 = quantile(Q,0.66)) %>%
  filter(Q >= q66) %>%
  group_by(waterYear) %>%
  summarise(mc = mean(GenConc))

#extreme events dataset
ee_data <- import(paste("data/wrtds_output/eList_k/daily/",poi,"/",station,".csv",sep = "")) %>%
  mutate(c90 = quantile(GenConc,0.90)) %>%
  filter(GenConc > c90) %>%
  count(waterYear)

