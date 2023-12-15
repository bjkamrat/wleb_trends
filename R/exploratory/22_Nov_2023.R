# Script Objective: Baseflow separation using the USGS Hydrograph Separation Program (HYSEP) using the
# R package DV stats

source("R/funcs/baseflow_HYSEP.R")

# load in libraries
library(tidyverse)
library(rio)
library(ggplot2)


# load in flow data for a trial site
q.data <- import("data/wrtds/output/elist_k/daily/srp/Maumee.csv")

bf_Q <- baseflow_HYSEP(q.data$Q,area_mi2 = 6330,method = "fixed")

q.data$bf_Q <- bf_Q

q.data <- q.data %>%
  mutate(ratio = bf_Q/Q,
         sf_Q = (Q-bf_Q)*86400,
         bf_Q = bf_Q*86400,
         bf_flux = (bf_Q*GenConc)/1000,
         sf_flux = (sf_Q*GenConc)/1000,
         gen_flux = bf_flux+sf_flux)


q.data %>%
  mutate(fc = ifelse(ratio > 0.80, "base","storm")) %>%
  ggplot()+
  geom_line(aes(x = Day, y = GenFlux, color = fc))+
  facet_wrap(~waterYear)


q.data %>%
  mutate(fc = ifelse(ratio >0.8, "storm","base"),
         period = ifelse(Date >= "2015-07-03","Post","Pre"))%>%
  group_by(period,fc,Month) %>%
  summarise(mean = mean(GenConc)) %>%
  ggplot()+
  geom_point(aes(x = Month, y = mean, color = period))+
  geom_line(aes(x = Month, y = mean, color = period))+
  facet_wrap(~fc)


q.data %>%
  mutate(fc = ifelse(ratio >0.9, "storm","base"),
         period = ifelse(Date >= "2015-07-03","Post","Pre"))%>%
  group_by(period,fc) %>%
  summarize(count = n())


