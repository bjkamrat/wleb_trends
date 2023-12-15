library(tidyverse)
library(rio)
library(ggplot2)

data <- import("data/results/seasonal/combo/all_seasonal.csv") %>%
  filter(scenario != "all")


data %>%
  #filter(type == "conc") %>%
  filter(type == "flux") %>%
  ggplot(aes(x = reorder(scenario,desc(per_slope)), y = per_slope, color = station,group = station)) +
  geom_point(aes(shape = station))+
  geom_line()+
  facet_grid(poi~season, scales = "free")+
  labs(x = "scenario", y = "Rate of percent change over period")+
  theme_bw()

data %>%
  filter(poi == "srp") %>%
  ggplot(aes(x = reorder(type,desc(per_slope)), y = per_slope, color = season,group = season)) +
  geom_point(aes(shape = season))+
  geom_line()+
  facet_grid(scenario~station, scales = "free")


# all results
data <- import("data/results/seasonal/combo/all_seasonal.csv") %>%
  filter(scenario == "all")

theme_update(text = element_text(size=24))

data %>%
  #filter(type == "conc") %>%
  filter(type == "flux" & poi =="tp") %>%
  ggplot(aes(x = reorder(season,desc(per_slope)), y = per_slope, color = station,group = station)) +
  geom_point(aes(shape = station))+
  geom_line()+
  labs(x = "scenario", y = "Rate of percent change over period")+
  theme_bw()
