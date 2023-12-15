################################################################################
# Author: Brock Kamrath
# Date: 27 Sep 2023
# Objective: Plot seasonal data
################################################################################

# load in libraries
library(tidyverse)
library(rio)
library(glue)
library(ggtext)

# load in data
r.data <- import("data/process/seasonal/combo/all_seasonal.csv")


# initial plot

r.data %>%
  filter(scenario != "all" & type == "flux") %>%
  ggplot(aes(x = station, y = unit_slope,color= scenario))+
  geom_point()+
  facet_grid(poi~season,scales = "free")

# create barbell plot for the winter values of srp
data <- r.data %>%
  filter(scenario != "all" & type == "flux" & season == "winter") %>%
  select(station, per_change, poi,scenario) %>%
  mutate(per_change = round(per_change,3)) %>%
  pivot_wider(names_from = scenario, values_from = per_change) %>%
  rename(percent_pre = pre,
         percent_post = post) %>%
  mutate(bump_pre = case_when(percent_pre < percent_post ~
                            percent_pre - 5,
                            percent_pre > percent_post ~
                            percent_pre + 5,
                            TRUE ~ NA_real_),
         bump_post = case_when(percent_pre < percent_post ~
                              percent_post + 5,
                               percent_pre > percent_post ~
                              percent_post - 5,
                               TRUE ~ NA_real_))


data %>%
  pivot_longer(cols = -c(station,poi), names_to=c(".value", "scenario"),
               names_sep = "_") %>%
  drop_na() %>%
  ggplot(aes(x = percent, y = station, color = scenario))+
    geom_line(color = "black",size=1, show.legend = FALSE)+
    geom_point()+
    geom_text(aes(label = glue("{percent}%"), x= bump))+
    facet_wrap(~poi)


ggsave("results/figs/seasonal_intial_plot_per_slope.tiff", width = 8, height = 4)
