library(rio)
library(tidyverse)
library(ggplot2)
library(dplyr)
library("RColorBrewer")

raw_data <- import("data/wrtds_output/eList_k/daily/tp/Tiffin.csv") %>%
  mutate(logConc = log(GenConc),
         logQ = log(Q),
         waterYear = as.integer(waterYear))

q90 <- quantile(raw_data$Q, 0.90)

raw_data %>%
  mutate(period = ifelse(waterYear >= 2015, "post", "pre")) %>%
  #filter(Month == 12 | Month == 1 | Month == 2) %>%
  filter(Q >= q90) %>%
  #filter(waterYear <= 2015) %>%
  ggplot(aes(x = logQ, y = logConc))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~period)


raw_data %>%
  mutate(period = ifelse(waterYear >= 2015, "post", "pre")) %>%
  #filter(Month == 12 | Month == 1 | Month == 2) %>%
  filter(Q >= q90) %>%
  #filter(waterYear <= 2015) %>%
  ggplot(aes(GenConc))+
  geom_histogram()+
  facet_wrap(~period)

# Average SRP concentration

mean_summary <-  raw_data %>%
  mutate(period = ifelse(waterYear >= 2015, "post", "pre")) %>%
  filter(Q >= q90) %>%
  group_by(period) %>%
  summarise(mean_conc = mean(GenConc))
