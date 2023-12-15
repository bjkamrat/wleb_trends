# Author: Brock Kamrath
# Date: 8 Aug 2023
# Objective: Plot changes in seasonal trends

# Libraries
library(tidyverse)
library(ggplot2)
library(rio)


#import dataset
data <- import("data/wrtds_output/trends/seasonal.csv")


data %>%
  rename("Concentration" = "ChangeperConcRate",
         "Flux" = "ChangeperFluxRate") %>%
  filter(season != "annual") %>%
  pivot_longer(cols = c(3,4),names_to= "type", values_to = "change_rate") %>%
  ggplot()+
  geom_point(aes(x = reorder(station,change_rate), y = change_rate,color = type,shape = type))+
  geom_hline(yintercept = 0, color = "black")+
  theme_classic()+
  facet_wrap(~season)+
  labs(x = "Watershed", y =  "Change in annualized rates of change (%/yr)")
