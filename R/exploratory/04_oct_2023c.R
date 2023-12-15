################################################################################
# Author: Brock Kamrath
# Date: 2 Oct 2023
# OBjective: Change point analysis on daily SRP concentrations
################################################################################

# Step 1: load in libraries
library(tidyverse)
library(rio)
library(lubridate)

# Step 2: Read in data
r.data <- import("data/process/seasonal/combo/srp_conc_peryr.csv")

# Step 3: clip to necessary data
data <- r.data %>%
  filter(DecYear >= 2009 & DecYear <= 2021) %>%
  select(waterYear,Q,GenConc)

Qlow <- quantile(data$Q, 0.25)
Qhigh <- quantile(data$Q, 0.75)

data <- data %>%
  mutate(q_cond = ifelse(Q >= Qhigh,"High",ifelse(Q >= Qlow,"Moderate","Low")))


r.data %>%
  filter(Year >= 2009 & Year <= 2021) %>%
  ggplot(aes(x = Year,y = fn_conc, color = season))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = 2015)+
  facet_wrap(~station)+
  lims(y = c(0,0.35))+
  theme_bw()

