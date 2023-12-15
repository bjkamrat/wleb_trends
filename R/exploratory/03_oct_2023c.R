################################################################################
# Author: Brock Kamrath
# Date: 2 Oct 2023
# OBjective: Change point analysis on daily SRP concentrations
################################################################################

# Step 1: load in libraries
library(tidyverse)
library(rio)
library(xts)
library(lubridate)

# Step 2: Read in data
r.data <- import("data/wrtds/output/eList_k/daily/srp/Tiffin.csv")

# Step 3: clip to necessary data
data <- r.data %>%
  filter(DecYear >= 2009 & DecYear <= 2021) %>%
  select(waterYear,Q,GenConc)

Qlow <- quantile(data$Q, 0.25)
Qhigh <- quantile(data$Q, 0.75)

data <- data %>%
  mutate(q_cond = ifelse(Q >= Qhigh,"High",ifelse(Q >= Qlow,"Moderate","Low")))


data %>%
  ggplot(aes(x = as.factor(waterYear),y = GenConc))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", color="red", fill="red") +
  geom_smooth(se=TRUE, aes(group=1))+
  facet_wrap(~q_cond)

