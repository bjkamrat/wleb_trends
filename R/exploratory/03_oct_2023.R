################################################################################
# Author: Brock KAmrath
# Date: 2 Oct 2023
# OBjective: Change point analysis on daily SRP concentrations
################################################################################

# Step 1: load in libraries
library(tidyverse)
library(rio)
library(xts)
library(lubridate)

library(readxl)
d.data <- read_excel("data/raw/srp/2023_08_04_data_download.xlsx",
                                        sheet = "Maumee_samples")

colnames(d.data) <- c("Date","qaul_q","Q","qual_srp","srp")

# Step 2: Read in data
r.data <- import("data/wrtds/output/eList_k/daily/srp/Maumee.csv")
r.data.inputs <- import("data/wrtds/input/srp/Maumee.csv")



# Step 3: clip to necessary data
data <- d.data %>%
  #filter(DecYear >= 2009 & DecYear <= 2021) %>%
  select(Date,Q,srp) %>%
  mutate(period = ifelse(Date >= "2015-07-01","post","pre"),
         Month = month(Date),
         Year = year(Date))


data %>%
  filter(Month == 9) %>%
  ggplot(aes(x = log(Q),y = log(srp)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~period)
