################################################################################
# Author: Brock KAmrath
# Date: 2 Oct 2023
# OBjective: Change point analysis on daily SRP concentrations
################################################################################

# Step 1: load in libraries
library(tidyverse)
library(rio)
library(xts)
library(changepoint)
library(lubridate)
yrs <- c(2001, 2002, 2002, 2002, 2003, 2005)
yr <- as.Date(as.character(yrs), format = "%Y")
y <- year(yr)


# Step 2: Read in data
r.data <- import("data/wrtds/output/eList_k/annual/srp/Maumee.csv")

# Step 3: clip to necessary data
data <- r.data %>%
  filter(DecYear >= 2009 & DecYear <= 2021) %>%
  mutate(Year = year(as.Date(as.character(DecYear), format = "%Y")))

# Step 4: convert to time series
ts.data <- ts(data$GenConc, order.by=as.Date(data$Date, "%Y-%m-%d"))

library(tsbox)
ts.data <- ts_ts(ts_long(data))

plot(ts.data)
cpt.mean(ts.data, method = "PELT",penalty = "CROPS", pen.value = c(1,25))
