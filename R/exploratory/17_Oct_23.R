###############################################################################
#  17_oct_23.R
#  Details: Read in weather data and determine snow melt days for future
#           investigation.
#
#
#############################################################################

# Load in libraries
library(rio)
library(tidyverse)
library(ggplot2)
library(dplyr)


# read in data
raw.df <- import("data/raw/precip.csv")

# plot data to ID differences in precipitation
raw.df %>%
  filter(STATION == "USW00014825") %>%
  ggplot(aes(x = DATE, y = SNWD, color = as.factor(STATION)))+
  geom_line()

# pivot data wider by station


# create days between 10/1/2007 and 9/30/2022
