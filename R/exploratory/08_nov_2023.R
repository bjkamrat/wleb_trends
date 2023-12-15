################################################################################
# Title: 8_Nov_23.R
# Author: Brock Kamrath
# Date: 8 Nov 2023
# Time: AM deep work session
# Objective: Convert Maumee long-term sub daily data into daily fwmc data, then join
#            daily flow data to calculate the long-term cumulative load data.
###############################################################################

# Step 1: Load in libraries
library(tidyverse)
library(rio)
library(readxl)

source("R/funcs/water_year.R")

# read in general data for maumee

# Step 2: Read in raw wq data (raw)
raw <- read_excel("data/raw/old_downloads/maumee_srp.xlsx")

# convert column names in raw data
colnames(raw) <- c("Datetime","rq","q_cfs","rconc","conc_mgL")

# Step 3: create full daily dataset for fwmc

# convert raw data to daily data using flow weighted mean concentrations
fwmc <- raw %>%
  mutate(date = as.Date(Datetime)) %>%
  filter(date >= "1982-10-01" & date <= "2022-09-30")%>%
  group_by(date) %>%
  summarise(conc = weighted.mean(conc_mgL,q_cfs,na.rm = TRUE)) %>%
  mutate(remark = if_else(conc <= 0.0001, "<","")) %>%
  filter(conc >= 0.0001) %>%
  select(date,remark,conc) %>%
  na.omit(conc)

write.csv(fwmc, file = "data/wrtds/input/long_term_maumee.csv",row.names = FALSE)
# now this dataset can be run through wrtds and wrtds K to produce daily estimates that can be
# input into this code down in later lines.

# Step 4: create sequence of days between dates to join fwmc to in order to determine
# days with data per year

daily <- as.data.frame(seq(as.Date("1982-10-01"), as.Date("2022-09-30"), by = "days"))
colnames(daily) <- c("date")
daily$wy <-water_year(daily$date)

# join the two dataframes
fwmc <- left_join(daily,fwmc, by = "date")

# determine number of missing values per year
missing_data <- fwmc %>%
  group_by(wy) %>%
  summarise(sum_na = sum(is.na(conc)))%>%
  mutate(use = ifelse(sum_na > 55, "no","yes"))

# Load in daily wrtds-k data after running run_wrtds.R for the initial eList and then
# run_wrtds_k.R to calculate the daily estimates.
# Cacluate daily load and then cumulative daily load

daily_wrtdsk <- import("data/wrtds/output/long_term/maumee_daily.csv") %>%
  mutate(csum = (cumsum(GenFlux)/1638800),
         FNcsum = (cumsum(FNFlux)/1638800),
         csum_q = (cumsum(Q*86400)/1638800),
         csum_c = (cumsum(GenConc)))

# plot the cumulative load time series for Maumee with break points at 1984,
# 2002, and 2015

daily_wrtdsk %>%
  ggplot(aes(x = Date, y = csum))+
  geom_point()+
  geom_line()+
  geom_vline(xintercept = as.Date("1984-10-01"))+
  geom_vline(xintercept = as.Date("2002-10-01"))+
  geom_vline(xintercept = as.Date("2015-07-03"))

# what if I calculate the slope of the line
rollingSlope.lm.fit <- function(vector) {
  a <- coef(.lm.fit(cbind(1, seq(vector)), vector))[2]
  return(a)
}

library(zoo, warn.conflicts = FALSE)
daily_wrtdsk$seven_day_slope <- rollapply(daily_wrtdsk$csum, width=500, FUN=rollingSlope.lm.fit,fill=NA, align = "right")

daily_wrtdsk %>%
  ggplot(aes(x = Date, y = seven_day_slope))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept = as.Date("1984-10-01"))+
  geom_vline(xintercept = as.Date("2002-10-01"))+
  geom_vline(xintercept = as.Date("2015-07-03"))
