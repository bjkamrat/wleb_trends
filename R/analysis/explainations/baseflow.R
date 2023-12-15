# Author: Brock Kamrath
# Date: 8 Aug 2023
# Objective: Determine baseflow and determine monthly

# Libraries
library(tidyverse)
library(ggplot2)
library(rio)
library(FlowScreen)
library(lubridate)


# set poi and station
poi <- "srp"
station <- "Maumee"

# # read in general data
# g_data <- read_excel("data/wrtds_input/gen_dat.xlsx")

month.abb
wy.month.abb <- c("Oct", "Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")

# df <- data %>%
#   mutate(wtr_yrVAR=factor(wtr_yr(Date))) %>%
#   #seq along dates starting with the beginning of your water year
#   mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
#                               "-", month(Date), "-", day(Date))))
#
# df %>%
#   ggplot(., aes(x = CDate, y = GenConc, colour = wtr_yrVAR)) +
#   geom_point() +
#   geom_smooth(se=FALSE) +
#   scale_x_date(date_labels = "%b %d")


#import dataset
data <- import(paste("data/wrtds_output/eList_k/daily/",poi,"/",station,".csv",sep = "")) %>%
  mutate(baseQ = bf_eckhardt(Q,0.84,0.8),
         ratio = baseQ/Q,
         year = floor(DecYear)) %>%
  rename("month" = "Month")

# Separate into a baseflow file, summarise by month
b_data <- data %>%
  filter(ratio < 0.9) %>%
  group_by(year,month) %>%
  summarise(mc = mean(GenConc,na.rm = TRUE)) %>%
  drop_na() %>%
  mutate(yearmon = zoo::as.yearmon(paste(year, " ", month), "%Y %m")) #%>%
  filter(year >= 2015)

b_data %>%
  ggplot(aes(x = yearmon, y = mc))+
  geom_point()+
  geom_smooth(method = "lm")


b_data %>%
  ggplot(aes(x = year, y = mc))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~month)


slr_all <- lm(mc ~ yearmon,data = b_data)
summary(slr_all)


# check baseflow load vs. total load
b_data <- data %>%
  filter(ratio > 0.9) %>%
  group_by(waterYear) %>%
  summarise(baseload = sum(Q*GenConc*86.4))

t_data <- data %>%
  group_by(waterYear) %>%
  summarise(totalload = sum(Q*GenConc*86.4))

c_data <- left_join(b_data,t_data, by= c("waterYear"))

c_data %>%
  mutate(ratio = baseload/totalload)

