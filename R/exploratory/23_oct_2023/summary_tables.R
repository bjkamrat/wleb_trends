## ---------------------------
##
## Script name: winter_analysis.R
##
## Purpose of script: This script will include a comparison of precip and Q, along with
##    an analysis of concentrations in the winter.
##
## Author: Dr. Brock Kamrath
##
## Date Created: 2023-10-20
##
## Copyright (c) Brock Kamrath, 2023
## Email: kamrath.brock@epa.gov
##
## ---------------------------
##
## Notes: This script will join the weather and water quality files and provide analysis
##
##
## ---------------------------

options(scipen = 6, digits = 2) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)

library(tidyverse)
library(data.table)
library(rio)
library(zoo)

## ---------------------------

# read in general data
gen_dat <- import("data/wrtds/input/gen_dat.xlsx")

# set pollutant of interest
poi <- "srp"

results <- NULL

for(i in 1:nrow(gen_dat)){

#read in water quality (wrtds_k) daily data
wq.data <- import(paste("data/wrtds/output/eList_k/daily/",poi,"/",gen_dat$station[i],".csv",sep = ""))

# low flow
q30 <- quantile(wq.data$Q, 0.3)

# moderate flow
q60 <- quantile(wq.data$Q, 0.6)

#high flow
q90 <- quantile(wq.data$Q, 0.9)

# Step 1: plot relationship between Q and Precipitation
data <- wq.data %>%
  mutate(period = ifelse(waterYear >= 2016,"post","pre"), # period
         #season = ifelse(Day > 335 | Day < 75, "winter","grow"),
         season = ifelse(Month == 12 |Month == 1| Month == 2 | Month == 3, "winter","grow"),
         fc = ifelse(Q >= q90, "v.high",ifelse(Q >= q60, "high", ifelse(Q>=q30,"moderate", "low"))))

# # See data in ln C vs. ln Q form
# data %>%
#   ggplot(aes(x = log(Q),y = log(GenConc), color = period))+
#   geom_point()+
#   geom_smooth(method = "lm")+
#   facet_wrap(~fc)
#
# #
# data %>%
#   ggplot(aes(x = GenConc, color = period))+
#   geom_histogram()+
#   facet_wrap(~fc)

results[[i]] <- data %>%
  group_by(period,season,fc) %>%
  summarise(mean = mean(GenConc,na.rm = TRUE),
            se = sd(GenConc, na.rm = TRUE)/sqrt(length((GenConc))),
            count = n())%>%
  pivot_wider(names_from= period, values_from = c(count,mean,se)) %>%
  mutate(per_change = ((mean_post - mean_pre)/mean_pre)*100,
         station = gen_dat$station[i])

}

# join lists together into a dataframe
res <- do.call(rbind.data.frame, results)
write.csv(res, "data/process/seasonal/mean_comp/per_change.csv",row.names = FALSE)

#plot data
res %>%
  ggplot(aes(x = fc, y = per_change,color = station))+
  geom_point()+
  facet_wrap(~season)

## set up source scripts
source("R/funcs/summarySE.R")

## summarise by mean and ci
df2 <- summarySE(data, measurevar = "GenConc",
                 groupvars=c("period", "season","fc"))


# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.3) # move them .05 to the left and right

# Use 95% confidence interval instead of SEM
p1 <- ggplot(df2, aes(x=fc, y=GenConc,color = period)) +
  geom_errorbar(aes(ymin=GenConc-ci, ymax=GenConc+ci), width=.1, position=pd) +
  # geom_smooth(se = FALSE) +
  geom_point(position=pd)+
  facet_wrap(~season,scales = "free")+
  labs(x = "Year" , y = "Q, cubic meters per second")+
  theme(text = element_text(size = 9))+
  lims(y = c(0,0.1))+
  theme_bw()

p1

# Run the regression
################################################################################
# Prepare data for  need to figure out the statistical analysis
#################################################################################
library(car)

# analysis for concentration
g1 <- glm(GenConc ~ Q + period + season + Q*period,
          family = Gamma(link = log),
          data = data)

summary(g1)

vif(g1)


data %>%
  mutate(yearmonth = zoo::as.yearmon(Date)) %>%
  group_by(fc,period,yearmonth) %>%
  summarise(fwmc = weighted.mean(GenConc,Q)) %>%
  ggplot(aes(yearmonth,fwmc,color = fc))+
  geom_point()+
  #geom_smooth()+
  geom_smooth(method = "lm")


data %>%
  group_by(period,fc) %>%
  summarise(mean_conc = mean(GenConc,na.rm = TRUE))



