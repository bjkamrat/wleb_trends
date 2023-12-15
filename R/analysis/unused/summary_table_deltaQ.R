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

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation
memory.limit(30000000)

## ---------------------------

## load up the packages we will need:  (uncomment as required)
library(plyr)
library(tidyverse)
library(rio)

## ---------------------------

## set up source scripts
source("R/funcs/summarySE.R")

# read in general data
gen_dat <- import("data/wrtds/input/gen_dat.xlsx")

wq.data <- NULL

for(i in 1:nrow(gen_dat)){

#read in water quality (wrtds_k) daily data
r.data <- import(paste("data/wrtds/output/eList_k/daily/srp/",gen_dat$station[i],".csv",sep = ""))
wq.data[[i]] <- r.data%>%
  select(Date,waterYear,Q) %>%
  mutate(station = gen_dat$station[i],
         station = as.factor(station))

}

big.data = do.call(rbind, wq.data)

###############################################################################
# plot water year values
## all the points

big.data %>%
  ggplot(aes(x = as.factor(waterYear), y = Q))+
  geom_boxplot()+
  facet_wrap(~station,scales = "free")

## summarise by mean and ci
df2 <- summarySE(big.data, measurevar = "Q",
                    groupvars=c("waterYear", "station"))


# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

# Use 95% confidence interval instead of SEM
p1 <- ggplot(df2, aes(x=waterYear, y=Q)) +
  geom_errorbar(aes(ymin=Q-ci, ymax=Q+ci), width=.1, position=pd) +
  # geom_smooth(se = FALSE) +
  geom_point()+
  facet_wrap(~station,scales = "free")+
  labs(x = "Year" , y = "Q, cubic meters per second")+
  theme(text = element_text(size = 9))+
  theme_bw()

ggsave(p1, filename = "results/figs/Qwateryear.tiff",height = 5, width = 8, unit = "in", dpi = 300)

##############################################################################
# low flow
q30 <- quantile(wq.data$Q, 0.3)

# moderate flow
q60 <- quantile(wq.data$Q, 0.6)

#high flow
q90 <- quantile(wq.data$Q, 0.9)

# run a t.test on the pre/post daily mean flows
sum.tab <- big.data %>%
  mutate(period = ifelse(Date >= "2015-07-03","post","pre"),
         period = as.factor(period)) %>%
  group_by(station,period) %>%
  #group_map(~ t.test(Q ~ period, .x, paired = FALSE))
  summarise(mean = mean(Q, na.rm = TRUE),
            sd = sd(Q,na.rm = TRUE)) %>%
  pivot_wider(names_from= period, values_from = c(mean,sd)) %>%
  mutate(per_change = ((mean_post - mean_pre)/mean_pre)*100) %>%
  select(station,mean_pre, sd_pre,mean_post,sd_post,per_change)

write.csv(sum.tab , file = "data/process/Q_period_comp.csv", row.names = FALSE)












# Step 1: plot relationship between Q and Precipitation
data <- wq.data %>%
  mutate(period = ifelse(Date >= "2015-07-03","post","pre"), # period
         #season = ifelse(Day > 335 | Day < 75, "winter","grow"),
         season = ifelse(Month == 12 |Month == 1| Month == 2 | Month == 3, "winter","grow"),
         fc = ifelse(Q >= q90, "v.high",ifelse(Q >= q60, "high", ifelse(Q>=q30,"moderate", "low"))))
# flow pre/post (mean value)
results[[i]] <- data %>%
    group_by(period) %>%
    summarise(mean = mean(Q,na.rm = TRUE),
              sd = sd(Q, na.rm = TRUE),
              count = n())%>%
    pivot_wider(names_from= period, values_from = c(count,mean,sd)) %>%
    mutate(per_change = ((mean_post - mean_pre)/mean_pre)*100,
           station = gen_dat$station[i])



# compare pre and post daily flow values using a student's t. test


# results[[i]] <- data %>%
#   group_by(period,season,fc) %>%
#   summarise(mean = mean(Q,na.rm = TRUE),
#             sd = sd(Q, na.rm = TRUE),
#             count = n())%>%
#   pivot_wider(names_from= period, values_from = c(count,mean,sd)) %>%
#   mutate(per_change = ((mean_post - mean_pre)/mean_pre)*100,
#          station = gen_dat$station[i])

}

# join lists together into a dataframe
res <- do.call(rbind.data.frame, results)
#write.csv(res, "exploratory/scratch/23_oct_2023/per_change.csv")

#plot data
res %>%
  ggplot(aes(x = season, y = per_change,color = station,shape = station))+
  geom_point()+
  facet_wrap(~fc)

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

}

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



