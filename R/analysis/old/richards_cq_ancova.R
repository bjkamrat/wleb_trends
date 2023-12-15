################################################################################
# Author: Brock Kamrath
# Date: 30 Aug 2023
# Objective: Richards et al., 2008 analysis of high, low flow and winter/summer seasons
###############################################################################

library(rio)
library(tidyverse)
library(ggplot2)
library(dplyr)
library("RColorBrewer")

raw_data <- import("data/wrtds_output/eList_k/daily/srp/Maumee.csv")

q60 <- quantile(raw_data$Q, 0.60)

base_data <- raw_data %>%
  mutate(flow = ifelse(Q >= q60, "High","Low"),
         season = ifelse(Month %in% c(12,1,2,3,4,5),"winter","summer"),
         logConc = log(GenConc),
         logQ = log(Q)) %>%
  filter(waterYear >= 2009 & waterYear <= 2021) %>%
  dplyr::select(Date,Month,waterYear,Q,GenConc,logQ,logConc,flow,season) %>%
  mutate(YEAR = as.factor(waterYear))

# Run ANCOVA
#code to run the 2-way anova for the epiphyte data to test model assumptions
ancova_data <- base_data %>%
  filter(season == "summer")

ancova<-lm(logConc~ logQ + YEAR, data=ancova_data)
anova(ancova)
m <- summary(ancova)

library(HH)
ancova(logConc ~ logQ + YEAR, data=ancova_data)


mod <- ancova(logConc ~ logQ + YEAR, data=ancova_data)
pred <- predict(mod)

ggplot(data = cbind(ancova_data, pred),
       aes(logQ, logConc, color=YEAR)) + geom_line(aes(y=pred))

# pull out results

est <- m$coefficients[-2,1]
res <- as.data.frame(est)
res$year <- seq(2009,2021,by = 1)
res$inter <- res$est

for(i in 2:nrow(res)){
  res$inter[i] <- res$est[1] + res$est[i]
}

res %>%
  ggplot(aes(x= year,y = inter))+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)

summary(lm(inter ~ year, data = res))
