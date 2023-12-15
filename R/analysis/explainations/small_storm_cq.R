#################################################################################
# Date: 23 Aug 2023
# Author: Brock Kamrath
# Objective: evaluate read concentration and flow data using Richards et al. 2009 methodology
###############################################################################

#load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(rio)
library(hydroEvents)

# load in general datafile
# set seed
set.seed(522)

# set poi
poi <- "srp"

# set seasons (month start (mon1) and season length (seasons_len))
seasons <- c("winter","spring","summer","fall")
mon1 <- c(12,3,8,10)
season_length <- c(3,5,2,2)

# load in general data
gen_dat <- read_excel("data/wrtds_input/gen_dat.xlsx")

p <- NULL

for(i in 1:nrow(gen_dat)){

# load in c/Q data
data <- import(paste0("data/wrtds_output/eList_k/daily/",poi,"/",gen_dat$station[i],".csv",sep = ""))

# set threshold
sfQ <- quantile(data$Q,0.6) # 40% flow exceedance
lsQ <- quantile(data$Q, 0.90) # 10% flow exceedance

data_ss <- data %>%
  filter(Q >= sfQ & Q < lsQ) %>%
  group_by(waterYear) %>%
  summarise(mean = mean(GenConc))

p[[i]] <- data_ss %>%
  ggplot(aes(x = waterYear, y = mean))+
  geom_point()+
  geom_smooth(se = FALSE,span = 0.75)+
  geom_vline(xintercept = 2015)+
  lims(y = c(0,0.15))+
  labs(y = "", x = "", title = gen_dat$station[i])+
  theme_classic()+
  scale_x_continuous(breaks = seq(2008,2022, 2))

# rate_all <- 100*((data_ss$mean[nrow(data_ss)] - data_ss$mean[1])/data_ss$mean[1])/(nrow(data_ss)-1)
# rate_recent <- 100*((data_ss$mean[nrow(data_ss)] - data_ss$mean[which(data_ss$waterYear==2015)])/data_ss$mean[which(data_ss$waterYear==2015)])/7
}


#visualize all plots
p[1]

gridExtra::grid.arrange(p[[4]],p[[7]],p[[6]],p[[5]],p[[8]],
                        p[[1]],p[[3]],ncol = 2)


###############################################################################

# Visuzalize
data_sf %>%
  ggplot(aes(x = as.factor(waterYear),y = GenConc))+
  geom_point()+
  geom_smooth(se = TRUE, aes(group = 1))

data_sr %>%
  group_by(waterYear) %>%
  summarise(fwmc = weighted.mean(GenConc,Q)) %>%
  ggplot(aes(x = waterYear,y = fwmc))+
  geom_point()+
  geom_smooth(se = FALSE)
