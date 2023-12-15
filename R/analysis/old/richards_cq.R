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

# load in c/Q data
data <- import(paste0("data/wrtds_output/eList_k/daily/",poi,"/",gen_dat$station[1],".csv",sep = ""))

# set threshold
sfQ <- quantile(data$Q,0.6) # 40% flow exceedance
lsQ <- quantile(data$Q, 0.95) # 5% flow exceedance

#filter dataset to those where the seven day flow is greater than the 60% flow
data_sf <- data %>%
  filter(Q7>=sfQ)

# find stormevents that are at least 3 days.
events = eventPOT(data$Q, threshold = sfQ) %>%
  mutate(span = end-srt) %>%
  filter(span >=3)

# clip events to those with a peak greater than 95% flow
events_ls <- events %>%
  filter(max >= lsQ)

#clip events to those with a peak lower than 95% flow
events_ss <- events %>%
  filter(max < lsQ)

#ID rising and falling limbs -> this needs work
limbs_ss <-  limbs(data$Q,events = events_ss,to.plot = FALSE)

##############################################################################
#clip to rising and falling limb datasets - > this needs work
ris.outputs <- c()
fal.outputs <- c()

for(i in 1:nrow(limbs_ss)){
  x <- seq(limbs_ss$ris.srt[i],limbs_ss$ris.end[[i]], by = 1)
  y <- seq(limbs_ss$fal.srt[i],limbs_ss$fal.end[[i]], by = 1)
  ris.outputs <- c(ris.outputs,x)
  fal.outputs <- c(fal.outputs,y)
}

data_sr <- data[c(ris.outputs),]
data_sf <- data[c(fal.outputs),]

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
