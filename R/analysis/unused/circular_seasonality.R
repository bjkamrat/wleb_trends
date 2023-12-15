###############################################################################
# Author: Brock Kamrath
# Date: 30 aug 2023
# Objective: Conduct seasonality analysis using monthly loads in provided by egret
###############################################################################

# Load in libraries
library(tidyverse)
library(ggplot2)
library(EGRET)
library(readxl)

# set parameters
poi <- "tp"

# load in general data
gen_dat_2 <- read_excel("data/wrtds_input/gen_dat_V2.xlsx")

cir_stat <- read.csv("data/wrtds_input/circular_stats_monthly.csv")

results <- list()

# load in eList
for(i in 1:nrow(gen_dat_2)){

  datafile <- paste0("./data/wrtds_output/eList/",gen_dat_2$station[i],".",poi,".RData",sep = "")

  # load in datafile
  load(datafile)

  monthlyResults <- calculateMonthlyResults(eList) %>%
    mutate(FNLoad = FNFlux*nDays) %>%
    filter(Year >= 2009 & Year <= 2021) %>%
    rename("month" = "Month")

  years <- unique(monthlyResults$Year)

  output <- list()

  for(j in 1:length(years)){

    data <- monthlyResults %>%
      filter(Year == years[j])


    dat_j <- left_join(data, cir_stat, by = "month")

    sc_dat <- dat_j %>%
    summarise(s = sum(FNLoad*sin),
              c = sum(FNLoad*cosine))

    Pr <- (sc_dat$s^2 + sc_dat$c^2)^0.5

    if(sc_dat$c < 0){
      theta <- (atan(sc_dat$s/sc_dat$c)*180/pi)+180
    } else if(sc_dat$s < 0){
      theta <- (atan(sc_dat$s/sc_dat$c)*180/pi)+360
    } else{
      theta <- atan(sc_dat$s/sc_dat$c)*180/pi
    }

    Is <- Pr/sum(dat_j$FNLoad)

    output$year[j] <- data$Year[1]
    output$theta[j] <- theta
    output$Is[j] <- Is
    output$station[j] <- gen_dat_2$station[i]

  }

  results[[i]] <- output

}

seasonality <- do.call(rbind.data.frame, results)


seasonality %>%
  ggplot(aes(x = year, y = theta, color = station ))+
  #ggplot(aes(x = year, y = Is, color = station ))+
  geom_point()+
  geom_smooth()
