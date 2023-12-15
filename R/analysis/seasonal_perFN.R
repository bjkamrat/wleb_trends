################################################################################
# Date: 21 Aug 2023
# Objective: % of annual load release during season -> trend
################################################################################

# add source
source("R/funcs/filter_setPA.R")

#load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(foreach)
library(doParallel)
library(iterators)
library(EGRET)
library(EGRETci)

# load in general datafile
# set seed
set.seed(522)

# set poi
poi <- "tp"

# set seasons (month start (mon1) and season length (seasons_len))
seasons <- c("winter","spring","offseason")
mon1 <- c(12,3,8)
season_length <- c(3,5,4)

# load in general data
gen_dat_2 <- read_excel("data/wrtds/input/gen_dat_V2.xlsx")

#Scenario 1
gen_dat_2$start_year <- 2009

output <- NULL
results <- NULL

# start year must be altered for the last season
for(i in 1:length(seasons)){
  for(j in 1:nrow(gen_dat_2)){
    # set path for inputs to egret
    datafile <- paste0("./data/wrtds/output/eList/",gen_dat_2$station[j],".",poi,".RData",sep = "")

    # load in datafile
    load(datafile)

    eList_sen <- setPA(eList, paStart = mon1[i], paLong = season_length[i])
    returnDF <- tableResults(eList_sen)

    seasonPctResults <- setupSeasons(eList_sen)

    results$initialflux[j] <- round(seasonPctResults$pctFlux[1],2)
    results$initialFNflux[j] <- round(seasonPctResults$pctFNFlux[1],2)

    results$perFlux[j] <- round(mean(seasonPctResults$pctFlux),2)
    results$perFNFlux[j] <- round(mean(seasonPctResults$pctFNFlux),2)


    results$finalflux[j] <- round(seasonPctResults$pctFlux[nrow(seasonPctResults)],2)
    results$finalFNflux[j] <- round(seasonPctResults$pctFNFlux[nrow(seasonPctResults)],2)

    results$station[j] <- gen_dat_2$station[j]
    results$year1[j] <- gen_dat_2$start_year[j]
    results$year2[j] <- 2021
    results$season[j] <- seasons[i]

    results$span[j] <- results$year2[j]-results$year1[j]

    results$perSlopeFlux[j] <- round(100*((results$finalflux[j] - results$initialflux[j])/(results$initialflux[j]))/results$span[j],2)
    results$perSlopeFNFlux[j] <- round(100*((results$finalFNflux[j] - results$initialFNflux[j])/(results$initialFNflux[j]))/results$span[j],2)

  }

  df <- as.data.frame(do.call(cbind, results))
  results <- NULL
  output[[i]] <- df
}

seasonal_results <- as.data.frame(do.call(rbind,output))
write.csv(seasonal_results, file = paste("data/wrtds/output/trends/seasonal/",poi,"/perAnnual_v2.csv",sep =""), row.names = FALSE)




