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
poi <- "srp"

# set seasons (month start (mon1) and season length (seasons_len))
seasons <- c("annual","winter","spring","summer","fall")
mon1 <- c(10,12,3,8,10)
season_length <- c(12,3,5,2,2)

# load in general data
gen_dat_2 <- read_excel("data/wrtds_input/gen_dat_V2.xlsx")

#set start and end years
gen_dat_2$start_year <- 2009 # Scenario 1: 2009-2021


output <- NULL
results <- NULL

# start year must be altered for the last season
for(i in 1:length(seasons)){
  for(j in 1:nrow(gen_dat_2)){
    # set path for inputs to egret
    datafile <- paste0("./data/wrtds_output/eList/",gen_dat_2$station[j],".",poi,".RData",sep = "")

    # load in datafile
    load(datafile)

    eList_sen <- setPA(eList, paStart = mon1[i], paLong = season_length[i])
    returnDF <- tableResults(eList_sen)

    if(i <= 4){
      pairResults1 <- runPairs(eList, year1 = gen_dat_2$start_year[j], year2 = 2021, windowSide = 0,
                               paStart = mon1[i], paLong = season_length[i])

      results$initalconc[j] <- returnDF[returnDF$Year == gen_dat_2$start_year[j], 4]
      results$intitalflux[j] <- returnDF[returnDF$Year == gen_dat_2$start_year[j], 6]

      results$finalconc[j] <- returnDF$`FN Conc [mg/L]`[which(returnDF$Year == 2021)]
      results$finalflux[j] <- returnDF$`FN Flux [10^6kg/yr]`[which(returnDF$Year == 2021)]

    } else {
      pairResults1 <- runPairs(eList, year1 = gen_dat_2$start_year[j]-1, year2 = 2020, windowSide = 0,
                               paStart = mon1[i], paLong = season_length[i])

      results$initalconc[j] <- returnDF[returnDF$Year == gen_dat_2$start_year[j], 4]
      results$intitalflux[j] <- returnDF[returnDF$Year == gen_dat_2$start_year[j], 6]

      results$finalconc[j] <- returnDF$`FN Conc [mg/L]`[which(returnDF$Year == 2020)]
      results$finalflux[j] <- returnDF$`FN Flux [10^6kg/yr]`[which(returnDF$Year == 2020)]
    }



    results$totalChangeConc[j] <- round(pairResults1$TotalChange[1],3)
    results$totalChangeFlux[j] <- round(pairResults1$TotalChange[2],3)
    results$perChangeConc[j] <- round((pairResults1$TotalChange[1]/pairResults1$x11[1])*100,3)
    results$perChangeFlux[j] <- round((pairResults1$TotalChange[2]/pairResults1$x11[2])*100,3)
    results$station[j] <- gen_dat_2$station[j]
    results$year1[j] <- gen_dat_2$start_year[j]
    results$year2[j] <- 2021
    results$season[j] <- seasons[i]
    results$span[j] <- results$year2[j]-results$year1[j]
    results$perSlopeConc[j] <- results$perChangeConc[j]/results$span[j]
    results$perSlopeFlux[j] <- results$perChangeFlux[j]/results$span[j]

  }

  df <- as.data.frame(do.call(cbind, results))
  results <- NULL
  output[[i]] <- df
}

seasonal_results <- as.data.frame(do.call(rbind,output))
write.csv(seasonal_results, file = paste("data/wrtds_output/trends/seasonal/",poi,"/scen1.csv",sep =""), row.names = FALSE)




