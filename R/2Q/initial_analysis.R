################################################################################
# create table results for EGRET for each site and pollutant for the 2Q analysis
# results will be annual results
################################################################################

#load in libraries
library(EGRET)
library(tidyverse)
library(ggplot2)
library(rio)
library(readxl)

#source
source("R/2Q/WRTDS_FN_2Q_ver.1.1.R")

# load in general datafile
# set seed
set.seed(522)

# set poi
poi <- "tp"

# load in general data
gen_dat_2 <- read_excel("data/wrtds_input/gen_dat_V2.xlsx")

# #set start and end years
# start_year <- 2009 # Scenario 1: 2009-2021
# end_year <- 2021
#
dy_res <- NULL
returnDF <- NULL

# start year must be altered for the last season
for(i in 1:nrow(gen_dat_2)){
  # set path for inputs to egret
  datafile <- paste0("./data/wrtds_output/eList/",gen_dat_2$station[i],".",poi,".RData",sep = "")

  # load in datafile
  load(datafile)

  # conduct analysis for high and low flows based on a cutoff of 0.6
  res <- Estby2Flows(eList)

  ann.est <- annualWY.est(res) %>%
    filter(waterYear >= 2009 & waterYear <= 2021) %>%
    select(-DecYear,-STAID,-Parameter) %>%
    mutate(station = gen_dat_2$station[i])

  returnDF[[i]] <- ann.est

  change_est <- ann.est[,c(10,9,5,6,7,8)] %>%
    pivot_longer(cols = c(3:6),names_to = "type", values_to = "value") %>%
    separate(type, c('type', 'flow'))

  ##############################################################################
  # calculate trends from 2009 to 2015 and from 2015 to 2021 in concentration
  ## set flow
  ty <- "FNConc" # if you change this, then you have to change the write.csv file name
  fc <- c("HIGH","LOW")
  dynamDF <- NULL

  for(k in 1:length(fc)){
    output_df <- change_est %>%
      filter(type == ty & flow == fc[k])

    run_scen <- NULL
    scen <- c("pre","all","post")
    start <- c(1,1,7)
    end <- c(7,13,13)
    span <- c(6,12,6)

    for(j in 1:length(start)){
      ### Pre
      unit_change <- as.numeric(output_df[end[j],5] - output_df[start[j],5])
      unit_slope <- as.numeric((output_df[end[j],5] - output_df[start[j],5])/span[j])

      per_change <- as.numeric(100*((output_df[end[j],5] - output_df[start[j],5])/output_df[start[j],5]))
      per_slope <-  as.numeric(100*((output_df[end[j],5] - output_df[start[j],5])/output_df[start[j],5])/span[j])

      run_scen[[j]] <- c(unit_change,unit_slope, per_change, per_slope,fc[k],ty,scen[j],gen_dat_2$station[i])
    }

  df1 <- as.data.frame(do.call(rbind,run_scen))

  dynamDF[[k]] <- df1
  }

  dy_res[[i]] <- as.data.frame(do.call(rbind,dynamDF))
}

  annual <- as.data.frame(do.call(rbind,returnDF))
  # write annual results
  write.csv(annual, file = paste("data/results/2Q/",poi,"_overall_results_2Q.csv", sep = ""),row.names = FALSE)

  df <- as.data.frame(do.call(rbind,dy_res))
  colnames(df) <- c("unit_change","unit_slope","per_change","per_slope","flow","type","scenario","station")

  # write annual results
  write.csv(df, file = paste("data/results/2Q/",poi,"_conc_results.csv", sep = ""), row.names = FALSE)


################################################################################
  # Plot annual values from 2Q analysis
  ann.est %>%
    filter(DecYear <= 2021 & DecYear >= 2009) %>%
    ggplot()+
    geom_point(aes(x = DecYear, y = FNFlux.LOW),color = "blue")+
    geom_line(aes(x = DecYear, y = FNFlux.LOW),color = "blue")+
    geom_point(aes(x = DecYear, y = FNFlux.HIGH),color = "red")+
    geom_line(aes(x = DecYear, y = FNFlux.HIGH),color = "red")+
    geom_point(aes(x = DecYear, y = FNFlux),color = "black")+
    geom_line(aes(x = DecYear, y = FNFlux),color = "black")


  mon.est %>%
    filter(Month == 1) %>%
    mutate(yrmon = make_date(Year, Month)) %>%
    ggplot()+
    geom_point(aes(x = yrmon, y = FNFlux.LOW),color = "blue")+
    geom_line(aes(x = yrmon, y = FNFlux.LOW),color = "blue")+
    geom_point(aes(x = yrmon, y = FNFlux.HIGH),color = "red")+
    geom_line(aes(x = yrmon, y = FNFlux.HIGH),color = "red")+
    geom_point(aes(x = yrmon, y = FNFlux),color = "black")+
    geom_line(aes(x = yrmon, y = FNFlux),color = "black")
    #scale_x_date(date_breaks = "5 month",date_labels = "%B-%Y")+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

################################################################################
