################################################################################
# Author: Brock Kamrath
# Date 31 July 2023
# Objective: Write a function that runs eList for WRTDS Kalman, then outputs daily
# and annual loads
################################################################################

# This script takes the previously created wrtds models and reruns them using the
# Kalman filter


# Load in libraries
library(readxl)
library(tidyverse)
library(EGRET)

# set seed
set.seed(522)

# load in general data
gen_dat_2 <- read_excel("data/wrtds/input/gen_dat.xlsx")

# set parameter
poi <-  "tss"

# Create empty lists for future use
df_conc_list <- NULL
df_flux_list <- NULL

# run for loop for each site
for(i in 1:nrow(gen_dat_2)){

  # set path for inputs to egret
  datafile <- paste0("./data/wrtds/output/eList/",gen_dat_2$station[i],".",poi,".RData",sep = "")

  # load in datafile
  load(datafile)

  # now we will run the WRTDSKalman estimation (using the default number of iterations and rho values for now)
  eList_K <- WRTDSKalman(eList, niter = 500, rho = 0.9)

  # plot results
  plotConcHist(eList_K, plotAnnual = FALSE, plotGenConc = TRUE)
  plotFluxHist(eList_K, plotAnnual = FALSE, plotGenFlux = TRUE, fluxUnit = 5)

  # set daily results into a
  DailyResults <- eList_K$Daily

  # write daily results to csv file
  write.csv(DailyResults, file = paste0("data/wrtds/output/eList_k/daily/",poi,"/",gen_dat_2$station[i],".csv",sep = ""), row.names = FALSE)

  # Summarizing results at an annual time step
  AnnualResults <- setupYears(eList_K$Daily)

  # write annual results to csv file
  write.csv(AnnualResults, file = paste0("data/wrtds/output/eList_k/annual/",poi,"/",gen_dat_2$station[i],".csv",sep = ""), row.names = FALSE)


  # Evaluate the changes in load for 2008 and 2015
  df_conc <- tableChangeSingle(eList_K, fluxUnit = 5, yearPoints=c(2009,2015,2021), flux = FALSE)
  colnames(df_conc) <- c("Year1","Year2","change_mgL","slope_mgL_yr","change_%","slope_%_yr")
  df_conc$station <- gen_dat_2$station[i]

  df_conc_list[[i]] <- df_conc

  df_flux <- tableChangeSingle(eList_K, fluxUnit = 5, yearPoints=c(2009,2015,2021), flux = TRUE)
  colnames(df_flux) <- c("Year1","Year2","change_tons_yr","slope_tons_yr_yr","change_%","slope_%_yr")
  df_flux$station <- gen_dat_2$station[i]

  df_flux_list[[i]] <- df_flux

}

df_conc <- do.call(rbind, df_conc_list)
write.csv(df_conc, "data/wrtds/output/trends/annual/srp/conc_trends.csv", row.names = FALSE)

df_flux <- do.call(rbind, df_flux_list)
write.csv(df_flux, "data/wrtds/output/trends/annual/srp/flux_trends.csv", row.names = FALSE)

