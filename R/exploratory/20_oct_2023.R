###############################################################################
#  18_oct_23.R
#  Details: Investigate Hydrologic and biological controls on P export using
#  methods dervied from Williams et al., 2016
#  - See Lorenz curves
#
#
#############################################################################

options(scipen = 999)

# Load in libraries
library(rio)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ineq)

################################################################################
# gini function , split by period

gini<-function(period){
  df_sub<-df[df$period==period,]
  Qgin <- ineq(df_sub$Q,type="Gini")
  Fluxgin <- ineq(df_sub$GenFlux,type="Gini")
  rel_in <- Fluxgin/Qgin
  outputs <- c(Qgin = round(Qgin,2),Fluxgin = round(Fluxgin,2),rel_in = round(rel_in,2))
  return(outputs)
}
################################################################################
# read in general data
gen_dat <- import("data/wrtds/input/gen_dat_V2.xlsx")

#stations vector
stations <-  gen_dat$station

#periods vector
periods <- c("pre","post")

k <- 1
gini_results <- NULL

#double for loop to run gini function for each combination of statoin and period
for(i in 1:length(stations)){
  for(j in 1:length(periods)){

    # read in data
    raw.df <- import(paste("data/wrtds/output/eList_k/daily/srp/",stations[i],".csv", sep = ""))

    # plot data to ID differences in precipitation
    df <- raw.df %>%
      mutate(Qcum = cumsum(Q),
             Fluxcum = cumsum(GenFlux),
             period = ifelse(Date >= "2015-07-03", "post","pre")) %>%
      select(period, Month, waterYear,Q,GenConc,GenFlux,Qcum,Fluxcum)

    gini_results[[k]] <- gini(period = periods[j])
    gini_results[[k]]$station <- stations[i]
    gini_results[[k]]$periods <- periods[j]
    k <- k+1
  }
}

# combine gini_results list into single dataframe for export
results <- as.data.frame(do.call(rbind,gini_results))


ggplot(results,aes(x = year,y=rel_in))+
  geom_point()+
  geom_smooth()


################################################################################
# Fourier analysis (try this - might not be necessary, but worth a shot)




