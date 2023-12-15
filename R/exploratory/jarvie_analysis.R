################################################################################
# Author: Brock Kamrath
# Date: 14 Sep 2023
# Objective: Calculate and plot cumulative SRP loads using WRTDS-K estimates of
#            daily loads at each site. This will be similar to the analysis
#            conducted by Jarvie et al., (2017)
#
# Inputs: Daily WRTDS-K load estimates in the data/wrtds/output/eList_k/daily
#         folder
#
# Outputs: Cumulative SRP loads from WY2008-WY2022 in kg/ha
################################################################################

# Load in libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(rio)

################################################################################
# load in general data to use for loops
# set seed
set.seed(522)

# set poi
poi <- "srp"

#set station
station <- "Sandusky"

# load in general data
gen_dat_2 <- read_excel("data/wrtds/input/gen_dat_V2.xlsx")
################################################################################
# set watershed area
area <- as.numeric(gen_dat_2[gen_dat_2$station == station, 7])

# split raw data by period and conduct linear regression for each line
pre_post <- c("pre","post")

# read in raw wrtds_k data from station and create period columan to separate pre and post data
raw_data <- import(paste("data/wrtds/output/eList_k/daily/",poi,"/",station,".csv",sep = "")) %>%
  mutate(period = ifelse(Date >= "2015-07-03", "post", "pre"))

# create new list to put pre and post data into
all_dat <- list()

#pre-data
all_dat[[1]] <- raw_data %>%
  filter(period == pre_post[1]) %>%
  mutate(csum = (cumsum(GenFlux)/area),
         FNcsum = (cumsum(FNFlux)/area),
         csum_q = (cumsum(Q*86400)/area),
         csum_c = (cumsum(GenConc)))

#post-data
  all_dat[[2]] <- raw_data %>%
  filter(period == pre_post[2]) %>%
  mutate(csum = (cumsum(GenFlux)/area),
         FNcsum = (cumsum(FNFlux)/area),
         csum_q = (cumsum(Q*86400)/area),
         csum_c = (cumsum(GenConc)))

# create a c-q relationship for each period
  # create null list to put information
  cq <- NULL

  # c-q relationship for pre-data
  cq[[1]] <- lm(GenConc ~ Q, all_dat[[1]])
  summary(cq[[1]])

  #c-q relationship for post-data
  cq[[2]] <- lm(GenConc ~ Q, all_dat[[2]])
  summary(cq[[2]])

  # here I am predicting the concentration for the post-period using the pre-period c-q relationship
  all_dat[[2]]$mod_conc <- predict(cq[[1]], newdata = all_dat[[2]])

  #here I am converting the predicted concentration to flux then summing it
  all_dat[[2]] <- all_dat[[2]] %>%
    mutate(mod_flux = mod_conc*Q*86.4,
           csum_mod = cumsum(mod_flux)/area)

  #Create a null model (m) list to store slr results
  m <- NULL

  # pre-data relationship of cumulative SRP loads vs. decimal year (DecYear)
  m[[1]] <- lm(csum ~ DecYear, all_dat[[1]])
  summary(m[[1]])

  # Post data relationship of cumulative SRP load vs. decimal year (DecYear)
  m[[2]] <- lm(csum ~ DecYear, all_dat[[2]])
  summary(m[[2]])

  # Post period relationship of cumulative SRP load vs. decimal yar (DecYear), using
  # the predicted concentrations from the pre period c-q relationship
  m[[3]] <- lm(csum_mod ~ DecYear, all_dat[[2]])
  summary(m[[3]])

  #creat a dataframe for model results
models <- c()

for(i in 1:3){

  coef <- summary(m[[i]])$coef
  v.coef <- c(t(coef[,c(1:2)]))
  names(v.coef) <- paste(rep(rownames(coef), each =  2), c("coef", "stderr"))
  v.model_info <- c(r.squared = summary(m[[i]])$r.squared, F = summary(m[[i]])$fstatistic[1], df.res = summary(m[[i]])$df[2])

  v.all <- c(v.coef, v.model_info)
  models <- rbind(models, cbind(data.frame(model = paste(station[1], sep = "")), t(v.all)))

}

# write results to file
write.csv(models, file = paste("data/process/jarvie/",poi,"/",station[1],".csv",sep =""),row.names = FALSE)
