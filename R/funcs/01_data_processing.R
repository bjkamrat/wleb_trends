# # Author: Brock Kamrath
# Date: 5/03/2023
# Objective: R/functions

#load necessary packages
library(Hmisc)
library(lubridate)

 #create a function to read in data
get_data <- function(file){
  read_csv(file)
}

# create function to convert from wide to long
raw_long <- function(input){
  output <- input %>%
    mutate(Date = as.Date(INT.date, format = "%m/%d/%Y")) %>%
    select(Date, SRP, TP, TSS, NO23) %>%
    pivot_longer(!Date,names_to = "pol",values_to = "flux_kgyr" )
  
  return(output)
}


# water year function
##############################################################
#in this script or another, define a water year function
water_year <- function(date){
  ifelse(month(date) < 10, year(date), year(date)+1)
}
##############################################################

# calculate geometric mean
geomMean<-function(values){ 
  prod(values)^(1/length(values)) 
}

################################################################################
# Functions for daily flow-weighted averages
###############################################################################
# calculate flow-weighted daily average
d_fwmc <- function(data){
  output <-  weighted.mean(data$conc_mgL,data$q_cfs,na.rm = FALSE)
  return(output)
}



# for loop that evaluates data each individual site for one pollutant of interest
calc_daily_fwmc <- function(data){
  
  # find the unique dates (i.e., single dates with concentration data)
  conc_daily <- as.data.frame(unique(df$Date))
  
  colnames(conc_daily) = c("Date")
  
  for(i in 1:nrow(conc_daily)){
    conc_day <- df %>%
      filter(Date == conc_daily$Date[i])
    
    conc_daily$conc[i] <- d_fwmc(conc_day)
  }
  
  conc_daily <- conc_daily %>%
    mutate(remark = if_else(conc <= 0.001, "<","")) %>%
    filter(conc >= 0.001) %>%
    select(Date,remark,conc)
  
  return(conc_daily)
}

################################################################################