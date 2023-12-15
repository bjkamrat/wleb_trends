library(readxl)
library(tidyverse)
library(EGRET)

obtain_daily <- function(site){
  gen_dat <- read_excel("data/wrtds/input/gen_dat.xlsx")

  stat_dat <- gen_dat %>%
    filter(station == site)

  q_data <- readNWISDaily(siteNumber = stat_dat$usgs,parameterCd = "00060",
                         startDate = stat_dat$start_date,endDate = stat_dat$end_date)

  return(q_data)
}
