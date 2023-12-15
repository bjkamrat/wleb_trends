# water year function
library(lubridate)
##############################################################
#in this script or another, define a water year function
water_year <- function(date){
  ifelse(month(date) < 10, year(date), year(date)+1)
}
##############################################################