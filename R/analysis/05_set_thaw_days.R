# I am going to set the freeze, thaw, and warm periods within the water quality dataset using
# the frost dataset


# load in libraries
library(tidyverse)
library(rio)
library(lubridate)
library(zoo)



# read in weaterh data from OHio State University north west station located north of Hoytville, Ohio
r.data <- import("data/process/frozen_soil/frost_data_final.xlsx") %>%
  mutate(Date = as.Date(Date,format = "%m/%d/%Y")) %>%
  select(Date, frost, frost_out_days) %>%
  mutate_if(is.character, ~replace(., is.na(.), "warm"))

# load in Maumee River data
wq.data <- import("data/wrtds/output/eList_k/daily/srp/Maumee.csv") %>%
  mutate(Date = as.Date(Date)) %>%
  select(Date, Julian, Month, Day, waterYear, Q, GenConc, GenFlux)

# join data
j.data <- left_join(wq.data,r.data,by = c("Date")) %>%
  mutate(season = ifelse(Month >= 7,"fall","spring"))

# set frost periods to "frost"
j.data$season[j.data$frost == "frost_in" |j.data$frost == "frost"] <- "frost"

# set thaw days to "thaw"
j.data$season[j.data$frost_out_days >= -5 & j.data$frost_out_days <=20] <- "thaw"

# save frozen/thaw data
write.csv(j.data, file = "data/process/frozen_soil/frost_thaw_warm.csv",row.names = FALSE)





