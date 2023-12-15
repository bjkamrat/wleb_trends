# individual site analysis for frozen, thaw, and warm seasons.
# I want to know the fw mean values and the percent change between them for each season
# I also need to know the count


# libraries
library(tidyverse)
library(ggplot2)
library(rio)

# read in frost/thaw data, clip out unnesscary information
ftw.data <- import("data/process/frozen_soil/frost_thaw_warm.csv") %>%
  select(Date,season) %>%
  mutate(Date = as.Date(Date,format = "%Y-%m-%d"))

# set site
site <-  c("Tiffin","Blanchard","Maumee", "Raisin")

# set pollutant of interest
poi <- c("srp","tp","tss")

output <- NULL

for(i in 1:length(site)){
  # load in wq data
  # load in Maumee River data
  wq.data <- import(paste("data/wrtds/output/eList_k/daily/srp/",site[i],".csv",sep = "")) %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, Julian, Month, Day, waterYear, Q, GenConc, GenFlux) %>%
    mutate(station = site[i])

  # join the data into j.data
  j.data <- left_join(wq.data,ftw.data,by = c("Date"))

  ################################################################################
  # Begin analysis for Maumee River
  # first, determine the number of values and mean values
  output[[i]] <- j.data %>%
    mutate(period = ifelse(Date >= "2015-07-03","post","pre"))
}

# join lists together into a dataframe
res <- do.call(rbind.data.frame, output)

################################################################################
# ecdf analysis
res %>% ggplot(aes(GenConc, color = period))+
  stat_ecdf()+
  facet_wrap(station~season,scales = "free")

res_ecdf <- res %>%
  group_by(station,period,season) %>%
  mutate(rank = rank(-Q)) %>%
  mutate(P = 100 * (rank / (length(Q) + 1)))


res_ecdf %>% ggplot(aes(x = P, y = Q, color = period))+
  geom_line()+
  scale_y_log10()+
  xlab("% Time flow equalled or exceeded")+
  ylab("Concentration (mg/L)")+
  facet_wrap(station~season,ncol = 3)

################################################################################
# count analysis
res %>%
  group_by(period,season,pollutant) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from= c(period,pollutant), values_from = c(count))

################################################################################
# weighted means
data.frame <- res %>%
  group_by(period,season,station) %>%
  summarise(fwmc = mean(Q)) %>%
  pivot_wider(names_from= c(period), values_from = c(fwmc)) %>%
  mutate(per_change = round(((post - pre)/pre)*100))

total <- res %>%
  group_by(period,station) %>%
  summarise(total = sum(Q))

szn <- res %>%
  group_by(period,season,station) %>%
  summarise(mean = mean(Q),
            szn_total = sum(Q))

all <- left_join(szn,total,by= c("period","station")) %>%
  mutate(per_flow = round(100*(szn_total/total),1))

write.csv(all, file = "data/process/frozen_soil/Q_overview.csv",row.names = FALSE)


