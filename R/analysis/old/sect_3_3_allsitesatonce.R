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
site <-  "Raisin"

# set pollutant of interest
poi <- c("srp","tp","tss")

output <- NULL

for(i in 1:length(poi)){
  # load in wq data
  # load in Maumee River data
  wq.data <- import(paste("data/wrtds/output/eList_k/daily/",poi[i],"/",site,".csv",sep = "")) %>%
    mutate(Date = as.Date(Date)) %>%
    select(Date, Julian, Month, Day, waterYear, Q, GenConc, GenFlux) %>%
    mutate(pollutant = poi[i])

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

res_srp <- res %>%
  filter(pollutant == "srp")

slr <- lm(GenConc ~ period*season, res_srp)
summary(slr)
anova(slr)

################################################################################
# ecdf analysis
res %>% ggplot(aes(GenConc, color = period))+
  stat_ecdf()+
  facet_wrap(pollutant~season,scales = "free")

res_ecdf <- res %>%
  group_by(pollutant,period,season) %>%
  mutate(rank = rank(-GenConc)) %>%
  mutate(P = 100 * (rank / (length(GenConc) + 1)))


res_ecdf %>% ggplot(aes(x = P, y = GenConc, color = period))+
  geom_line()+
  xlab("% Time concentration equalled or exceeded")+
  ylab("Concentration (mg/L)")+
  facet_wrap(pollutant~season,scales = "free")

################################################################################
# count analysis
res %>%
  group_by(period,season,pollutant) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from= c(period,pollutant), values_from = c(count))

################################################################################
# weighted means
data.frame <- res %>%
  group_by(period,season,pollutant) %>%
  summarise(fwmc = (weighted.mean(GenConc,Q))) %>%
  pivot_wider(names_from= c(period), values_from = c(fwmc)) %>%
  mutate(per_change = round(((post - pre)/pre)*100)) %>%
  pivot_wider(names_from= c(pollutant), values_from = c(pre,post,per_change))

data.frame <- data.frame[,c(1,2,5,8,3,6,9,4,7,10)]

write.csv(data.frame, file = paste("data/process/frozen_soil/r_output/",site,".csv",sep = ""),row.names = FALSE)


