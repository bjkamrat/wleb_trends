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


# read in data
raw.df <- import("data/wrtds/output/eList_k/daily/srp/Sandusky.csv")

# plot data to ID differences in precipitation
df <- raw.df %>%
  mutate(Qcum = cumsum(Q),
         Fluxcum = cumsum(GenFlux),
         period = ifelse(Date >= "2015-07-03", "post","pre")) %>%
  select(period, Month, waterYear,Q,GenConc,GenFlux,Qcum,Fluxcum)


lc.y <- Lc(df$Q)
plot(lc.y, col = 2)


df_period <-  df %>%
  filter(period == "pre")

ineq(df_period$GenFlux,type="Gini")

gini<-function(year){
  df_sub<-df[df$waterYear==year,]
  Qgin <- ineq(df_sub$Q,type="Gini")
  Fluxgin <- ineq(df_sub$GenFlux,type="Gini")
  rel_in <- Fluxgin/Qgin
  outputs <- c(Qgin = Qgin,Fluxgin = Fluxgin,rel_in = rel_in)
  return(outputs)
}

year<-unique(df$waterYear)

gini_results<-as.data.frame(t(sapply(year,gini)))

gini_results$year <- year

ggplot(gini_results,aes(x = year,y=rel_in))+
  geom_point()+
  geom_smooth()


# what are the flow-weight mean annual concentrations for the period?
df %>%
  group_by(waterYear) %>%
  summarise(fwmc = weighted.mean(GenConc,Q)) %>%
  ggplot(aes(x = waterYear, y = fwmc))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm")

################################################################################
# Fourier analysis (try this - might not be necessary, but worth a shot)




