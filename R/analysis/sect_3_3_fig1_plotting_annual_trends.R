################################################################################
# title: Plotting FN
# Objective: Plot trends and loads from 2012 through 2022
################################################################################

# load libraries

library(tidyverse)
library(ggplot2)
library(readxl)
library(gridExtra)

# load in gen_funcs
gen_dat <- read_excel("data/wrtds/input/gen_dat.xlsx")

# set parameters:
period <- "annual"
poi <- "srp"

plots <- list()

for(i in 1:nrow(gen_dat)){
site <- gen_dat$station[i]

area <- as.numeric(gen_dat[gen_dat$station == site,6])

# create setup for single file then make it for all sites

# read in raw eList-K data for site and pollutant of interest
raw_data <- read.csv(paste0("data/wrtds/output/eList_k/",period,"/",poi,"/",site,".csv",sep = ""))

# convert Flux values from kg/d to kg/ha/yr
data <- raw_data %>%
  mutate(DecYear = round(DecYear,0),
         GenFlux = (365*GenFlux)/area,
         FNFlux = (365*FNFlux)/area,
         tya = zoo::rollmean(GenFlux, k = 5, fill = NA,align='right'))%>%
  filter(DecYear >= 2008 & DecYear<= 2022)



plots[[i]] <- data %>%
  ggplot() +
    geom_col(aes(x = DecYear, y = GenFlux), fill = "gray80") +
    geom_point(aes(x = DecYear, y = FNFlux), color = "firebrick3",size = 1.1)+
    geom_line(aes(x = DecYear, y = FNFlux), color = "firebrick3")+
    geom_point(aes(x = DecYear, y = tya), color = "dodgerblue3",size = 1.1,shape = 18)+
    geom_line(aes(x = DecYear, y = tya), color = "dodgerblue3")+
  labs(x = "", y = "", title = site)+
  scale_x_continuous(breaks=seq(2008, 2022, 1),guide = guide_axis(angle = 60))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(data$GenFlux)*1.1))+
  theme_bw()+
  theme(text = element_text(size=8))


}


n <- length(plots)
nCol <- floor(sqrt(n))
p1 <- do.call("grid.arrange", c(plots, ncol=1))

ggsave(plot = p1,file = paste0("results/figs/",period,"/",poi,".tiff",sep = ""), width = 4, height = 8, units = "in", dpi = 500)

