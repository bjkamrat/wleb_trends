#####################################################################################
# objective: pull total annual flow-normalized yield (flux per unit area) for a single year
# Auther: Brock Kamrath
####################################################################################

# Load in libraries
library(readxl)
library(tidyverse)
library(EGRET)
library(rio)

# set seed
set.seed(522)

# load in general data
gen_dat <- read_excel("data/wrtds_input/gen_dat_V2.xlsx")

# set parameter
poi <-  "tp"

# Create empty lists for future use
df_flux <- NULL
df_flux_list <- NULL

# run for loop for each site
for(i in 1:nrow(gen_dat)){

  # set path for inputs to egret
  data <- import(paste0("./data/wrtds_output/eList_k/annual/",poi,"/",gen_dat$station[i],".csv",sep = ""))

  df_flux$station <- gen_dat$station[i]
  df_flux$FNyield08 <- ((data$FNFlux[which(data$waterYear == 2010)])*365)/gen_dat$area_ha[i]

  df_flux_list[[i]] <- df_flux

}

df <- do.call(rbind, df_flux_list)
write.csv(df, "data/wrtds_output/trends/annual/srp/yield_10.csv", row.names = FALSE)
