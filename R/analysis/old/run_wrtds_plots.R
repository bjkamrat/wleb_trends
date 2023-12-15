################################################################################
# Author: Brock Kamrath
# Date 31 July 2023
# Objective: Write a function that runs eList for WRTDS Kalman, then outputs daily
# and annual loads
################################################################################

# Load in libraries
library(readxl)
library(tidyverse)
library(EGRET)

# set seed
set.seed(522)

# load in general data
gen_dat_2 <- read_excel("data/wrtds/input/gen_dat_V2.xlsx")

# set parameter
poi <-  "tp"

# Create empty lists for future use
df_conc_list <- NULL
df_flux_list <- NULL

################################################################################
# set path for inputs to egret - Blanchard
datafile <- paste0("./data/wrtds/output/eList/",gen_dat_2$station[3],".",poi,".RData",sep = "")

# load in datafile
load(datafile)

eList_bla <- setPA(eList)

###################################################################################
# set path for inputs to egret - Honey
datafile <- paste0("./data/wrtds/output/eList/",gen_dat_2$station[5],".",poi,".RData",sep = "")

# load in datafile
load(datafile)

eList_hon <- setPA(eList)

################################################################################
# set path for inputs to egret - Maumee
datafile <- paste0("./data/wrtds/output/eList/",gen_dat_2$station[1],".",poi,".RData",sep = "")

# load in datafile
load(datafile)

eList_mau <- setPA(eList)

################################################################################
# set path for inputs to egret - Raisin
datafile <- paste0("./data/wrtds/output/eList/",gen_dat_2$station[6],".",poi,".RData",sep = "")

# load in datafile
load(datafile)

eList_rai <- setPA(eList)

################################################################################
# set path for inputs to egret - Sandusky
datafile <- paste0("./data/wrtds/output/eList/",gen_dat_2$station[4],".",poi,".RData",sep = "")

# load in datafile
load(datafile)

eList_san <- setPA(eList)

################################################################################
# set path for inputs to egret - Tiffin
datafile <- paste0("./data/wrtds/output/eList/",gen_dat_2$station[2],".",poi,".RData",sep = "")

# load in datafile
load(datafile)

eList_tif <- setPA(eList)

################################################################################
colors <- colorRampPalette(c("white","black"))

# plot interannual variation
plotContours(eList_mau, 2009, 2021, 5, 1000,
             contourLevels = seq(0,0.7,0.1),
             tinyplot = TRUE)

plotContours(eList_san,2009, 2021, 1, 200,
             contourLevels = seq(0,0.9,0.1),tcl=0.2,tick.lwd=1.5,
             tinyplot = TRUE)

plotContours(eList_rai,2009, 2021, 2, 100,
             contourLevels = seq(0,0.4,0.05),tcl=0.2,tick.lwd=1.5,
             tinyplot = TRUE)

plotContours(eList_tif,2009, 2021, 0.5, 50,
             contourLevels = seq(0,0.6,0.1),tcl=0.2,tick.lwd=1.5,
             tinyplot = TRUE)

plotContours(eList_bla,2009, 2021, 0.5, 50,
             contourLevels = seq(0,0.7,0.1),tcl=0.2,tick.lwd=1.5,
             tinyplot = TRUE)


