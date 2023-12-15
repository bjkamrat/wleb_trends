################################################################################
# title: Total change slopes
# Objective: Seasonal Analysis
################################################################################
# add source
source("R/funcs/filter_setPA.R")

# set seed
set.seed(522)

# load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(EGRET)

# Prep to load in eList
# load in general data
gen_dat_2 <- read_excel("data/wrtds_input/gen_dat.xlsx")

# set parameter
poi <-  "srp"

# Create empty lists for future use
df_conc_list <- NULL
df_flux_list <- NULL

  # set path for inputs to egret
  datafile <- paste0("./data/wrtds_output/eList/",gen_dat_2$station[4],".",poi,".RData",sep = "")

  # load in datafile
  load(datafile)

  q1 <- round(quantile(Daily$Q,0.15),1)
  q2 <- round(quantile(Daily$Q,0.5),1)
  q3 <- round(quantile(Daily$Q,0.85),1)


  yearStart <- 2008
  yearEnd <- 2022


  par(mar=c(3,3,0.2,0.2), # whitespace around the plots
      oma=c(1,1,3,1), # outer margin
      mgp=c(2,0.5,0), # spacing between the label numbers and plots
      mfcol = c(2,2)) # rows/columns

  centerDate <- "01-01"
  plotConcTimeSmooth(eList, q1, q2,q3, centerDate,qUnit = 2,tinyPlot = TRUE, concMax=0.3,
                     yearStart, yearEnd, cex.legend = 0.9, printTitle = FALSE,
                     showXLabels=FALSE,showXAxis=FALSE,
                     showYLabels=TRUE,customPar=TRUE, printLegend = FALSE, usgsStyle = TRUE)

  centerDate <- "09-01"

  plotConcTimeSmooth(eList, q1, q2,q3, centerDate,qUnit = 2, tinyPlot = TRUE,concMax=0.3,
                     yearStart, yearEnd, cex.legend = 0.9,printTitle = FALSE,
                     printLegend = TRUE,customPar = TRUE, usgsStyle = TRUE)

  centerDate <- "05-01"

  plotConcTimeSmooth(eList, q1, q2,q3, centerDate,qUnit = 2, tinyPlot = TRUE, concMax=0.3,
                     yearStart, yearEnd, cex.legend = 0.9,printTitle = FALSE,showYLabels=FALSE,
                     showXLabels=FALSE,showXAxis=FALSE, showYAxis=FALSE, customPar = TRUE, printLegend = FALSE, usgsStyle = TRUE)


  centerDate <- "11-01"

  plotConcTimeSmooth(eList, q1, q2,q3, centerDate,qUnit = 2, tinyPlot = TRUE, concMax=0.3,
                     yearStart, yearEnd, cex.legend = 0.9,printTitle = FALSE,
                     printLegend = FALSE, customPar = TRUE, showYLabels=FALSE, showYAxis=FALSE, usgsStyle = TRUE)

  mtext("Blanchard River @ Finlay, OH", outer=TRUE, font=2)


