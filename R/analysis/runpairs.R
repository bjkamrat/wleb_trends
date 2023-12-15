library(tidyverse)
library(readxl)
library(foreach)
library(doParallel)
library(iterators)
library(EGRET)
library(EGRETci)

# load in general datafile
# set seed
set.seed(522)

# set poi
poi <- "tp"

# load in general data
gen_dat <- read_excel("data/wrtds/input/gen_dat.xlsx")

# set path for inputs to egret
datafile <- paste0("./data/wrtds/output/eList/",gen_dat$station[1],".",poi,".RData",sep = "")

# load in datafile
load(datafile)


###############################################################################
nBoot <- 100
blockLength <- 200
coreOut <- 1 #Number of cores to leave out of processing tasks

widthCI <- 90
ciLower <- (50-(widthCI/2))/100
ciUpper <- (50+(widthCI/2))/100
probs <- c(ciLower,ciUpper)


pairResults1 <- runPairs(eList, year1 = 2015, year2 = 2022, windowSide = 0,
                         paStart = 10, paLong = 12)


bootPairOut <- runPairsBoot(eList, pairResults1)

plotHistogramTrend(eList,bootPairOut, caseSetUp = NA)

