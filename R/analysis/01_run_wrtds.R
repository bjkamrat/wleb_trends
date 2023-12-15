# This script pulls in two functions, convert2sample and obtain_daily, and uses the
# general data file to run wrtds for each site and pollutant of interest
# the Rdata produced, which includes the wrtds model, is then saved in the
# data/wrtds/output file.

library(devtools)
library(EGRET)

# function to convert raw downloaded data into daily flow weighted concentration
# in a format that is readable by EGRET.
source("R/funcs/convert2sample.R")

# function to obtain daily flow values for the site specified in the stationID
source("R/funcs/obtain_daily.R")

#convert2sample(poi)

set.seed(522)

# load in gen_dat
gen_dat <- read_excel("data/wrtds/input/gen_dat.xlsx")

# set parameter values
poi <- "tss"
stationID <- "Maumee"

parametercd <-  "80180" # parametercd for SRP = 00671, TP = 00665, NO3 = 00631, TSS = 80180

# Clip general data to specified station
siteID <- gen_dat %>%
  filter(station == stationID) %>%
  .$usgs %>%
  .[1]

# set path for inputs to egreat
filePath <- paste0("./data/wrtds/input/",poi,"/",sep ="")
fileName <- paste0(stationID,".csv",sep = "")

# readUserSample
Sample <- readUserSample(filePath, fileName)

## Read in flow data ##
#read in flow data directly from USGS
Daily <- obtain_daily(site = stationID)

# # # read user daily if needed
#   filePath <- paste0("./data/wrtds_input/q/",sep ="")
#   fileName <- paste0(stationID,".csv",sep = "")
#   Daily <- readUserDaily(filePath, fileName)

INFO<- readNWISInfo(siteID, parametercd,interactive = FALSE)

# Merge discharge with sample data:
eList <- mergeReport(INFO, Daily, Sample)


############################
# Check sample data:
boxConcMonth(eList)
boxQTwice(eList)
plotConcTime(eList)
plotConcQ(eList)
multiPlotDataOverview(eList)
############################

# run the modelEstimation
eList <- modelEstimation(eList)

errorStats(eList)

################################################################################
# set values and save eList
eList$INFO$staAbbrev <- stationID
eList$INFO$constitAbbrev <- poi

# save results
savePath <- paste("data/wrtds/output/eList/")
saveResults(savePath, eList)
################################################################################
# plot flow
plotFlowSingle(eList, istat=5)

# plot flux and conc trends
tableResults(eList)
plotConcHist(eList)
plotFluxHist(eList)
tableChange(eList, yearPoints = c(2009,2015,2021))

eListOut <- runSeries(eList, windowSide = 6, verbose = FALSE)
tableResults(eListOut)
plotConcHist(eListOut)
plotFluxHist(eListOut)
tableChange(eListOut, yearPoints = c(1985, 1995, 2010))

# check for winter values
eList_w <- setPA(eList,paStart = 12, paLong = 3)
plotConcHist(eList_w)
plotFluxHist(eList_w)
tableChange(eList_w, yearPoints = c(2008,2015,2022))

#plot difference contours - intraannual
plotDiffContours(eList, year0=2015,year1=2022,
                 qBottom=0.2,qTop=100,maxDiff = 0.3)

#plot contours - interannual
clevel<-seq(0,1,0.025)
plotContours(eList, yearStart=2009,yearEnd=2021,qBottom=5,qTop=200,
             contourLevels = clevel)

pairOut_1 <- runPairs(eList, year1 = 2015, year2 = 2022, windowSide = 0)
plotMonthTrend(pairOut_1)

