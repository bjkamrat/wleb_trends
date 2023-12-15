#####################################################################################
# This script contains the functions to run the FN_2Q approach for decomposing
# WRTDS flow-normalized (FN) fluxes into low-flow and high-flow components.
# The FN_2Q approach is formally documented in the following journal publication:
# Zhang, Q., J.S. Webber, D.L. Moyer, and J.G. Chanat, 2021,
# An approach for decomposing estimated river water-quality trends into different flow classes,
# Science of the Total Environment 755: 143562, doi: 10.1016/j.scitotenv.2020.143562.

#####################################################################################
# This script was written by Qian Zhang (UMCES/CBPO) and James S. Webber (USGS).
# Emails: qzhang@chesapeakebay.net; jwebber@usgs.gov.
# Date of version: July 23, 2021.

#####################################################################################
# The script is designed to further process an existing WRTDS workspace (called "eList"),
# which stores the flow-normalized (FN) estimates from the original FN procedure in WRTDS.
# To obtain such an "eList" for a specific data set, the standard WRTDS method needs
# to be run based on a record of water-quality samples and a record of daily flow discharges.
# For WRTDS workflow and further information, refer to the following two resources:
# (1) EGRET website: http://usgs-r.github.io/EGRET/index.html.
# (2) EGRET manual: https://pubs.usgs.gov/tm/04/a10/.

#####################################################################################
# This is the main function to decomposing the WRTDS daily estimates to low-flow and high-flow components.
# High flows = daily flows higher than or equal to the median of all flows that have occurred on a specific calendar date.
# Low flows = daily flows lower than the median of all flows that have occurred on a specific calendar date.
# By default, the cutoff parameter is set to be 0.5 (i.e., 50th percentile).
# Thus, the two flow classes each contain 50% of discharge values for each calendar date.
# Users may modify this parameter to accommodate other definition of high-flow and low-flow classes. For example,
# a cutoff of 0.6 = 60th percentile. This results in 60% and 40% of values in the low-flow and high-flow classes, respectively.
# We strongly recommend at least several discharge values in each flow class in order to appropriately implement the FN computation.
# For example, for a 20-yr record, a cutoff of 0.95 would result in only one value (5% x 20 = 1) in the high-flow class.
# Similarly, a cutoff of 0.05 would result in only one value in the low-flow class. Neither is appropriate for running this approach.

Estby2Flows <- function(eList, cutoff=0.6)
{
  # obtain data from eList
  localDaily <- getDaily(eList)
  localINFO <- getInfo(eList)
  localsurfaces <- getSurfaces(eList)

  # define flow pdfs
  allLogQsByDayOfYear <- bin_Qs(localDaily)      # all flows
  allLogQsByDayOfYear1 <- allLogQsByDayOfYear    # high flows
  allLogQsByDayOfYear2 <- allLogQsByDayOfYear    # low flows
  Nq <- data.frame(Day=1:366, TotalQ=NA, HighQ=NA, LowQ=NA)

  for(i in 1:366)
  {
    Q <- allLogQsByDayOfYear[[i]]                # flows for day i
    # use ranking to separate the flows
    allLogQsByDayOfYear1[[i]] <- Q[order(Q,decreasing=T)][1:ceiling((1-cutoff)*length(Q))]  # values for high flows
    allLogQsByDayOfYear2[[i]] <- Q[order(Q,decreasing=F)][1:floor(cutoff*length(Q))]        # values for low flows
    Nq$TotalQ[i] <- length(Q)
    Nq$HighQ[i] <- length(allLogQsByDayOfYear1[[i]])
    Nq$LowQ[i] <- length(allLogQsByDayOfYear2[[i]])
    print(paste("day", i, "has", length(Q), "flows"))        # number of flows for day i
    print(paste("day", i, "has", Nq$HighQ[i], "high flows")) # number of flows for day i
    print(paste("day", i, "has", Nq$LowQ[i], "low flows"))   # number of flows for day i
  }

  # get conc and flux from surface for high flows (above cutoff, which by default is the 50th percentile)
  concFlux_list <- getConcFluxFromSurface(eList, allLogQsByDayOfYear1,localDaily = localDaily, localsurfaces = localsurfaces)
  localDaily$FNConc.HIGH <- as.numeric(tapply(concFlux_list[["allConcReplicated"]],
                                              concFlux_list[["allDatesReplicated"]], "mean"))
  localDaily$FNFlux.HIGH <- as.numeric(tapply(concFlux_list[["allFluxReplicated"]],
                                              concFlux_list[["allDatesReplicated"]], "mean"))

  # get conc and flux from surface for low flows (below cutoff, which by default is the 50th percentile)
  concFlux_list <- getConcFluxFromSurface(eList, allLogQsByDayOfYear2, localDaily = localDaily, localsurfaces = localsurfaces)
  localDaily$FNConc.LOW <- as.numeric(tapply(concFlux_list[["allConcReplicated"]],
                                             concFlux_list[["allDatesReplicated"]], "mean"))
  localDaily$FNFlux.LOW <- as.numeric(tapply(concFlux_list[["allFluxReplicated"]],
                                             concFlux_list[["allDatesReplicated"]], "mean"))

  # multiply FN flux (not conc.) by proportion, so their sum equals localDaily$FNFlux (original FNFlux)
  localDaily <- merge(localDaily, Nq, by.x="Day", by.y="Day", all.x=TRUE)
  localDaily <- localDaily[order(localDaily$Julian),]
  head(localDaily)
  localDaily$FNFlux.HIGH <- localDaily$FNFlux.HIGH / localDaily$TotalQ * localDaily$HighQ
  localDaily$FNFlux.LOW <- localDaily$FNFlux.LOW / localDaily$TotalQ * localDaily$LowQ

  # check results
  table(round(localDaily$FNFlux.HIGH + localDaily$FNFlux.LOW - localDaily$FNFlux)) # always 0
  table(Nq$HighQ + Nq$LowQ-Nq$TotalQ)  # always 0
  table(Nq$HighQ - Nq$LowQ)            # always 1 or 0

  # add some descriptions
  staid <- localINFO$site_no
  parameter <- localINFO$paramShortName
  localDaily$STAID <- staid
  localDaily$Parameter <- parameter
  localDaily$Year <- floor(localDaily$DecYear)
#
#   # seasonal additions
#   seasons = function(x){
#     if(x %in% 3:7) return("Spring")
#     if(x %in% 8:11) return("Offseason")
#     if(x %in% c(12,1,2)) return("Winter")
#
#   }
#
#   localDaily$Season <-  sapply(localDaily$Month, seasons)
#   localDaily$Szn_yr <-  paste(localDaily$Season, localDaily$Year, sep = "-")
#   localDaily$SznSeq <- rep(1000,nrow(localDaily))
#
#   j <- 1000
#
#   for(i in 2:nrow(localDaily)){
#     if(localDaily$Season[i] != localDaily$Season[i-1]){
#       j <- j+1
#     }
#     localDaily$SznSeq[i] <- j
#   }

  # output
  localDaily <- localDaily[, c("DecYear","Q","ConcDay","FluxDay","FNConc","FNFlux",
                               "FNConc.HIGH","FNFlux.HIGH","FNConc.LOW","FNFlux.LOW",
                               "Date","waterYear","Year","Month","Day","TotalQ","HighQ","LowQ","MonthSeq","STAID","Parameter")]
  return(localDaily)
}

#####################################################################################
# The following functions are provided to produce summary tables of monthly and annual estimates
# These functions only need read in the "Daily.out" object, which is the output from the "Estby2Flows" function.
# Users may modify the code to produce flux estimates for other temporal scales, e.g., seasonal.

# Produce a table for monthly estimates
monthly.est <- function(Daily.out)
{
  d <- Daily.out[, c("DecYear","Q","FluxDay","FNConc","FNFlux",
                     "FNConc.HIGH","FNFlux.HIGH","FNConc.LOW","FNFlux.LOW",
                     "waterYear","Year","Month","MonthSeq")]
  Monthly.out <- aggregate(d, by=list(Daily.out$MonthSeq), FUN="mean")[,-1]
  Monthly.out$STAID <- Daily.out$STAID[1]
  Monthly.out$Parameter <- Daily.out$Parameter[1]
  return(Monthly.out)
}


# # Produce a table for seasonal estimates
# seasonal.est <- function(Daily.out)
# {
#   d <- Daily.out[, c("DecYear","Q","FluxDay","FNConc","FNFlux",
#                      "FNConc.HIGH","FNFlux.HIGH","FNConc.LOW","FNFlux.LOW",
#                      "waterYear","Year","Season","Month","SznSeq","MonthSeq")]
#   seasonal.out <- aggregate(d, by=list(Daily.out$SznSeq), FUN="mean")[,-1]
#   seasonal.out$STAID <- Daily.out$STAID[1]
#   seasonal.out$Parameter <- Daily.out$Parameter[1]
#   return(seasonal.out)
# }
#
#

# Produce a table for annual estimates (calender year)
annual.est <- function(Daily.out)
{
  d <- Daily.out[, c("DecYear","Q","FluxDay","FNConc","FNFlux",
                     "FNConc.HIGH","FNFlux.HIGH","FNConc.LOW","FNFlux.LOW","Year")]
  Annual.out <- aggregate(d, by=list(Daily.out$Year), FUN="mean")[,-1]
  Annual.out$STAID <- Daily.out$STAID[1]
  Annual.out$Parameter <- Daily.out$Parameter[1]
  Annual.out <- Annual.out[-c(1, length(Annual.out$DecYear)),]  # remove incomplete years (first and last)
  return(Annual.out)
}

# Produce a table for annual estimates (water year)
annualWY.est <- function(Daily.out)
{
  d <- Daily.out[, c("DecYear","Q","FluxDay","FNConc","FNFlux",
                     "FNConc.HIGH","FNFlux.HIGH","FNConc.LOW","FNFlux.LOW","waterYear")]
  AnnualWY.out <- aggregate(d, by=list(Daily.out$waterYear), FUN="mean")[,-1]
  AnnualWY.out$STAID <- Daily.out$STAID[1]
  AnnualWY.out$Parameter <- Daily.out$Parameter[1]
  AnnualWY.out <- AnnualWY.out[-length(AnnualWY.out$DecYear),] # remove incomplete year
  return(AnnualWY.out)
}

#####################################################################################
# This function is developed to report the uncertainties on FN and FN_2Q estimates.
# The output of this function are three lists, each having 100 columns.
# List 1: FNFlux.wBT = the overall FN flux,
# List 2: FNFlux.LOW.wBT = the low-flow FN flux, and
# List 3: FNFlux.HIGH.wBT = the high-flow FN flux.
# Note: This function may take one to a few hours to run,
# which depends on the number of bootstrap replicates (nBoot, default = 100).
# For the WRTDS wBT uncertainty approach, refer to the following two resources:
# (1) Hirsch et al. (2015): https://doi.org/10.1016/j.envsoft.2015.07.017.
# (2) EGRETci R package: https://github.com/USGS-R/EGRETci.

FN.2Q.wBT <- function(eList, nBoot = 100, bootBreak = 39, blockLength = 100)
{
  library(EGRET)
  library(EGRETci)
  library(truncnorm)
  library(binom)
  library(foreach)
  library(doParallel)
  library(iterators)

  # Obtain data from eList
  localDaily <- getDaily(eList)
  localINFO <- getInfo(eList)
  localSample <- getSample(eList)

  # Initiate data frames to store uncertainty estimates
  FNFlux.wBT <- as.data.frame(matrix(NA, ncol = nBoot, nrow = length(localDaily$Date)))
  FNFlux.LOW.wBT <- as.data.frame(matrix(NA, ncol = nBoot, nrow = length(localDaily$Date)))
  FNFlux.HIGH.wBT <- as.data.frame(matrix(NA, ncol = nBoot, nrow = length(localDaily$Date)))

  # Run wBT analysis
  for (i in 1:nBoot)
  {
    cat(paste("\n This is wBT run", i, "\n"))
    Sample.new <- blockSample(localSample = localSample, blockLength = blockLength)
    eList.new <- mergeReport(INFO = localINFO, Daily = localDaily, Sample = Sample.new, verbose = FALSE)
    eList.new <- modelEstimation(eList.new, windowY = 7, windowQ = 2, windowS = 0.5,
                                 minNumObs = 100, minNumUncen = 50, edgeAdjust = TRUE, verbose = FALSE)
    FN2Q.new <- Estby2Flows(eList.new, cutoff=0.5)
    FNFlux.wBT[,i] <- eList.new$Daily$FNFlux
    FNFlux.LOW.wBT[,i] <- FN2Q.new$FNFlux.LOW
    FNFlux.HIGH.wBT[,i] <- FN2Q.new$FNFlux.HIGH
    if(sum(FN2Q.new$FNFlux - eList.new$Daily$FNFlux)==0){cat("\n Mass balance is maintained in this run. \n")}
    if(sum(FN2Q.new$FNFlux - eList.new$Daily$FNFlux)>0){cat("\n Mass balance is not maintained in this run. \n")}
    rm(Sample.new, eList.new, FN2Q.new)
  }

  return(list(FNFlux.wBT, FNFlux.HIGH.wBT, FNFlux.LOW.wBT))
}

#####################################################################################
# This function reports out the confidence interval and the likelihood of trends
# based on the object returned from the wBT function (call it wBT.output)
# The output of this function are also three lists.
# List 1: FNFlux.CI = the 95% CI in the overall FN flux,
# List 2: FNFlux.LOW.CI = the 95% CI in the low-flow FN flux, and
# List 3: FNFlux.HIGH.CI = the 95% CI in the high-flow FN flux.

FN.2Q.wBT.report <- function(wBT.output = wBT.output, eList)
{
  # Compute wBT annual estimates
  d <- data.frame(waterYear=eList$Daily$waterYear, wBT.output[[1]])
  d1 <- aggregate(d, by=list(d$waterYear), FUN="sum")[,-1]   # kg/yr
  d1$waterYear <- aggregate(d$waterYear, by=list(d$waterYear), FUN="mean")[,-1]
  d1 <- d1[-length(d1$waterYear),]   # remove incomplete year
  FNFlux.annual.wBT <- d1

  d <- data.frame(waterYear=eList$Daily$waterYear, wBT.output[[2]])
  d1 <- aggregate(d, by=list(d$waterYear), FUN="sum")[,-1]   # kg/yr
  d1$waterYear <- aggregate(d$waterYear, by=list(d$waterYear), FUN="mean")[,-1]
  d1 <- d1[-length(d1$waterYear),]   # remove incomplete year
  FNFlux.HIGH.annual.wBT <- d1

  d <- data.frame(waterYear=eList$Daily$waterYear, wBT.output[[3]])
  d1 <- aggregate(d, by=list(d$waterYear), FUN="sum")[,-1]   # kg/yr
  d1$waterYear <- aggregate(d$waterYear, by=list(d$waterYear), FUN="mean")[,-1]
  d1 <- d1[-length(d1$waterYear),]   # remove incomplete year
  FNFlux.LOW.annual.wBT <- d1

  # Report the 90% confidence interval of annual trends
  CI <- function(x) { return(quantile(x, probs=c(0.05, 0.95))) }
  FNFlux.CI <- data.frame(waterYear=FNFlux.annual.wBT$waterYear, t(apply(FNFlux.annual.wBT[,-1], 1, CI)))
  FNFlux.LOW.CI <- data.frame(waterYear=FNFlux.LOW.annual.wBT$waterYear, t(apply(FNFlux.LOW.annual.wBT[,-1], 1, CI)))
  FNFlux.HIGH.CI <- data.frame(waterYear=FNFlux.HIGH.annual.wBT$waterYear, t(apply(FNFlux.HIGH.annual.wBT[,-1], 1, CI)))
  names(FNFlux.CI) <- c("waterYear", "CI.low", "CI.high")
  names(FNFlux.LOW.CI) <- c("waterYear", "CI.low", "CI.high")
  names(FNFlux.HIGH.CI) <- c("waterYear", "CI.low", "CI.high")

  # Report the 90% CI of the period-of-record change in the annual FN estimates
  CI.change <- function(x = FNFlux.annual.wBT)
  {
    First <- x[1,]
    Last <- x[length(x$waterYear),]
    return((CI(Last - First)))
  }

  print(paste("The 90% confidence interval (in kg/yr) of the annual FN estimates:"))
  print(CI.change(x = FNFlux.annual.wBT))

  print(paste("The 90% confidence interval (in kg/yr) of the annual, high-flow FN estimates:"))
  print(CI.change(x = FNFlux.HIGH.annual.wBT))

  print(paste("The 90% confidence interval (in kg/yr) of the annual, low-flow FN estimates:"))
  print(CI.change(x = FNFlux.LOW.annual.wBT))

  # Report the likelihood of a downward trend in the annual FN estimates over the period of record
  Likelihood.down <- function(x = FNFlux.annual.wBT)
  {
    First <- x[1,]
    Last <- x[length(x$waterYear),]
    Change <- Last-First
    return(signif(mean(Change < 0),2)*100)
  }

  print(paste("The likelihood (in percent) of a downward trend in the annual FN estimates:"))
  print(Likelihood.down(x = FNFlux.annual.wBT))

  print(paste("The likelihood (in percent) of a downward trend in the annual, high-flow FN estimates:"))
  print(Likelihood.down(x = FNFlux.HIGH.annual.wBT))

  print(paste("The likelihood (in percent) of a downward trend in the annual, low-flow FN estimates:"))
  print(Likelihood.down(x = FNFlux.LOW.annual.wBT))

  # Return the 95% confidence interval of estimates for each year
  return(list(FNFlux.CI, FNFlux.HIGH.CI, FNFlux.LOW.CI))
}

# End of script

