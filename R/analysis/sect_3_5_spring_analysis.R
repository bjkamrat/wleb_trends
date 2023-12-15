################################################################################
# create table results for EGRET for each site and pollutant
# results will be annual results
################################################################################


#load in libraries
library(EGRET)
library(tidyverse)
library(ggplot2)
library(rio)
library(readxl)


# load in general datafile
# set seed
set.seed(522)

# set poi
poi <- "tp"

# load in general data
gen_dat_2 <- read_excel("data/wrtds/input/gen_dat.xlsx")

# #set start and end years
# start_year <- 2009 # Scenario 1: 2009-2021
# end_year <- 2021
#

conc <- NULL
flux <- NULL
returnDF <- NULL

# start year must be altered for the last season
for(j in 1:nrow(gen_dat_2)){
    # set path for inputs to egret
    datafile <- paste0("./data/wrtds/output/eList/",gen_dat_2$station[j],".",poi,".RData",sep = "")

    # load in datafile
    load(datafile)

    eList_sen <- setPA(eList, paStart = 3, paLong = 5)

    returnDF[[j]] <- tableResults(eList_sen, fluxUnit = 4)

    colnames(returnDF[[j]]) <- c("Year", "q_cms","conc","fn_conc","flux","fn_flux")

    returnDF[[j]] <- returnDF[[j]] %>%
      mutate(flux = flux,
             fn_flux = fn_flux,
             station = gen_dat_2$station[j],
             area = gen_dat_2$area_km2[j],
             fn_yield = (fn_flux*1000)/area) # flux and fn flux in metric tons per year

    ########################################################################
    # view C-Q relationships
    q5 <- round(quantile(eList$Daily$Q,0.05),1)
    q1 <- round(quantile(eList$Daily$Q,0.15),1)
    q2 <- round(quantile(eList$Daily$Q,0.60),1)
    q3 <- round(quantile(eList$Daily$Q,0.85),1)
    q95 <- round(quantile(eList$Daily$Q,0.95),1)

    date1 <- "2009-05-15"
    date2 <- "2015-05-15"
    date3 <- "2021-05-15"
    qLow <- q5
    qHigh <- q95

    plotConcQSmooth(eList, date1, date2, date3, qLow, qHigh, legendTop = 0.3)

    yearEnd <- 2008
    yearStart <- 2022

    # January plot
    centerDate <- "05-15"
    plotConcTimeSmooth(eList, q1, q2, q3, centerDate,
                       yearStart, yearEnd,
                       legendTop = 0.4,logScale=TRUE)


    #######################################################################
    # save table changes for 2008, 2015, and 2022
    conc[[j]] <- tableChangeSingle(eList_sen, yearPoints=c(2008,2015,2022)) %>%
      mutate(station = gen_dat_2$station[j])

    colnames(conc[[j]]) <- c("span_start","span_end","unit_change","unit_slope","per_change","per_slope","station")

    conc[[j]]$scenario <- c("pre","all","post")

    flux[[j]] <- tableChangeSingle(eList_sen, yearPoints=c(2008,2015,2022),fluxUnit = 4,flux = TRUE) %>%
      mutate(station = gen_dat_2$station[j])

    colnames(flux[[j]]) <- c("span_start","span_end","unit_change","unit_slope","per_change","per_slope", "station")

    flux[[j]]$scenario <- c("pre","all","post")
}

annual <- as.data.frame(do.call(rbind,returnDF))

delta_conc <- as.data.frame(do.call(rbind,conc))
delta_conc$type <- "conc"
delta_conc$season <- "spring"

delta_flux <- as.data.frame(do.call(rbind,flux))
delta_flux$type <- "flux"
delta_flux$season <- "spring"

# Lets write some table to mess around with in excel
# annual values
write.csv(annual, file=paste("data/process/seasonal/spring/",poi,"_overall_results.csv",sep = ""),
            row.names = FALSE)

# concrentration changes
write.csv(delta_conc, file=paste("data/process/seasonal/spring/",poi,"_conc_results.csv",sep = ""),
            row.names = FALSE,quote=FALSE)

# flux changes
write.csv(delta_flux, file=paste("data/process/seasonal/spring/",poi,"_flux_results.csv",sep = ""),
            row.names = FALSE,quote=FALSE)

