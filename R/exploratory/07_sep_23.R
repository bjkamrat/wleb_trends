###############################################################################
#  07_sep_23.R
#  Details: combine seasonal outputs into single dataframe.
#
#
#############################################################################

# Load in libraries
library(rio)
library(tidyverse)
library(ggplot2)
library(dplyr)
library("RColorBrewer")

# set parameters
params <- import("data/combo_file.csv")

poi <- "srp"

returnDF <- NULL

for(i in 1:nrow(params)){

    returnDF[[i]] <- import(paste0("data/process/seasonal/",params[i,1],"/",poi,"_",params[i,2],"_results.csv",sep = ""))


}

df <- plyr::ldply(returnDF, data.frame)
df$poi <- "srp"

write.csv(delta_flux, file=paste("data/process/seasonal/combo/",poi,"_results.csv",sep = ""),
          row.names = FALSE,quote=FALSE)
