library(readxl)
library(tidyverse)

convert2sample <- function(poi){

  gen_dat <- read_excel("data/wrtds/input/gen_dat.xlsx")

  folderPath <- paste0("./data/raw/",poi,sep = "")
  fileName <- grep(".xlsx", dir(folderPath), value = TRUE)
  filePath <- paste(folderPath, fileName, sep = "/")

  for(i in 1:nrow(gen_dat)){
  raw <- read_excel(filePath, sheet = paste0(gen_dat$station[i],"_samples",sep=""))

  colnames(raw) <- c("Datetime","rq","q_cfs","rconc","conc_mgL")

  fwmc <- raw %>%
    mutate(date = as.Date(Datetime)) %>%
    filter(date >= gen_dat$start_date[i]) %>%
    group_by(date) %>%
    summarise(conc = weighted.mean(conc_mgL,q_cfs,na.rm = TRUE)) %>%
    mutate(remark = if_else(conc <= 0.0001, "<","")) %>%
    filter(conc >= 0.0001) %>%
    select(date,remark,conc) %>%
    na.omit(conc)

  write.csv(fwmc, file = paste0("data/wrtds/input/",poi,"/",gen_dat$station[i],".csv", sep = ""),row.names = FALSE)
  }
}
