library(rio)
library(tidyverse)
library(ggplot2)
library(dplyr)
library("RColorBrewer")

raw_data <- import("data/wrtds_output/eList_k/daily/srp/Maumee.csv") %>%
  mutate(logConc = log(GenConc),
         logQ = log(Q),
         waterYear = as.integer(waterYear))

q90 <- quantile(raw_data$Q, 0.85)
q60 <- quantile(raw_data$Q, 0.60)

raw_data %>%
  mutate(flow_state = ifelse(Q >= q90, "High",ifelse(Q <= q60,"Low","Mod")),
         period = ifelse(waterYear >= 2015, "post", "pre")) %>%
  filter(Month == 12 | Month == 1 | Month == 2) %>%
  #filter(Q>= q40) %>%
  #filter(waterYear <= 2015) %>%
  ggplot(aes(x = logQ, y = logConc,color = flow_state))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~waterYear)

# Run ANCOVA
#code to run the 2-way anova for the epiphyte data to test model assumptions
ancova_data <- raw_data %>%
  mutate(flow_state = ifelse(Q >= q90, "High",ifelse(Q <= q60,"Low","Mod"))) %>%
  filter(waterYear >= 2009 & waterYear <= 2021) %>%
  filter(flow_state == "Mod") %>%
  mutate(YEAR = as.factor(waterYear))

ancova<-lm(logConc~ logQ + YEAR, data=ancova_data)
anova(ancova)
summary(ancova)

library(HH)
ancova(logConc ~ logQ + YEAR, data=ancova_data)


mod <- ancova(logConc ~ logQ + YEAR, data=ancova_data)
pred <- predict(mod)

ggplot(data = cbind(ancova_data, pred),
         aes(logQ, logConc, color=YEAR)) + geom_line(aes(y=pred))


emmeans::emmeans(ancova, "YEAR")

library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)

res.aov <- ancova_data %>%
  anova_test(logConc ~ logQ + YEAR)

get_anova_table(res.aov)

pwc <-  ancova_data%>%
  emmeans_test(
    logConc ~ logQ, covariate = YEAR,
    p.adjust.method = "bonferroni"
  )

pwc

###############################################################################
output <-  NULL

yrs <- as.integer(seq(2008,2022,by = 1))
length(yrs)

for(i in 1:length(yrs)){
  yr <- as.integer(yrs[i])

  data <- filter(raw_data, waterYear %in% yr)

  slr <- lm(logConc ~ logQ, data = data)
  m <- summary(slr)

  output[[i]] <- c(m$coefficients[1,1], m$coefficients[2,1])

}

lm_res <- as.data.frame(do.call(rbind,output))

colnames(lm_res) <- c("intercept","slope")
lm_res$year <- seq(2008,2022,1)

ggplot(lm_res,aes(x= year, y = slope))+
  geom_point()+
  geom_smooth(method= "lm", se = FALSE)
