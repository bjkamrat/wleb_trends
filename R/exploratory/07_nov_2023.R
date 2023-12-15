# Objective: monthly c-q between periods for single site

library(rio)
library(tidyverse)
library(ggplot2)
library(dplyr)
library("RColorBrewer")

raw_data <- import("data/wrtds/output/eList_k/daily/srp/Maumee.csv") %>%
  mutate(logConc = log(GenConc),
         logQ = log(Q),
         waterYear = as.integer(waterYear))

library(readxl)
raw_data <- read_excel("data/raw/srp/2023_10_26_data_download.xlsx",
                                        sheet = "Maumee_samples")
colnames(raw_data) <- c("Date","Q_qual","Q","Conc_qual","Conc")
View(raw_data)

raw_data <- raw_data %>%
  select(Date,Q,Conc) %>%
  mutate(period = ifelse(Date >= "2015-07-04", "post", "pre")) %>%
  drop_na(Q)

q90 <- quantile(raw_data$Q, 0.90)
q60 <- quantile(raw_data$Q, 0.60)


raw_data %>%
  mutate(flow_state = ifelse(Q >= q90, "High",ifelse(Q <= q60,"Low","Mod")),
         Month = month(Date)) %>%
  filter(Conc >= 0.003) %>%
  #filter(waterYear <= 2015) %>%
  ggplot(aes(x = log(Q), y = Conc,color = period))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~Month)

raw_data %>%
  ggplot()+
  geom_point(aes(x = Year,y = Conc))

# calculate summary stats using raw_data
sum.stat <- raw_data %>%
  mutate(flow_state = ifelse(Q >= q90, "High",ifelse(Q <= q60,"Low","Mod")),
         Month = month(Date)) %>%
  filter(Conc >= 0.003) %>%
  group_by(period,Month,flow_state) %>%
  summarise(mean = mean(Conc,na.rm = TRUE),
            se = sd(Conc, na.rm = TRUE)/sqrt(length((Conc))),
            count = n()) # %>%
  # pivot_wider(names_from= period, values_from = c(count,mean,se)) %>%
  # mutate(per_change = ((mean_post - mean_pre)/mean_pre)*100)



sum.stat %>%
  ggplot(aes(x = Month, y = mean,color = period))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  facet_wrap(~flow_state)














# Run ANCOVA
#code to run the 2-way anova for the epiphyte data to test model assumptions
ancova_data <- raw_data %>%
  mutate(flow_state = ifelse(Q >= q90, "High",ifelse(Q <= q60,"Low","Mod"))) %>%
  filter(waterYear >= 2009 & waterYear <= 2021,
         period = ifelse(waterYear >= 2016, "post", "pre")) %>%
  filter(flow_state == "Mod") %>%
  mutate(YEAR = as.factor(waterYear),
         MONTH = as.factor(Month))

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
