################################################################################
# title: Plotting FN
# Objective: Plot comparisons of recent and full period trends
################################################################################

# load libraries

library(tidyverse)
library(ggplot2)
library(readxl)
library(gridExtra)
library(ggrepel)

# load in gen_funcs
data <- read_excel("data/wrtds_output/trends/comps/slopes.xlsx") %>%
  filter(station != "Chickasaw")

head(data)


################################################################################
# create dataset of rate changes that can be used as a table in the word document
################################################################################
df <- data %>%
  filter(season == "annual") %>%
  pivot_wider(names_from = poi, values_from = 5:10)


df <- df[,-c(12:15)]

df2 <- rapply(object = df, f = round, classes = "numeric", how = "replace", digits = 1)

df2 <- df2[,c(1:3,8,4,10,6,9,5,11,7)]

write.csv(df2, file = "data/wrtds_output/trends/comps/rate_changes.csv",row.names = FALSE)


################################################################################
annotations <- data.frame(
  xpos = c(-Inf,-Inf,Inf,Inf),
  ypos =  c(-Inf, Inf,-Inf,Inf),
  annotateText = c("Strengthening downward","Weakening downward"
                   ,"Weakening postive","Strengthening positive"),
  hjustvar = c(0,0,1,1) ,
  vjustvar = c(0,1,0,1)) #<- adjust

annotations$hjustvar<-c(-0.2, -0.2,  1.25,  1.25)  # higher values = right, lower values = left
annotations$vjustvar<-c(-2,2,-2, 2)
################################################################################

srp_conc <- data %>%
  filter(poi == "srp" & season == "annual") %>%
  ggplot(aes(x = full_conc,y = diff_conc))+
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))+
  geom_point()+
  scale_x_continuous(breaks=seq(-20,20,4), limits = c(-20,20))+
  scale_y_continuous(breaks=seq(-20,20,4), limits = c(-20,20))+
  labs(x = "Rate of Change in SRP concentration (08/22)", y = "Difference in SRP trend (15/22 - 08/22)")+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_label_repel(aes(label = station),
                   box.padding   = 1.25,
                   label.padding = 0.3,
                   point.padding = 1.5,
                   segment.color = 'grey50',
                   max.overlaps = getOption("ggrepel.max.overlaps", default = 50))+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

srp_conc


srp_flux <- data %>%
  filter(poi == "srp" & season == "annual") %>%
  ggplot(aes(x = full_flux,y = diff_flux))+
  geom_point()+
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))+
  scale_x_continuous(breaks=seq(-8,8,2), limits = c(-8,8))+
  scale_y_continuous(breaks=seq(-8,8,2), limits = c(-8,8))+
  labs(x = "Rate of Change in SRP flux (08/22)", y = "Difference in SRP flux trend (15/22 - 08/22)")+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_label_repel(aes(label = station),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

tp_conc <- data %>%
  filter(poi == "tp" & season == "annual") %>%
  ggplot(aes(x = full_conc,y = diff_conc))+
  geom_point()+
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))+
  scale_x_continuous(breaks=seq(-8,8,2), limits = c(-8,8))+
  scale_y_continuous(breaks=seq(-8,8,2), limits = c(-8,8))+
  labs(x = "Rate of Change in TP concentration (08/22)", y = "Difference in TP trend (15/22 - 08/22)")+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_label_repel(aes(label = station),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

tp_flux <- data %>%
  filter(poi == "tp" & season == "annual") %>%
  ggplot(aes(x = full_flux,y = diff_flux))+
  geom_point()+
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))+
  scale_x_continuous(breaks=seq(-8,8,2), limits = c(-8,8))+
  scale_y_continuous(breaks=seq(-8,8,2), limits = c(-8,8))+
  labs(x = "Rate of Change in TP flux (08/22)", y = "Difference in TP flux trend (15/22 - 08/22)")+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_label_repel(aes(label = station),
                   box.padding   = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50')+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))


grid.arrange(srp_conc, srp_flux, tp_conc, tp_flux)


################################################################################
# create a plot for each season
################################################################################
srp_conc <- data %>%
  filter(poi == "srp" & season != "annual") %>%
  ggplot(aes(x = full_conc,y = diff_conc))+
  geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText))+
  geom_point()+
  scale_x_continuous(breaks=seq(-20,20,4), limits = c(-20,20))+
  scale_y_continuous(breaks=seq(-20,20,4), limits = c(-20,20))+
  labs(x = "Rate of Change in SRP concentration (08/22)", y = "Difference in SRP trend (15/22 - 08/22)")+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  geom_label_repel(aes(label = station),
                   box.padding   = 1.25,
                   label.padding = 0.3,
                   point.padding = 1.5,
                   segment.color = 'grey50',
                   max.overlaps = getOption("ggrepel.max.overlaps", default = 50))+
  facet_wrap(~season)+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))

srp_conc

