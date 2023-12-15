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
# create point plot using ggplot and SRP values
################################################################################
data_annual <- data %>%
  pivot_longer(cols = c(5:10),names_to = "category",values_to = "value") %>%
  separate(category, c('period', 'type')) %>%
  filter(period != "diff" & season == "annual" & poi == "srp")


data_seasonal <- data %>%
  pivot_longer(cols = c(5:10),names_to = "category",values_to = "value") %>%
  separate(category, c('period', 'type')) %>%
  filter(period != "diff" & season != "annual" & poi == "srp")

########################################################################
# Create better labels for Facets
# New facet label names for dose variable
type.labs <- c("FN Concentration", "FN Flux")
names(type.labs) <- c("conc", "flux")

# New facet label names for supp variable
period.labs <- c("Full Period Trends (08-22)", "Recent Trends (15-22)")
names(period.labs) <- c("full", "recent")
#########################################################################
# SRP plot
srp_plot <- ggplot(data_annual)+
  geom_hline(yintercept = 0, color = "black")+
  geom_point(aes(x = reorder(abbv,value), y = value),color = "red",size = 1.25)+
  facet_grid(type~period, labeller = labeller(type = type.labs, period = period.labs))+
  geom_point(data = data_seasonal, aes(x = abbv, y = value, color = season,shape = season),alpha = 0.6,size =1.25)+
  labs(x = "Site", y = "Rate of Change in SRP (%/yr)", color = "Season", shape = "Season")+
  theme_bw()+
  scale_colour_brewer(palette = "Dark2")+
  scale_shape_manual(values = c(22,23,24,25))+
  theme(text = element_text(size = 10))

srp_plot
ggsave("figs/slopes/srp_seasonal.tiff",width = 6, height = 4)

###############################################################################

###############################################################################
# Start TP plot
#########################################################################
data_annual <- data %>%
  pivot_longer(cols = c(5:10),names_to = "category",values_to = "value") %>%
  separate(category, c('period', 'type')) %>%
  filter(period != "diff" & season == "annual" & poi == "tp")


data_seasonal <- data %>%
  pivot_longer(cols = c(5:10),names_to = "category",values_to = "value") %>%
  separate(category, c('period', 'type')) %>%
  filter(period != "diff" & season != "annual" & poi == "tp")


# TP plot
tp_plot <- ggplot(data_annual)+
  geom_hline(yintercept = 0, color = "black")+
  geom_point(aes(x = reorder(abbv,value), y = value),color = "red",size = 1.25)+
  facet_grid(type~period, labeller = labeller(type = type.labs, period = period.labs))+
  geom_point(data = data_seasonal, aes(x = abbv, y = value, color = season,shape = season),alpha = 0.6,size =1.25)+
  labs(x = "Site", y = "Rate of Change in TP (%/yr)", color = "Season", shape = "Season")+
  theme_bw()+
  scale_colour_brewer(palette = "Dark2")+
  scale_shape_manual(values = c(22,23,24,25))+
  theme(text = element_text(size = 10))

tp_plot
ggsave("figs/slopes/p_seasonal.tiff",width = 6, height = 4)

