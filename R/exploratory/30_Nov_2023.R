# Objective: Create plot for changing loads as %/yr and mt/yr for SRP, TP, and TSS
#            at the four sites of interest

# load in libraries
library(tidyverse)
library(rio)
library(ggplot2)

# load in general data
gen.dat <- import("data/wrtds/input/gen_dat.xlsx")

# for loop parameters
poi <- c("srp","tp","tss")

in.dat <- NULL

# write a for loop to pull data from each pollutant into single dataframe
for(i in 1:length(poi)){
  in.dat[[i]] <- import(paste("data/process/annual/",poi[i],"_flux_results.csv",sep = "")) %>%
    filter(scenario != "all") %>%
    mutate(pollutant = poi[i])

}

#combine in.dat from each site into a useable dataframe
all.dat <- as.data.frame(do.call(rbind,in.dat))

# join gen and all data then calculate yield/yr
all.dat <- left_join(all.dat, gen.dat, by = "station") %>%
  mutate(yield_slope = (flux_slope*1000)/area_km2)

# create plots
## flux_slope

#change pre/post labels to show difference
all.dat$scenario <- factor(all.dat$scenario, levels=c('pre', 'post'))
all.dat$station <- factor(all.dat$station, levels=c('Tiffin','Blanchard','Maumee', 'Raisin'))

# New facet label names for supp variable
pol.labs <- c("SRP", "TP","TSS")
names(pol.labs) <- c("srp", "tp","tss")

p1 <- all.dat %>%
  ggplot(aes(x = scenario, y = flux_per_slope, color = station, group = station,shape = station)) +
  geom_point() +
  geom_line(show.legend = F) +
  ylab("Slope of annual FN load trends for over each period, %/yr") +
  xlab("")+
  facet_wrap(~pollutant,ncol = 1, labeller = labeller(pollutant = pol.labs))+
  labs(color = "Station",shape = "Station")+
  theme_bw()+
  theme(legend.position="bottom")+
  theme(text = element_text(size = 9)) +
  scale_y_continuous(breaks = seq(-3, 6, by = 1))


ggsave(p1, filename = "results/figs/flux_slopes.tiff", dpi = 350, height = 7, width = 4.25)



Pre <- all.dat %>%
  filter(scenario == "pre")

Post <- all.dat %>%
  filter(scenario == "post")

head(Post)

#change pre/post labels to show difference
all.dat$station <- factor(all.dat$station, levels=c('Tiffin','Blanchard','Maumee', 'Raisin'))

p <- ggplot(all.dat) +
  geom_segment(data = Pre,
               aes(x = yield_slope, y = station,
                   yend = Post$station, xend = Post$yield_slope),
               arrow = arrow(angle = 15, length = unit(.15, "inches"), type = "closed"),
               color = "#aeb6bf",
               size = 1, #Note that I sized the segment to fit the points
               alpha = .5) +
  geom_point(aes(x = yield_slope, y = station, color = scenario), size = 4, show.legend = TRUE)+
  ggtitle("Enrollment Trends at Historically Black Colleges and Universities")+
  facet_wrap(~pollutant,scales= "free")

p
