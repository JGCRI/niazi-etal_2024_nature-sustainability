## Global peak and decline plot
# This plot has a global trend, marginal distributions, and validation data from other studies
# Hassan Niazi, JGCRI, April 2022

# rm(list = ls(all=TRUE))

library(tidyverse)
library(ggplot2)
library(ggExtra)
library(RColorBrewer)
library(egg)

## Data prep ---------------
read_csv("groundwater_production_FINAL.csv") -> all_data
# validation data sets
read.csv("historyv1.csv", sep = ",") -> history
read.csv("konikowv1.csv", sep = ",") -> konikow

# data for peak water and peak year 900x8
all_data %>%   
  select(groundwater, scenario, year, value) %>%
  filter(year != 1975) %>%
  group_by(scenario, year) %>% 
  summarise(value = sum(value)) %>%
  summarise(
    max = max(value),
    max_yr = .$year[which.max(value)],
    taken = sum(value)
  ) %>%
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>%
  mutate(ssp = gsub("ssp", "SSP", ssp), rcp = paste0("RCP", rcp), rcp = gsub("p", ".", rcp), ssp_rcp = paste0(ssp, "_", rcp)) %>%
  select(ssp_rcp, ssp, rcp, max, max_yr, calib, surf, gw) ->
  gw_peaks_detailed

# get years and quantiles 21x6
all_data %>%   
  dplyr::filter(value > 0) %>% 
  #dplyr::filter(year >= 2010) %>%
  dplyr::filter(year != 1975) %>%
  group_by(scenario, year) %>% 
  summarise(value = sum(value)) %>%
  ungroup() %>% group_by(year) %>%
  summarise(
    median = median(value),
    iqr.25 = quantile(value, probs = 0.25),
    iqr.5 = quantile(value, probs = 0.05),
    iqr.75 = quantile(value, probs = 0.75),
    iqr.95 = quantile(value, probs = 0.95),
    min = min(value),
    max = max(value)
  ) ->
  year_total


## New global PnD plot ----

colourCount = length(unique(history$study))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

## Same figure as below after archived with different arrangement (marginals at bottom left instead of top right)

## Global Peak and decline with marginals 
## Main middle plot ----
year_total %>%
  ggplot() +
  #geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = "red", alpha = 0.35) +
  #geom_ribbon(aes(x = year, ymin = iqr.5, ymax = iqr.95), fill = "grey95") +
  
  #geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = "red", alpha = 0.35) +
  #geom_ribbon(aes(x = year, ymin = iqr.5, ymax = iqr.95), fill = "grey90") +

  geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = "grey92") +
  geom_ribbon(aes(x = year, ymin = iqr.5, ymax = iqr.95), fill = "grey80") +
  
  #geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = "dodgerblue", alpha = 0.2) +
  #geom_ribbon(aes(x = year, ymin = iqr.5, ymax = iqr.95), fill = "dodgerblue", alpha = 0.5) +
  
  geom_ribbon(aes(x = year, ymin = iqr.25, ymax = iqr.75), fill = "dodgerblue", alpha = 0.95) +
  geom_line(aes(x = year, y = median), color = "black", size = 1.5) +
  #geom_line(aes(x = year, y = median), color = "black", size = 1.5, alpha=0.25) +
  #stat_smooth(aes(x = year, y = median),se=FALSE, geom="line", method = 'loess', alpha=1, size=2)+
  geom_ribbon(data = konikow, aes(x = year, ymin = ymin, ymax = ymax), fill = "forestgreen", alpha = 0.5) +
  geom_line(data = konikow, aes(x = year, y = value, linetype = study), color = "black", size = 1) +
  geom_point(data = history, aes(x = year, y = depletion, color = study, group = study, shape=study), size = 2, stroke=1) +
  #geom_point(data=gw_peaks_detailed, aes(x= mean(max_yr), y=mean(max)), color="black", size=4) + #global mean
  #geom_vline(data=gw_peaks_detailed, aes(xintercept = mean(max_yr)), size = 0.5, color = "grey80", linetype = "dashed")+
  #geom_hline(data=gw_peaks_detailed, aes(yintercept = mean(max)), size = 0.5, color = "grey80", linetype = "dashed")+
  scale_color_manual(values = getPalette(colourCount), name = "") +
  scale_shape_manual(values = c(0,1,2, 5,6,7,8,9, 14,11,12), name = "") +
  #scale_shape_manual(values = c(1:colourCount), name = "") +
  scale_linetype_manual(values = c("dashed"), name = "") +
  #ylim(0, 1200) +
  scale_x_continuous(expand = c(0.0, 0),
                     breaks = c(1960, 1980, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2075, 2080, 2090, 2100),
                     labels = c("1960", "1980", "2000", "2010", "", "2030", "", "2050", "", "", "2075", "", "", "2100")  
                     #breaks = c(1960, 1980, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2075, 2090, 2100),
                     #labels = c("1960", "1980", "2000", "2010", "", "2030", "", "2050", "", "2075", "", "2100")  
  ) + 
  scale_y_continuous(limits = c(0,1700), expand = c(0, 0)) +
  #scale_x_continuous(expand = c(0,0),breaks=c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), labels=c("2010","","","","2050","","","","","2100")) +
  #labs(y = expression(paste("Global Groundwater Depletion (", km ^ 3, ")")), x =  "") +
  #labs(y = expression(paste("Global Peak Water Withdrawal (", km ^ 3, ")")), x =  "Year") +
  labs(y = expression(paste("Groundwater Withdrawal (", km ^ 3, yr ^  -1 , ")")), x =  "Year") +
  #guides(linetype="legend") +
  theme_bw() + theme(
    legend.text = element_text(size = 8), legend.position = "right", #legend.justification='left',
    panel.grid.major=element_line(colour="white"), panel.grid.minor=element_line(colour="white"),
    axis.text = element_text(size = 15), axis.title = element_text(size = 16),
    plot.margin = margin(10, 20, 10, 10)
  ) -> mainplot

mainplot
#ggsave("globalpeakndecline_box.pdf", plot= line, width = 10, height = 8)
#ggsave("globalpeakndecline_landscape.pdf", plot= line, width = 11, height = 6) # change legend.position="right"


empty <- ggplot() + geom_point(aes(1, 1), color = "white") +
  theme_void()

## Marginal peak water ----

#prepare quantile for marginals of peak water
dt <- data.frame(x = gw_peaks_detailed$max_yr, y = gw_peaks_detailed$max)
dens <- density(dt$y)
df <- data.frame(x = dens$x, y = dens$y)
probs <- c(0.25, 0.75)
quantiles <- quantile(dt$y, prob = probs)
df$quant <- factor(findInterval(df$x,quantiles))

ls <- ggplot(df, aes(x,y)) + 
  geom_line(size=0.75) + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant), alpha = 0.35) + 
  geom_segment(data=gw_peaks_detailed, aes( x=median(max), y=0, xend=median(max), yend=0.00142), size=0.75, color="black", linetype="dashed") +
  #scale_x_continuous(breaks=quantiles) + 
  scale_fill_manual(values=c("gray","dodgerblue","gray")) +
  #scale_fill_brewer(guide="none")
  scale_x_continuous(limits = c(0, 1700), expand = c(0.01, 0)) +
  coord_flip() + 
  scale_y_reverse() +
  labs(y = expression(paste("Probability Density")), x =  "") +
  theme_classic() +
  theme(axis.line.y=element_blank(),
        legend.position="none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        #axis.line.y.bottom = element_blank(),
        #                      panel.border = element_blank(),
        panel.grid=element_blank())

ls 

#densyr <- density(gw_peaks_detailed$max_yr)
#densyr <- with(density(gw_peaks_detailed$max_yr), data.frame(x, y))

## Marginals peak year ----

# prepare quantile for marginals of peak year 
dty <- data.frame(x=gw_peaks_detailed$max,y=gw_peaks_detailed$max_yr)
densy <- density(dty$y)
dfy <- data.frame(x=densy$x, y=densy$y)
dfyy <- data.frame(x=c(1955,dfy$x), y=c(0,dfy$y))
probsy <- c(0.25, 0.75)                                                         
quantilesy <- quantile(dty$y, prob=probsy)
dfy$quant <- factor(findInterval(dfy$x,quantilesy))

bs <- ggplot(dfyy, aes(x,y)) + 
  geom_line(size=0.75) + 
  geom_ribbon(data=dfy, aes(ymin=0, ymax=y, fill=quant), alpha = 0.35) + 
  geom_segment(data=gw_peaks_detailed, aes( x=median(max_yr), y=0, xend=median(max_yr), yend=0.027), size=0.75, color="black", linetype="dashed") +
  #scale_fill_brewer(guide="none") +
  scale_fill_manual(values=c("gray","dodgerblue","gray")) +
  scale_x_continuous(#breaks=quantilesy,
    limits = c(1955, 2100),
    #breaks = c(2045, 2050, 2100),
    #labels = c("2045", "2050", "2100"),
    expand = c(0.0, 0)) +
  scale_y_reverse() +
  labs(y = expression(paste("Probability Density")), x =  "") +
  theme_classic() +
  theme(legend.position="none",
        axis.line.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        #axis.title.y=element_line()
        #axis.line.x.bottom = element_blank()
        #panel.border = element_blank(),
        #panel.grid=element_blank()
  )
bs


# with shaded region ----
globalpeakndecline <- ggarrange(ls, mainplot, empty,  bs, ncol=2, widths = c(0.2,1), heights = c(2, 0.7)) 
#ggsave("globalpnd_mainv1.png", plot=globalpeakndecline, width = 12, height = 7)
#ggsave("globalpeakndecline_marginals_bl_shaded.pdf", plot=globalpeakndecline, width = 12, height = 7)
#ggsave("globalpeakndecline_marginals_bl_shaded.svg", plot=globalpeakndecline, width = 12, height = 7)









###### ARCHIEVED ########################## 

## Old: Global Peak and decline with marginal: from Neal ----

# main middle plot ----
year_total %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = iqr.5, ymax = iqr.95), fill = "grey", alpha = 0.25) +
  geom_ribbon(aes(x = year, ymin = iqr.25, ymax = iqr.75), fill = "dodgerblue", alpha = 0.75) +
  geom_line(aes(x = year, y = median), color = "black", size = 1.5) +
  #geom_line(aes(x = year, y = median), color = "black", size = 1.5, alpha=0.25) +
  #stat_smooth(aes(x = year, y = median),se=FALSE, geom="line", method = 'loess', alpha=1, size=2)+
  geom_ribbon(data = konikow, aes(x = year, ymin = ymin, ymax = ymax), fill = "forestgreen", alpha = 0.5) +
  geom_line(data = konikow, aes(x = year, y = value, linetype = study), color = "black", size = 1.5) +
  geom_point(data = history, aes(x = year, y = depletion, color = study, group = study, shape=study), size = 2, stroke=1.25) +
  scale_color_manual(values = getPalette(colourCount), name = "") +
  scale_shape_manual(values = c(0,1,2, 5,6,7,8,9, 14,11,12), name = "") +
  #scale_shape_manual(values = c(1:colourCount), name = "") +
  scale_linetype_manual(values = c("dashed"), name = "") +
  #ylim(0, 1200) +
  scale_x_continuous(expand = c(0.0, 0),
    breaks = c(1960, 1980, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2075, 2080, 2090, 2100),
    labels = c("1960", "1980", "2000", "2010", "", "2030", "", "2050", "", "", "2075", "", "", "2100")  
    #breaks = c(1960, 1980, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2075, 2090, 2100),
    #labels = c("1960", "1980", "2000", "2010", "", "2030", "", "2050", "", "2075", "", "2100")  
    ) +
  scale_y_continuous(limits = c(0,1200), expand = c(0, 0)) +
  #scale_x_continuous(expand = c(0,0),breaks=c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), labels=c("2010","","","","2050","","","","","2100")) +
  #labs(y = expression(paste("Global Groundwater Depletion (", km ^ 3, ")")), x =  "") +
  labs(y = expression(paste("Global Peak Water Withdrawal (", km ^ 3, ")")), x =  "Year") +
  #guides(linetype="legend") +
  theme_bw() + theme(
    legend.text = element_text(size = 10), legend.position = "bottom", #legend.justification='left',
    panel.grid.major=element_line(colour="white"), panel.grid.minor=element_line(colour="white"),
    axis.text = element_text(size = 15), axis.title = element_text(size = 16),
    plot.margin = margin(10, 20, 10, 10)
  ) -> line

line
#ggsave("globalpeakndecline_box.pdf", plot= line, width = 10, height = 8)
#ggsave("globalpeakndecline_landscape.pdf", plot= line, width = 11, height = 6) # change legend.position="right"


emptyy <- ggplot() + geom_point(aes(1, 1), color = "white") +
  theme_void()

t <- ggplot()+
  geom_density(data = gw_peaks_detailed, aes(x=max_yr), size=0.75, fill = "dodgerblue", alpha = 0.35) +
  scale_x_continuous(
    limits = c(1960, 2100),
    breaks = c(2045, 2050, 2100),
    labels = c("2045", "2050", "2100"),
    expand = c(0.0, 0)) +
  theme_void()

r <- ggplot()+
  geom_density(data = gw_peaks_detailed, aes(x=max), size=0.75, fill = "dodgerblue", alpha = 0.35) +
  labs(title = "", x = "", y = "") + 
  #xlim(0, 1700)  + 
  scale_x_continuous(limits = c(0, 1700), expand = c(0.01, 0)) +
  coord_flip() + 
  theme_void()

globalpeakndecline <- ggarrange(t, emptyy, line, r, ncol=2, widths = c(1,0.15), heights = c(0.7,2))

#ggsave("globalpeakndecline_marginals.svg", plot=globalpeakndecline, width = 11, height = 9)
#ggsave("globalpeakndecline_marginals_ls.pdf", plot=globalpeakndecline, width = 11, height = 7)



#################################################################-
# bottom marginal without shaded region between 25%-75% ----
b <- ggplot()+
  #geom_histogram(data = gw_peaks_detailed, aes(x=max_yr, y=..density..), fill = "yellow", alpha = 0.35, bins = 15) +
  geom_density(data=gw_peaks_detailed, aes(x=max_yr), size=0.75, fill = "dodgerblue", alpha = 0.35) +
  #geom_area(data = densyr, aes(x=x, y=y)) +
  #geom_vline(data=gw_peaks_detailed, aes(xintercept=1960, color="black")) +
  geom_vline(data=gw_peaks_detailed, aes(xintercept=mean(max_yr), color="red"), linetype="dashed") +
  geom_vline(data=gw_peaks_detailed, aes(xintercept=mean(max_yr)-sd(max_yr), color="green"), linetype="dashed") +
  geom_vline(data=gw_peaks_detailed, aes(xintercept=mean(max_yr)+sd(max_yr), color="green"), linetype="dashed") +
  #geom_area(data=gw_peaks_detailed, aes(x= ifelse(max_yr>(mean(max_yr)-sd(max_yr)) & max_yr<(mean(max_yr)+sd(max_yr)), max_yr, 0), y=y$y)) +
  scale_x_continuous(
    limits = c(1960, 2100),
    breaks = c(2045, 2050, 2100),
    labels = c("2045", "2050", "2100"),
    expand = c(0.0, 0)) +
  scale_y_reverse() +
  labs(y = expression(paste("Probability Density")), x =  "") +
  theme_classic() +
  theme(legend.position="none",
    axis.line.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.x=element_blank(),
    #axis.title.y=element_line()
    #axis.line.x.bottom = element_blank()
    #panel.border = element_blank(),
    #panel.grid=element_blank()
    )
b



l <- ggplot()+
  geom_density(data = gw_peaks_detailed, aes(x=max), size=0.75, fill = "dodgerblue", alpha = 0.35) +
  labs(title = "", x = "", y = "") + 
  geom_vline(data=gw_peaks_detailed, aes(xintercept=mean(max), color="red"), linetype="dashed") +
  geom_vline(data=gw_peaks_detailed, aes(xintercept=mean(max)-sd(max), color="green"), linetype="dashed") +
  geom_vline(data=gw_peaks_detailed, aes(xintercept=mean(max)+sd(max), color="green"), linetype="dashed") +
  #xlim(0, 1700)  + 
  scale_x_continuous(limits = c(0, 1700), expand = c(0.01, 0)) +
  coord_flip() + 
  scale_y_reverse() +
  labs(y = expression(paste("Probability Density")), x =  "") +
  theme_classic() +
  theme(axis.line.y=element_blank(),
    legend.position="none",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.title.y=element_blank(),
    #axis.line.y.bottom = element_blank(),
    #                      panel.border = element_blank(),
    panel.grid=element_blank())



# without shaded region ----
globalpeakndecline <- ggarrange(l, mainplot, emptyy,  b, ncol=2, widths = c(0.2,1), heights = c(2, 0.7)) 
#ggsave("globalpeakndecline_marginals_bl.png", plot=globalpeakndecline, width = 12, height = 7)
#ggsave("globalpeakndecline_marginals_bl.pdf", plot=globalpeakndecline, width = 12, height = 7)
#ggsave("globalpeakndecline_marginals_bl.svg", plot=globalpeakndecline, width = 12, height = 7)







## global peak and decline by 
ggplot(gw_peaks_detailed, aes(max_yr, max))+
  geom_jitter()+
  geom_smooth()+
  theme_bw()+
  xlim(c(2010,2100)) +
  facet_wrap(~rcp) +  # uncomment to view wrt to RCPs 
  labs(y = "Peak Groundwater Withdrawals (km3/yr)",
       x = "Year")


## Just reference studies 

year_total %>%
  ggplot() +
  geom_ribbon(data = konikow, aes(x = year, ymin = ymin, ymax = ymax), fill = "forestgreen", alpha = 0.5) +
  geom_line(data = konikow, aes(x = year, y = value, linetype = study), color = "black", size = 1) +
  geom_point(data = history, aes(x = year, y = depletion, color = study, group = study, shape=study), size = 2, stroke=1) +
  #geom_point(data=gw_peaks_detailed, aes(x= mean(max_yr), y=mean(max)), color="black", size=4) + #global mean
  #geom_vline(data=gw_peaks_detailed, aes(xintercept = mean(max_yr)), size = 0.5, color = "grey80", linetype = "dashed")+
  #geom_hline(data=gw_peaks_detailed, aes(yintercept = mean(max)), size = 0.5, color = "grey80", linetype = "dashed")+
  scale_color_manual(values = getPalette(colourCount), name = "") +
  scale_shape_manual(values = c(0,1,2, 5,6,7,8,9, 14,11,12), name = "") +
  #scale_shape_manual(values = c(1:colourCount), name = "") +
  scale_linetype_manual(values = c("dashed"), name = "") +
  #ylim(0, 1200) +
  scale_x_continuous(expand = c(0.0, 0.01),
                     breaks = c(1960, 1980, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2075, 2080, 2090, 2100),
                     labels = c("1960", "1980", "2000", "2010", "", "2030", "", "2050", "", "", "2075", "", "", "2100")  
                     #breaks = c(1960, 1980, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2075, 2090, 2100),
                     #labels = c("1960", "1980", "2000", "2010", "", "2030", "", "2050", "", "2075", "", "2100")  
  ) + 
  scale_y_continuous(limits = c(0,1700), expand = c(0, 0)) +
  #scale_x_continuous(expand = c(0,0),breaks=c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), labels=c("2010","","","","2050","","","","","2100")) +
  #labs(y = expression(paste("Global Groundwater Depletion (", km ^ 3, ")")), x =  "") +
  #labs(y = expression(paste("Global Peak Water Withdrawal (", km ^ 3, ")")), x =  "Year") +
  labs(y = expression(paste("Groundwater Withdrawal (", km ^ 3, yr ^  -1 , ")")), x =  "Year") +
  #guides(linetype="legend") +
  theme_bw() + theme(
    legend.text = element_text(size = 8), legend.position = "right", #legend.justification='left',
    panel.grid.major=element_line(colour="white"), panel.grid.minor=element_line(colour="white"),
    axis.text = element_text(size = 15), axis.title = element_text(size = 16),
    plot.margin = margin(10, 20, 10, 10)
  ) -> mainplot_justrefs

globalpeakndecline_justrefs <- ggarrange(ls, mainplot_justrefs, empty,  bs, ncol=2, widths = c(0.2,1), heights = c(2, 0.7)) 
globalpeakndecline_justrefs

#ggsave("globalpnd_main_justrefs.png", plot=globalpeakndecline_justrefs, width = 12, height = 7)
