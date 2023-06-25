# This figure synthesizes results across all SSP_RCP combinations (60 simulations each)

# subplots: violins, box and whiskers, data summary per panel, and a horizontal scale

# rm(list = ls(all=TRUE))

library(tidyverse)
library(ggtext)
library(egg)
library(ggpubr)

# source("GeomSplitViolin.R")

# data prep ----
expand.grid(paste0("ssp", 1:5),
            c("2p6", "4p5", "6p0", "8p5")) %>%
  mutate(x = paste0(Var1, "_", Var2)) %>%
  .$x -> combos

read_csv("groundwater_production_FINAL.csv") %>%
  select(groundwater, scenario, year, value) %>%
  filter(year != 1975) %>%
  group_by(scenario, year) %>% summarise(value = sum(value)) %>%
  summarise(max = max(value),
            max_yr = .$year[which.max(value)],
            taken = sum(value)) %>%
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>%
  mutate(ssp = gsub("ssp","SSP",ssp),
         rcp=paste0("RCP",rcp),
         rcp=gsub("p",".",rcp),
         gcm = gsub('gfdl', 'GFDL', gcm),
         gcm = gsub('hadg', 'HadGEM', gcm),
         gcm = gsub('ipsl', 'IPSL', gcm),
         gcm = gsub('miro', 'MIROC', gcm),
         gcm = gsub('nrsm', 'NorESM', gcm),
         calib = gsub('gl', 'PCR-GLOBWB', calib),
         calib = gsub('wg', 'WaterGapHM', calib),
         surf = gsub('rs', 'Restricted', surf),
         surf = gsub('ex', 'Expansion', surf),
         gw = gsub('lo', 'Low', gw),
         gw = gsub('md', 'Medium', gw),
         gw = gsub('hi', 'High', gw),
         ssp_rcp = paste0(ssp, "_", rcp)) %>%
  select(ssp_rcp, ssp, rcp, gcm, max, max_yr, taken, calib, surf, gw) ->
  gw_peaks_detailed

# write.csv(gw_peaks_detailed,file="peakyearvspeakwatersummary_v1.csv")



# Main violins ----
## Find peak groundwater extraction time for each scenario

gw_peaks_detailed %>%
  #group_by(calib, gw, surf, ssp, gcm, rcp) %>%
  group_by(ssp,rcp) %>%
  summarise(median.v = median(max), ten.v = quantile(max,probs=0.1), ninety.v=quantile(max,probs=0.9),
            median.y = median(max_yr), ten.y = quantile(max_yr,probs=0.1), ninety.y=quantile(max_yr,probs=0.9),
            mean.v=mean(max), mean.y=mean(max_yr),
            max.v=max(max), max.y=max(max_yr)) -> quantiles

# write.csv(quantiles,file="keystatistics_quantiles.csv")

gw_peaks_detailed %>% group_by(ssp) %>% summarise(median.y=round(median(max_yr),-1), median.v=round(median(max),-1), max.v=round(max(max),-1), max.y=max(max_yr)) -> maxssps
gw_peaks_detailed %>% group_by(rcp) %>% summarise(max.v=round(max(max),-1), max.y=max(max_yr)) -> maxrcps


# master synthesis
ggplot() +
  geom_violin(data=gw_peaks_detailed,aes(max_yr, max), adjust = 0.6, trim = T, scale = "count", fill = "grey90", colour = "grey40", draw_quantiles = c(0.25, 0.5, 0.75)) +
  #geom_split_violin(data=gw_peaks_detailed, aes(max_yr, max, fill = ssp), adjust = 0.6, trim = F, scale = "count")+
  # geom_split_violin(data=gw_peaks_detailed,aes(max_yr, max, fill = rcp), adjust = 0.6, trim = T)+
  geom_point(data=gw_peaks_detailed, aes(x= mean(max_yr), y=mean(max)), color="black", size=4) + #global mean
  #geom_point(data=gw_peaks_detailed, aes(x= median(max_yr), y=median(max)), color="black", size=4) + #global median
  geom_vline(data=gw_peaks_detailed, aes(xintercept = mean(max_yr)), size = 0.5, color = "grey80", linetype = "dashed")+
  geom_hline(data=gw_peaks_detailed, aes(yintercept = mean(max)), size = 0.5, color = "grey80", linetype = "dashed")+
  geom_point(data=quantiles, aes(x=mean.y, y=mean.v, color=rcp), shape=18, size=4 ) + #mean of every rcp
  geom_jitter(data=gw_peaks_detailed, aes(max_yr, max,color=rcp), alpha = 0.1) +
  #geom_pointrange(data=quantiles, aes(x=median.y, y=median.v, ymin=ten.v, ymax=ninety.v, color=rcp), size=1.5, alpha=0.75) +
  #geom_pointrange(data=quantiles, aes(x=median.y, y=median.v,xmax = ten.y, xmin = ninety.y, color=rcp), size=1.5, alpha=0.75) +
  geom_errorbar(data=quantiles, aes(x=median.y, y=median.v, ymin=ten.v, ymax=ninety.v, color=rcp), size=2, alpha=0.75) +
  geom_errorbarh(data=quantiles, aes(x=median.y, y=median.v,xmax = ten.y, xmin = ninety.y, color=rcp), size=2, alpha=0.75) +
  geom_point(data=quantiles, aes(x=median.y, y=median.v, color=rcp), size=4 ) + # median point
  # geom_rug(data=gw_peaks_detailed, aes(max_yr, max,color=rcp), length = unit(0.25, "cm"))+
  #geom_rug(data=quantiles, aes(median.y, median.v, color=rcp), length = unit(0.25, "cm"))+ #median.v, ten.v, ninety.v, median.y, ten.y, ninety.y, mean.v, mean.y
  geom_rug(data=quantiles, aes(ten.y, ten.v, color=rcp), size=1, length = unit(0.25, "cm"))+
  geom_rug(data=quantiles, aes(ninety.y, ninety.v, color=rcp), size=1, length = unit(0.25, "cm"))+
  #geom_text(data=maxssps, size=3, aes(x= 2100, y=1525, label = paste("Median peak year =", median.y,"\n","Maximum peak year =", max.y,"\n","Max. withdrawal =", max.v, ""), hjust = 1, vjust=0, parse = TRUE)) + # alternative geom_label
  geom_text(data=maxssps, size=3, aes(x= 2100, y=1450, label = paste("Tp,med =", median.y,"\n","Tp,max =", max.y,"\n","Qp,med =", median.v,"\n","Qp,max =", max.v, ""), hjust = 1, vjust=0, parse = TRUE)) + # alternative geom_label
  scale_color_manual(values = c("red2","forestgreen","dodgerblue2","purple")) +
  facet_wrap(~ssp, nrow=1)+
  labs(x="Peak Year",y=expression(paste("Global Peak Groundwater Withdrawal (",km^3,yr^-1,")"))) +
  theme_bw() +
  ylim(c(200,1600)) + xlim(c(2000,2100)) +
  theme(panel.spacing.x = unit(5, "mm"),
        #panel.grid.major = element_line(size = 0.1),
        # panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major=element_line(colour="white"),panel.grid.minor=element_line(colour="white"),
        axis.text = element_text(size=12), axis.title.x = element_text(color="black",size=15), axis.title.y = element_text(color="black",size=15),
        legend.position = "bottom", legend.title = element_blank(),
        strip.placement = "outside", strip.background = element_blank(), 
        strip.text = element_text(size=13,face="bold"),strip.text.y = element_text(angle=180)) -> synthesis_violins

synthesis_violins

#ggsave(filename = "synthesisplot_s.png", plot = synthesis_violins, width = 13, height = 6.5, units = c("in"), dpi = 300)
#ggsave(filename = "synthesisplot.pdf", plot = synthesis_violins, width = 13, height = 6.5, units = c("in"), dpi = 300)
#ggsave(filename = "synthesisplot.png", plot = last_plot(), width = 13, height = 6.5, units = c("in"), dpi = 300)

########################################################################-
# Extra SI Figures ----

## Synthesis violins by categories other than SSPs i.e. by RCPs, GCMs, Storage regimes, GW depletion limits, and Calibration datasets. 

gw_peaks_detailed %>% 
  mutate( gcm = gsub('gfdl', 'GFDL', gcm),
         gcm = gsub('hadg', 'HadGEM', gcm),
         gcm = gsub('ipsl', 'IPSL', gcm),
         gcm = gsub('miro', 'MIROC', gcm),
         gcm = gsub('nrsm', 'NorESM', gcm),
         calib = gsub('gl', 'PCR-GLOBWB', calib),
         calib = gsub('wg', 'WaterGapHM', calib),
         surf = gsub('rs', 'Restricted', surf),
         surf = gsub('ex', 'Expansion', surf),
         gw = gsub('lo', 'Low', gw),
         gw = gsub('md', 'Medium', gw),
         gw = gsub('hi', 'High', gw)) -> gw_peaks_detailed_rename

v <- ggplot() +
  geom_violin(data=gw_peaks_detailed_rename, aes(max_yr, max), adjust = 0.6, trim = T, scale = "count", fill = "grey90", colour = "grey40", draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_point(data=gw_peaks_detailed_rename, aes(x= mean(max_yr), y=mean(max)), color="black", size=4) + #global mean
  #geom_point(data=gw_peaks_detailed_rename, aes(x= median(max_yr), y=median(max)), color="black", size=4) + #global median
  geom_vline(data=gw_peaks_detailed_rename, aes(xintercept = mean(max_yr)), size = 0.5, color = "grey80", linetype = "dashed")+
  geom_hline(data=gw_peaks_detailed_rename, aes(yintercept = mean(max)), size = 0.5, color = "grey80", linetype = "dashed")+
  labs(x="Peak Year",y=expression(paste("Global Peak Groundwater Withdrawal (",km^3,yr^-1,")"))) +
  theme_bw() +
  ylim(c(200,1600)) + xlim(c(2000,2100)) +
  theme(panel.spacing.x = unit(5, "mm"),
        #panel.grid.major = element_line(size = 0.1),
        # panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major=element_line(colour="white"),panel.grid.minor=element_line(colour="white"),
        axis.text = element_text(size=12), axis.title.x = element_text(color="black",size=15), axis.title.y = element_text(color="black",size=15),
        legend.position = "bottom", legend.title = element_blank(),
        strip.placement = "outside", strip.background = element_blank(), 
        strip.text = element_text(size=13,face="bold"),strip.text.y = element_text(angle=180)) 

v + facet_wrap(~ssp, nrow=1) +  geom_jitter(data=gw_peaks_detailed_rename, aes(max_yr, max, color=ssp), alpha = 0.2) -> vssp
v + facet_wrap(~rcp, nrow=1) + geom_jitter(data=gw_peaks_detailed_rename, aes(max_yr, max, color=rcp), alpha = 0.2)  -> vrcp
v + facet_wrap(~gcm, nrow=1) + geom_jitter(data=gw_peaks_detailed_rename, aes(max_yr, max, color=gcm), alpha = 0.2)  -> vgcm
v + facet_wrap(~gw, nrow=1) + geom_jitter(data=gw_peaks_detailed_rename, aes(max_yr, max, color=gw), alpha = 0.2)  -> vgw
v + facet_wrap(~surf, nrow=1) + geom_jitter(data=gw_peaks_detailed_rename, aes(max_yr, max, color=surf), alpha = 0.2)  -> vsurf
v + facet_wrap(~calib, nrow=1)  + geom_jitter(data=gw_peaks_detailed_rename, aes(max_yr, max, color=calib), alpha = 0.2)  -> vcal

# saving
#ggsave(filename = "violin_ssp.png", plot = vssp, width = 13, height = 6.5, units = c("in"), dpi = 300)
#ggsave(filename = "violin_rcp.png", plot = vrcp, width = 13, height = 6.5, units = c("in"), dpi = 300)
#ggsave(filename = "violin_gcm.png", plot = vgcm, width = 13, height = 6.5, units = c("in"), dpi = 300)
#ggsave(filename = "violin_gw.png", plot = vgw, width = 13, height = 6.5, units = c("in"), dpi = 300)
#ggsave(filename = "violin_surf.png", plot = vsurf, width = 7, height = 6.5, units = c("in"), dpi = 300)
#ggsave(filename = "violin_cal.png", plot = vcal, width = 7, height = 6.5, units = c("in"), dpi = 300)


# for one big SI figures containing all violins 
#ggarrange(vssp, vrcp, vgcm, vgw, ncol=1, labels = c('(a)','(b)','(c)','(d)'), ggarrange(vsurf, vcal, ncol = 2, labels = c('(e)','(f)'))) -> violins_SI
# ggsave(filename = "synthesisplot_SI.png", plot = violins_SI, width = 10, height = 20, units = c("in"), dpi = 300)


########################################################################-
# preparing a file for dependence analysis 
gw_peaks_detailed %>% select(!ssp_rcp) %>% 
  mutate(ssp = gsub("SSP", "", ssp),
         rcp = gsub("RCP", "", rcp),
         #gcm = gsub(c('gfdl','hadg','ipsl','miro'), c('1','2','3','4'), gcm)
         gcm = gsub('gfdl', '1', gcm),
         gcm = gsub('hadg', '2', gcm),
         gcm = gsub('ipsl', '3', gcm),
         gcm = gsub('miro', '4', gcm),
         gcm = gsub('nrsm', '5', gcm),
         calib = gsub('gl', '1', calib),
         calib = gsub('wg', '2', calib),
         surf = gsub('rs', '1', surf),
         surf = gsub('ex', '2', surf),
         gw = gsub('lo', '1', gw),
         gw = gsub('md', '2', gw),
         gw = gsub('hi', '3', gw)) %>% 
  select(ssp, rcp, gcm, calib, surf, gw, max_yr, max, taken) ->
  gw_peaks_detailed_dep

# write.csv(gw_peaks_detailed_dep,file="peaks_dep.csv")





########################################################################-
# Junk ----

# per_rcp <- filter(gw_peaks,gw_peaks$rcp=="2p6"

# basic grid
g <- gw_peaks_detailed %>%
  complete(ssp_rcp = combos) %>%
  ggplot(aes(max_yr, max)) +
  ylim(c(0,1600)) +
  labs(y = "Peak withdrawal (km3/yr)",
       x = "Year")
# g


# eclipses by ssp
g +
  stat_ellipse(aes(fill = rcp),
               geom = "polygon",
               level = 0.90, alpha = 0.5, color = "black") +
  facet_wrap(~ssp, nrow = 1)+
  theme_bw()


# violin plot
gw_peaks_detailed %>%
  mutate(gw = factor(gw, levels = c("lo", "md", "hi"))) %>%
  #complete(ssp_rcp = combos) %>%
  #gather(metric, value, -calib, -ssp, -rcp, -surf, -gw) %>%
  ggplot(aes(rcp, max)) +
  geom_violin(adjust = 0.5, trim = F, scale = "count") +
  facet_wrap(~ssp, nrow = 1, strip.position = "top") +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values =  RColorBrewer::brewer.pal(3, "Spectral")) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()#,
        #axis.text = element_blank(),
        #strip.text.y = element_blank(),
        #strip.background = element_blank(),
        #panel.background = element_blank()
        ) +
  labs(y = "Global Groundwater Depletion (BCM)",
       x = NULL,
       shape = "Calibration data",
       fill = "Groundwater availability scenario")

