# R script to analyze groundwater peak and decline for Hejazi et al. (2019)
#
#
#
#
# -----------------------------------------------------------------------------
# Notes:
# -----------------------------------------------------------------------------
# This script can be used to analyze the water data derived from the SSP scenarios
#
##Remove anything stored in R##
rm(list = ls())
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
## Load libraries and source pre-defined functions
# -----------------------------------------------------------------------------
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(extrafont)
library(ggthemes)
library(checkpoint)
library(tidyr)
library(gridExtra)
#library(plyr)
library(RColorBrewer)
library(stringr)
library(devtools)
library(rgcam)
library(gcammaptools)
library(rgdal)
library(cowplot)
library(directlabels)
library(ggpmisc)
library(data.table)
library(raster)
library(scales)
library(ggpubr)

# setwd( "/Users/grah436/Desktop/groundwater_peak_and_decline" )
# setwd("C:/Users/niaz981/OneDrive - PNNL/Documents - GCIMS Water/1.4b Groundwater/2022_Hejazi_GroundwaterDepletionPeakLimits/Data/fromNeal")

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
#detach(package:plyr)
groundwater <-
  read.csv("groundwater_production_full_900.csv", sep = ",") %>% group_by(scenario, year) %>%
  summarise(value = sum(value)) %>% ungroup() %>% filter(year >= 2010) %>% separate(scenario, c('wd', 'hilo', 'ex', 'ssp', 'model', 'climate'), sep =
                                                                                      "_") %>% mutate(rcp = paste0("rcp", climate)) %>%
  mutate(
    wd = gsub("gl", "WGHM", wd),
    wd = gsub("wg", "PCR-GLOBWB", wd),
    hilo = gsub("lo", "5%", hilo),
    hilo = gsub("md", "25%", hilo),
    hilo = gsub("hi", "40%", hilo),
    ex = gsub("ex", "Expansion", ex),
    ex = gsub("rs", "Restricted", ex),
    hilo_f = factor(hilo, levels = c("5%", "25%", "40%")),
    ssp = gsub("ssp", "SSP", ssp),
    rcp = gsub("rcp", "RCP", rcp),
    rcp = gsub("p", ".", rcp),
    model = gsub("gfdl", "GFDL", model),
    model = gsub("hadg", "HadGEM", model),
    model = gsub("ipsl", "IPSL", model),
    model = gsub("miro", "MIROC", model),
    model = gsub("nrsm", "NorESM", model)
  )

groundwater %>% group_by(wd, hilo_f, ex, ssp, model, rcp) %>% 
  mutate(cumsum = cumsum(value)) %>%  dplyr::filter(year == 2100) -> cumsum

groundwater %>% dplyr::filter(year == 2100) -> groundwater

groundwater %>% group_by(wd, hilo_f, ex, ssp, rcp, model) %>% 
  mutate(max = max(value)) %>% dplyr::filter(max == value) %>% ungroup() -> max.year

groundwater <- groundwater  %>% mutate(groundwater = value / 120)
formattermax <- function(x) {x * 120}

cumsum <- cumsum %>% mutate(cumsum = cumsum / 2000)
formattercumsum <- function(x) {x * 2000}

max.year <- max.year %>% mutate(max.year = year / 2.1)
formatteryear <- function(x) {x * 2.1}

max.year %>% 
  ggplot() + 
  geom_density(aes(x = year, fill = ssp), alpha = 0.4) + 
  labs(x = "", y = "") + scale_fill_brewer(palette = "Set1") + 
  theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> a

max.year  %>% ggplot() + geom_density(aes(x = year, fill = rcp), alpha = 0.4) + 
  labs(x = "", y = "") + scale_fill_brewer(palette = "Set2") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> b

max.year  %>% ggplot() + geom_density(aes(x = year, fill = model), alpha = 0.4) + 
  labs(x = "", y = "") + scale_fill_brewer(palette = "Spectral") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> c

max.year  %>% ggplot() + geom_density(aes(x = year, fill = ex), alpha = 0.4) + 
  labs(x = "", y = "")  + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> d

max.year  %>% ggplot() + geom_density(aes(x = year, fill = hilo_f), alpha = 0.4) + 
  labs(x = "", y = "")  + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> e

max.year  %>% ggplot() + geom_density(aes(x = year, fill = wd), alpha = 0.4) + 
  labs(x = "", y = "") + scale_fill_brewer(palette = "Set1") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> f

ggarrange(a, b, c, d, e, f, ncol = 3, nrow = 2) -> cummulative.sum

annotate_figure(
  cummulative.sum,
  top = text_grob(
    "Year of Peak Groundwater Withdrawal",
    color = "black",
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  ),
  left = text_grob(
    "Relative Frequency",
    color = "black",
    rot = 90,
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  )
) -> cummulative.sum

cummulative.sum

# ggsave(filename="allvariationsdensities_yrs.png",plot=cummulative.sum,width=15.0,height=10.0)
# ggsave(filename="allvariationsdensities_yrsv1.pdf",plot=cummulative.sum,width=12.0,height=7.33)

# Global groundwater withdrawals 

groundwater %>% dplyr::filter(year == 2100) %>% 
  ggplot() + 
  geom_density(aes(x = groundwater, fill = ssp), alpha = 0.4) + 
  labs(x = "", y = "") +  scale_x_continuous(labels = formattermax) + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> a

groundwater %>% dplyr::filter(year == 2100) %>% 
  ggplot() + 
  geom_density(aes(x = groundwater, fill = rcp), alpha = 0.4) + 
  labs(x = "", y = "") + scale_x_continuous(labels = formattermax) + 
  scale_fill_brewer(palette = "Set2") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> b

groundwater %>% dplyr::filter(year == 2100) %>%
  ggplot() + 
  geom_density(aes(x = groundwater, fill = model), alpha = 0.4) +
  labs(x = "", y = "") + 
  scale_x_continuous(labels = formattermax) + scale_fill_brewer(palette = "Spectral") +
  theme_bw() +
  theme(legend.background = element_blank(),
        plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> c

groundwater %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                              groundwater, fill = ex), alpha = 0.4) + labs(x = "", y = "") +  scale_x_continuous(labels =
                                                                                                                                                                   formattermax) + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> d

groundwater %>% dplyr::filter(year == 2100) %>% ggplot() + 
  geom_density(aes(x = groundwater, fill = hilo_f), alpha = 0.4) + labs(x = "", y = "") + 
  scale_x_continuous(labels = formattermax) + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> e

groundwater %>% dplyr::filter(year == 2100) %>% ggplot() + 
  geom_density(aes(x = groundwater, fill = wd), alpha = 0.4) + 
  labs(x = "", y = "") + 
  scale_x_continuous(labels = formattermax) + 
  scale_fill_brewer(palette = "Set1") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> f

ggarrange(a, b, c, d, e, f, ncol = 3, nrow = 2) -> cummulative.sum

annotate_figure(
  cummulative.sum,
  top = text_grob(
    "Global Groundwater Withdrawals in 2100 [bcm]",
    color = "black",
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  ),
  left = text_grob(
    "Relative Frequency",
    color = "black",
    rot = 90,
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  )
) -> cummulative.sum

cummulative.sum 

# ggsave(filename="allvariationsdensities.png",plot=cummulative.sum,width=15.0,height=10.0)
# ggsave(filename="allvariationsdensitiesv1.pdf",plot=cummulative.sum,width=12.0,height=7.33)

# Global cummulative withdrawals 

cumsum %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                         cumsum, fill = ssp), alpha = 0.4) + labs(x = "", y = "") +  scale_x_continuous(labels =
                                                                                                                                                          formattercumsum) + scale_fill_brewer(palette = "Set1") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> a

cumsum %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                         cumsum, fill = rcp), alpha = 0.4) + labs(x = "", y = "") + scale_x_continuous(labels =
                                                                                                                                                         formattercumsum) + scale_fill_brewer(palette = "Set2") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> b

cumsum %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                         cumsum, fill = model), alpha = 0.4) + labs(x = "", y = "") + scale_x_continuous(labels =
                                                                                                                                                           formattercumsum) + scale_fill_brewer(palette = "Spectral") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> c

cumsum %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                         cumsum, fill = ex), alpha = 0.4) + labs(x = "", y = "") +  scale_x_continuous(labels =
                                                                                                                                                         formattercumsum) + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> d

cumsum %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                         cumsum, fill = hilo_f), alpha = 0.4) + labs(x = "", y = "") + scale_x_continuous(labels =
                                                                                                                                                            formattercumsum) + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> e

cumsum %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                         cumsum, fill = wd), alpha = 0.4) + labs(x = "", y = "") + scale_x_continuous(labels =
                                                                                                                                                        formattercumsum) + scale_fill_brewer(palette = "Set1") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.80, 0.75),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> f

ggarrange(a, b, c, d, e, f, ncol = 3) -> cummulative.sum

annotate_figure(
  cummulative.sum,
  top = text_grob(
    "Cummulative Global Groundwater Depletion (2010-2100) [bcm]",
    color = "black",
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  ),
  left = text_grob(
    "Relative Frequency",
    color = "black",
    rot = 90,
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  )
) -> cummulative.sum

# ggsave(filename="allvariationsdensities_cum.png",plot=cummulative.sum,width=15.0,height=10.0)
# ggsave(filename="allvariationsdensities_cumv1.pdf",plot=cummulative.sum,width=12.0,height=7.33)


## Find the peak groundwater extraction time for each scenario

groundwater %>% group_by(wd, hilo, ex, ssp, rcp, model) %>% mutate(max =
                                                                     max(value)) %>% dplyr::filter(max == value) %>% ungroup() %>%
  group_by(ssp, rcp) %>% summarise(
    median.v = median(value),
    ten.v = quantile(value, probs = 0.1),
    ninety.v = quantile(value, probs = 0.9),
    median.y = median(year),
    ten.y = quantile(year, probs = 0.1),
    ninety.y = quantile(year, probs = 0.9)
  ) %>%
  mutate(ssp = gsub("ssp", "SSP", ssp),
         rcp = gsub("rcp", "RCP", rcp)) ->
  quantiles

colors <-
  c(
    "RCP2p6" = "red2",
    "RCP4p5" = "forestgreen",
    "RCP6p0" = "dodgerblue2",
    "RCP8p5" = "purple"
  )

quantiles %>%
  ggplot() +
  geom_point(aes(x = median.y, y = median.v, color = rcp), size = 1.5) +
  geom_errorbar(aes(
    x = median.y,
    y = median.v,
    ymin = ten.v,
    ymax = ninety.v,
    color = rcp
  )) +
  geom_errorbarh(aes(
    x = median.y,
    y = median.v,
    xmax = ten.y,
    xmin = ninety.y,
    color = rcp
  )) +
  scale_color_manual(values = colors) +
  labs(x = "Year of Peak Groundwater Extraction", y = expression(paste("Peak Groundwater Extraction (", km ^
                                                                         3, ")"))) +
  facet_wrap(~ ssp, nrow = 1) +
  theme_bw() + theme(
    panel.spacing.x = unit(10, "mm"),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    axis.title.x = element_text(color = "black", size = 15),
    axis.title.y = element_text(color = "black", size = 15),
    legend.position = "bottom",
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    strip.text.y = element_text(angle = 180)
  )
#ggsave("peak_quantiles.png",width=12,height=8)


groundwater %>% mutate(value = if_else(year == 2100, value, value * 5)) %>% group_by(wd, hilo, ex, ssp, rcp, model) %>% summarise(value = sum(value)) %>% ungroup() %>%
  group_by(ssp, rcp) %>% summarise(mean = mean(value), sd = sd(value)) -> x

groundwater %>% mutate(value = if_else(year == 2100, value, value * 5)) %>% group_by(wd, hilo, ex, ssp, rcp, model) %>% summarise(value = sum(value)) %>% ungroup()
# %>%  write.csv("total_gw_extraction.csv")

groundwater %>% group_by(wd, hilo, ex, ssp, rcp, model) %>% mutate(max_value =
                                                                     max(value)) %>% dplyr::filter(max_value == value) %>% ungroup() %>%
  rename(peak_year = year) %>% select(-hilo_f,-value,-climate) # %>% write.csv("peak_gw_extraction.csv")

read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0) %>% group_by(scenario, groundwater) %>% mutate(max_value = max(value)) %>% ungroup() %>%
  mutate(peak = if_else((max_value == value) &
                          (year < 2100), 1, 0)) %>% dplyr::filter(peak == 1) %>% group_by(groundwater) %>%
  summarise(sum = sum(peak) / 900) %>%
  mutate(
    groundwater = gsub("-water withdrawals", "", groundwater),
    groundwater = gsub("-", "_", groundwater),
    groundwater = gsub(" ", "_", groundwater)
  ) %>% rename(subRegion = groundwater, value = sum) -> x

unique(mapGCAMBasins@data$subRegion)
metis.mapsProcess(
  polygonTable = x,
  subRegShape = metis::mapGCAMBasins,
  classPalette = "YlOrRd",
  mapTitleOn = F,
  legendOutsideSingle = F,
  legendPosition = c('LEFT', 'bottom'),
  legendTitleSizeI = 0.001,
  fillshowNA = F,
  legendTextSizeI = 0.4,
  facetLabelColor = NULL,
  facetLabelSize = 0,
  legendFixedBreaks = 20,
  extension = T,
  scaleRange = c(0, 1, 0.05),
  folderName = "probability"
)

read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0 &
                                                                               year >= 2010) %>% group_by(scenario, groundwater) %>% mutate(max_value =
                                                                                                                                              max(value)) %>% ungroup() %>%
  dplyr::filter(max_value == value) %>% group_by(groundwater) %>%
  summarise(median = median(year)) %>%
  mutate(
    groundwater = gsub("-water withdrawals", "", groundwater),
    groundwater = gsub("-", "_", groundwater),
    groundwater = gsub(" ", "_", groundwater),
    units = "Median Peak Extraction Year"
  ) %>% rename(subRegion = groundwater, value = median) -> x

numeric2Cat_param <- list("param")
numeric2Cat_breaks <-
  list(c(2000, 2020.1, 2040.1, 2060.1, 2080.1, 2100.1))
numeric2Cat_labels <-
  list(c(
    "2000 - 2020",
    "2020 - 2040",
    "2040 - 2060",
    "2060 - 2080",
    "2080 - 2100"
  ))
numeric2Cat_palette <-
  list(
    c(
      "2000 - 2020" = "lightskyblue",
      "2020 - 2040" = "dodgerblue",
      "2040 - 2060" = "royalblue1",
      "2060 - 2080" = "slateblue2",
      "2080 - 2100" = "darkslateblue"
    )
  )
numeric2Cat_legendTextSize <- list(c(0.5), c(0.5))
numeric2Cat_list <- list(
  numeric2Cat_param = numeric2Cat_param,
  numeric2Cat_breaks = numeric2Cat_breaks,
  numeric2Cat_labels = numeric2Cat_labels,
  numeric2Cat_palette = numeric2Cat_palette,
  numeric2Cat_legendTextSize = numeric2Cat_legendTextSize
)

metis.mapsProcess(
  polygonTable = x,
  subRegShape = metis::mapGCAMBasins,
  classPalette = "PuBuGn",
  mapTitleOn = F,
  legendOutsideSingle = F,
  numeric2Cat_list = numeric2Cat_list,
  legendPosition = c('LEFT', 'bottom'),
  legendTitleSizeI = 0.5,
  fillshowNA = F,
  legendTextSizeI = 0.4,
  facetLabelColor = NULL,
  facetLabelSize = 0,
  extension = T,
  scaleRange = c(2010, 2100),
  folderName = "median_year"
)




read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0) %>% dplyr::filter(year >= 2010) %>% group_by(scenario, year) %>% summarise(value =
                                                                                                                                                               sum(value)) %>%
  ungroup() %>% group_by(year) %>% summarise(
    median = median(value),
    iqr.25 = quantile(value, probs = 0.25),
    iqr.5 = quantile(value, probs = 0.05),
    iqr.75 = quantile(value, probs = 0.75),
    iqr.95 = quantile(value, probs = 0.95)
  ) ->
  year_total

read.csv("historyv1.csv", sep = ",") -> history
read.csv("konikowv1.csv", sep = ",") -> konikow

colourCount = length(unique(history$study))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

year_total %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = iqr.5, ymax = iqr.95),
              fill = "grey",
              alpha = 0.35) +
  geom_ribbon(aes(x = year, ymin = iqr.25, ymax = iqr.75),
              fill = "dodgerblue",
              alpha = 0.55) +
  geom_line(aes(x = year, y = median), color = "black", size = 1.5) +
  geom_ribbon(
    data = konikow,
    aes(x = year, ymin = ymin, ymax = ymax),
    fill = "forestgreen",
    alpha = 0.55
  ) +
  geom_line(
    data = konikow,
    aes(x = year, y = value, linetype = study),
    color = "black",
    size = 1.5
  ) +
  geom_point(
    data = history,
    aes(
      x = year,
      y = depletion,
      color = study,
      group = study
    ),
    size = 2
  ) +
  scale_color_manual(values = getPalette(colourCount), name = "") +
  scale_linetype_manual(values = c("twodash"), name = "") +
  ylim(0, 1200) +
  scale_x_continuous(
    expand = c(0.06, 0),
    breaks = c(
      1960,
      1980,
      2000,
      2010,
      2020,
      2030,
      2040,
      2050,
      2060,
      2070,
      2080,
      2090,
      2100
    ),
    labels = c(
      "1960",
      "1980",
      "2000",
      "2010",
      "",
      "",
      "",
      "2050",
      "",
      "",
      "",
      "",
      "2100"
    )
  ) +
  #scale_x_continuous(expand = c(0,0),breaks=c(2010,2020,2030,2040,2050,2060,2070,2080,2090,2100), labels=c("2010","","","","2050","","","","","2100")) +
  labs(y = expression(paste("Global Groundwater Depletion (", km ^ 3, ")")), x =
         "") +
  #guides(linetype="legend") +
  theme_bw() + theme(
    legend.text = element_text(size = 13),
    legend.position = "bottom",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.margin = margin(10, 20, 10, 10)
  ) -> line

read.csv("groundwater_production_full_900.csv", sep = ",") %>%
  dplyr::filter(value > 0) %>% dplyr::filter(year >= 2010) %>%
  group_by(scenario, year) %>% summarise(value = sum(value)) %>% ungroup() %>%
  group_by(scenario) %>% summarise(max = max(value))  %>% ungroup() %>%
  summarise(
    median = median(max),
    iqr.5 = quantile(max, 0.05),
    iqr.95 = quantile(max, 0.95)
  ) -> withdrawal.quantiles

read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0) %>% dplyr::filter(year >= 2010) %>% group_by(scenario, year) %>% summarise(value =
                                                                                                                                                               sum(value)) %>% ungroup() %>% group_by(scenario) %>% summarise(max = max(value))  %>%
  dplyr::filter(max <= 1650) %>%
  ggplot(aes(x = max)) + geom_density(fill = "dodgerblue", alpha = 0.5) +
  #geom_vline(xintercept = withdrawal.quantiles$iqr.5,color="black",size=0.5) +
  #geom_text(x = withdrawal.quantiles$iqr.5 - 50, y= 0.001,label=round(withdrawal.quantiles$iqr.5, digits=0), color="black") +
  #geom_vline(xintercept = withdrawal.quantiles$median,color="black",size=0.5) +
  #geom_text(x = withdrawal.quantiles$median - 50, y= 0.001,label=round(withdrawal.quantiles$median, digits=0), color="black") +
  #geom_vline(xintercept = withdrawal.quantiles$iqr.95,color="black",size=0.5) +
  #geom_text(x = withdrawal.quantiles$iqr.95 - 50, y= 0.001,label=round(withdrawal.quantiles$iqr.95, digits=0), color="black") +
  labs(title = "", x = "", y = "") + xlim(0, 1700)  + coord_flip() +
  theme(
    plot.margin = margin(-12, 1, 1,-3),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) ->
  plot_right


read.csv("groundwater_production_full_900.csv", sep = ",") %>%
  dplyr::filter(value > 0) %>% dplyr::filter(year >= 2010) %>%
  group_by(scenario, year) %>% summarise(value = sum(value)) %>% ungroup() %>%
  group_by(scenario) %>% mutate(max = max(value))  %>%
  ungroup() %>% filter(max == value) %>%
  summarise(
    median = median(year),
    iqr.5 = quantile(year, 0.75),
    iqr.95 = quantile(year, 0.99)
  ) -> year.quantiles

read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0) %>% dplyr::filter(year >= 2010) %>% group_by(scenario, year) %>% summarise(value =
                                                                                                                                                               sum(value)) %>% ungroup() %>% group_by(scenario) %>% mutate(max = max(value))  %>%
  ungroup() %>% filter(max == value) %>%
  ggplot(aes(x = as.integer(year)))  + geom_density(fill = "purple1", alpha =
                                                      0.5) +
  geom_vline(xintercept = year.quantiles$iqr.5,
             color = "black",
             size = 0.5) +
  geom_text(
    x = year.quantiles$iqr.5 - 2,
    y = 0.02,
    label = "75%",
    color = "black"
  ) +
  geom_vline(xintercept = year.quantiles$median,
             color = "black",
             size = 0.5) +
  geom_text(
    x = year.quantiles$median - 2,
    y = 0.02,
    label = "50%",
    color = "black"
  ) +
  geom_vline(xintercept = year.quantiles$iqr.95,
             color = "black",
             size = 0.5) +
  geom_text(
    x = year.quantiles$iqr.95 - 2,
    y = 0.02,
    label = "99%",
    color = "black"
  ) +
  labs(title = "", x = "", y = "")   + #xlim(1960,2100) + #scale_x_continuous(expand = c(0.05,0)) +
  #labs(title="",x="",y="")    + scale_x_continuous(expand = c(0,0)) +
  scale_x_continuous(
    limits = c(1960, 2100),
    breaks = c(2045, 2050, 2100),
    labels = c("2045", "2050", "2100"),
    expand = c(0.06, 0)
  ) +
  theme(
    plot.margin = margin(1, -3, -12, 1),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 10)
  ) + theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) ->
  plot_top

empty <- ggplot() + geom_point(aes(1, 1), color = "white") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(1, 1, -12, -3)
  )
# library( egg )
# ggarrange(plot_top, empty, line, plot_right, ncol=2,widths = c(1,0.3),heights = c(0.7,2))
ggarrange(plot_top, line, ncol = 1, heights = c(0.7, 2))

read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0) %>% mutate(value = if_else(year == 2100, value, value * 5)) %>% dplyr::filter(year >=
                                                                                                                                                                  2010) %>% group_by(scenario, year) %>% summarise(value = sum(value)) %>% mutate(value =
                                                                                                                                                                                                                                                    cumsum(value)) %>%
  ungroup() %>% group_by(year) %>% summarise(
    median = median(value),
    iqr.25 = quantile(value, probs = 0.25),
    iqr.5 = quantile(value, probs = 0.05),
    iqr.75 = quantile(value, probs = 0.75),
    iqr.95 = quantile(value, probs = 0.95)
  ) ->
  year_total

year_total %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = iqr.5, ymax = iqr.95),
              fill = "grey",
              alpha = 0.35) +
  geom_ribbon(aes(x = year, ymin = iqr.25, ymax = iqr.75),
              fill = "dodgerblue",
              alpha = 0.55) +
  geom_line(aes(x = year, y = median), color = "black", size = 1.5) +
  #ylim(0,1650) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = c(2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
    labels = c("2010", "", "", "", "2050", "", "", "", "", "2100")
  ) +
  labs(y = expression(paste(
    "Cumulative Global Groundwater Depletion (", km ^ 3, ")"
  )), x = "") +
  theme_bw() + theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    plot.margin = margin(10, 20, 10, 10)
  )



line <- ggplot(data = per.capita) +
  geom_line(aes(x = year, y = value, group = scenario), color = "blue") +
  #ylim(0,200) +
  scale_x_discrete(labels = c(2010, rep("", 7), 2050, rep("", 9), 2100)) +
  labs(title = "",
       x = "Year",
       y = expression(atop(Global ~ groundwater ~ depletion, paste((m ^ 3 ~ person ^
                                                                      -1 ~ year ^ -1)
       )))) +
  colScaleStudy +
  theme_bw() + theme(plot.margin = margin(-12,-3, 1, 1),
                     axis.text.x = element_text(size = 14, face = "bold"))


max.value.test <-
  per.capita %>% group_by(scenario) %>% summarise(capita = max(capita))
plot_right <- max.value.test %>%
  ggplot(aes(x = capita))  + geom_density(fill = "blue", alpha = 0.4) + labs(title =
                                                                               "", x = "", y = "") + xlim(0, 200) + colFillStudy + coord_flip() +
  theme(
    plot.margin = margin(-12, 1, 1,-3),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  )
max.year <-
  max.value.test %>% merge(per.capita, by = c("scenario", "capita"))
max.year$year <- as.factor(max.year$year)
plot_top <- max.year %>%
  ggplot(aes(x = as.integer(year)))  + geom_density(fill = "blue", alpha =
                                                      0.4) + labs(title = "", x = "", y = "")   +
  theme(
    plot.margin = margin(1, -3, -12, 1),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  )


empty <- ggplot() + geom_point(aes(1, 1), color = "white") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(1, 1, -12, -3)
  )
library(egg)
p <-
  ggarrange(
    plot_top,
    empty,
    line,
    plot_right,
    ncol = 2,
    widths = c(1, 0.3),
    heights = c(0.7, 2)
  )
#ggsave(filename="percapita_groundwater.png",plot=p,width=6.0,height=6.0)






process_production_data <- function(gcm) {
  read.csv(paste0("ag_production_by_subsector_", gcm, ".csv")) %>%
    filter(sector != "Forest")  %>% filter(year == 2100) %>%
    filter(grepl("IRR", technology)) %>%
    mutate(
      gcm = gcm,
      ssp = substr(scenario, 10, 13),
      rcp = substr(scenario, 20, 22)
    ) %>%
    dplyr::select(-Units, -output, -technology, -sector) %>%
    group_by(scenario, subsector, year) %>% summarise(value = sum(value)) %>%
    ungroup() %>%
    mutate(
      subsector = gsub("Root_Tuber", "RootTuber", subsector),
      subsector = gsub("biomass_grass", "biomass-grass", subsector),
      subsector = gsub("biomass_tree", "biomass-tree", subsector)
    ) %>%
    separate(subsector, c("crop", "basin"), sep = "_")
}

bind_rows(
  process_production_data("gfdl"),
  process_production_data("hadg"),
  process_production_data("nrsm"),
  process_production_data("miro"),
  process_production_data("ipsl")
) -> ag.prod.irr

read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0) %>%
  group_by(scenario, groundwater) %>% mutate(max_value = max(value)) %>% ungroup() %>%
  filter(max_value == value) %>% filter(year == 2100) %>% mutate(groundwater = gsub("-water withdrawals", "", groundwater)) -> peak.decline.basins


read.csv("basin_ID_swap.csv") -> basin.id

peak.decline.basins %>% left_join(basin.id, by = c("groundwater" = "basin.name.old")) -> peak.decline.basins

ag.prod.irr %>% anti_join(
  peak.decline.basins %>% select(scenario, basin.name),
  by = c("scenario", "basin" = "basin.name")
) ->
  peak.decline.prod

peak.decline.prod %>% group_by(scenario, crop) %>% summarise(value = sum(value)) ->
  global.peak.decline.prod#%>%
#write.csv("2100_ag_production_in_peak_and_decline_basins.csv")

ag.prod.irr %>% dplyr::filter(!grepl("biomass", crop)) %>% group_by(scenario, year) %>% summarise(global.prod =
                                                                                                    sum(value)) %>% ungroup() %>% full_join(
                                                                                                      global.peak.decline.prod %>% dplyr::filter(!grepl("biomass", crop)) %>%
                                                                                                        group_by(scenario, year) %>% summarise(value =
                                                                                                                                                 sum(value)) %>% ungroup(),
                                                                                                      by = c("scenario", "year")
                                                                                                    ) %>%
  mutate(percent = value / global.prod) -> pdf.irr.prod

ag.prod.irr %>% group_by(scenario, crop) %>% summarise(global.prod = sum(value)) %>% ungroup() %>% full_join(
  global.peak.decline.prod %>%
    group_by(scenario, crop) %>% summarise(value =
                                             sum(value)) %>% ungroup(),
  by = c("scenario", "crop")
) %>%
  mutate(percent = (value / global.prod) * 100) -> pdf.irr.prod

pdf.irr.prod %>% group_by(crop) %>% summarise(
  mean = mean(percent),
  sd = sd(percent),
  min = min(percent),
  max = max(percent),
  range = range(percent)
) -> percent

pdf.irr.prod %>%
  ggplot(aes(x = percent)) + geom_density(fill = "purple1", alpha = 0.4) + geom_point(data = (percent), aes(x =
                                                                                                              max, y = 0.25)) + geom_text(data = (percent), aes(
                                                                                                                x = max,
                                                                                                                y = 0.245,
                                                                                                                label = paste0(round(max, digits = 0), "%")
                                                                                                              )) +
  geom_point(data = (percent), aes(x = min, y = 0.25)) + geom_text(data = (percent), aes(
    x = min,
    y = 0.245,
    label = paste0(round(min, digits = 0), "%")
  )) +
  #geom_line(data = (percent),aes(x=value,y=0.09)) +
  geom_line(data = (percent %>% gather(range, value, max:min)), aes(x =
                                                                      value, y = 0.25)) +
  geom_text(data = (percent), aes(
    x = 85,
    y = 0.275,
    label = paste0("Mean = ", round(mean, digits = 0), "%")
  )) + geom_text(data = (percent), aes(
    x = 85,
    y = 0.265,
    label = paste0("S.D. = ", round(sd, digits = 0), "%")
  )) + labs(title = "", y = "Frequency", x = "Percentage of Total Global Irrigated Crop Production")  +
  scale_x_continuous(expand = c(0.05, 0)) +
  facet_wrap(~ crop, ncol = 4, scales = "free") + theme_bw() + theme(axis.text =
                                                                       element_text(size = 15),
                                                                     axis.title = element_text(size = 15))
#ggsave(filename="pdf_ag_total_crop.png",width=10.0,height=8.0)



ag.prod.irr %>% group_by(scenario, year) %>% summarise(value = sum(value)) %>% ungroup() ->
  ag.prod.global

ag.prod.global %>% write.csv("global_irrigated_prod.csv")

read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0) %>%
  group_by(scenario, year) %>% summarise(value = sum(value)) %>% ungroup() %>% group_by(scenario) %>% mutate(max_value =
                                                                                                               max(value)) %>% ungroup() %>%
  filter(max_value == value) %>% filter(year == 2100) -> peak.decline.scenarios

read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0) %>%
  group_by(scenario, groundwater) %>% mutate(max_value = max(value)) %>% ungroup() %>%
  filter(max_value == value) %>% filter(year == 2100) %>% mutate(groundwater = gsub("-water withdrawals", "", groundwater)) -> peak.decline.basins

read.csv("basin_ID_swap.csv") -> basin.id

peak.decline.basins %>% left_join(basin.id, by = c("groundwater" = "basin.name.old"))

ag.prod.irr %>% separate(subsector, c("crop", "basin", sep = "_")) -> ag.prod.irr

##Change scenario names from shorthand to full
#groundwater <- groundwater[!(groundwater$year==2015 & groundwater$scenario=="gl_md_ex_ssp4_hadg_4p5"),]



###Calculate cumulative sum of groundwater extraction, and the year at which extraction peaks for each scenario ----
cumsum <-
  groundwater %>% group_by(wd, hilo, ex, ssp, model, climate) %>% mutate(cumsum =
                                                                           cumsum(value))
max.year <-
  groundwater %>% group_by(wd, hilo, ex, ssp, model, climate) %>% mutate(max =
                                                                           max(value)) %>% dplyr::filter(value == max)

rs <-
  filter(groundwater, ex == "Restricted") %>% colnameReplace("value", "standard")
ex <- filter(groundwater, ex == "Expansion")

mean <-
  ex %>% group_by(ex) %>% summarise(
    mean = mean(value),
    stdev = sd(value),
    median = median(value)
  )

### Format data for geom_density by making into factor of 100
groundwater <- groundwater  %>% mutate(value = value / 120)
formattermax <- function(x) {
  x * 120
}
cumsum <- cumsum %>% mutate(cumsum = cumsum / 2000)
formattercumsum <- function(x) {
  x * 2000
}
max.year <- max.year %>% mutate(year = year / 2.1)
formatteryear <- function(x) {
  x * 2.1
}


####Create a 3x3 matrix of density plots for 2100 groundwater withdrawals, cumulative withdrawals, and peak year
groundwater %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                              value, fill = ex), alpha = 0.4) + labs(x = "", y = "") + scale_x_continuous(labels =
                                                                                                                                                            formattermax) + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.75, 0.5),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> a

groundwater %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                              value, fill = hilo_f), alpha = 0.4) + labs(x = "", y = "") + scale_x_continuous(labels =
                                                                                                                                                                formattermax) + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.75, 0.5),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> b

groundwater %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                              value, fill = wd), alpha = 0.4) + labs(x = "", y = "") + scale_x_continuous(labels =
                                                                                                                                                            formattermax) + scale_fill_brewer(palette = "Set1") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.75, 0.5),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> c
ggarrange(a, b, c, ncol = 3, common.legend = F) -> withdrawals
annotate_figure(
  withdrawals,
  top = text_grob(
    "2100 Global Groundwater Withdrawals [bcm]",
    color = "black",
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  ),
  left = text_grob(
    "Relative Frequency",
    color = "black",
    rot = 90,
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  )
) -> withdrawals

cumsum %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                         cumsum, fill = ex), alpha = 0.4) + labs(x = "", y = "") +  scale_x_continuous(labels =
                                                                                                                                                         formattercumsum) + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> d

cumsum %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                         cumsum, fill = hilo_f), alpha = 0.4) + labs(x = "", y = "") + scale_x_continuous(labels =
                                                                                                                                                            formattercumsum) + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> e

cumsum %>% dplyr::filter(year == 2100) %>% ggplot() + geom_density(aes(x =
                                                                         cumsum, fill = wd), alpha = 0.4) + labs(x = "", y = "") + scale_x_continuous(labels =
                                                                                                                                                        formattercumsum) + scale_fill_brewer(palette = "Set1") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> f
ggarrange(d, e, f, ncol = 3, common.legend = F) -> cummulative.sum
annotate_figure(
  cummulative.sum,
  top = text_grob(
    "Cummulative Global Groundwater Depletion (2010-2100) [bcm]",
    color = "black",
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  ),
  left = text_grob(
    "Relative Frequency",
    color = "black",
    rot = 90,
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  )
) -> cummulative.sum


max.year  %>% ggplot() + geom_density(aes(x = year, fill = ex), alpha =
                                        0.4) + labs(x = "", y = "") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> g

max.year  %>% ggplot() + geom_density(aes(x = year, fill = hilo_f), alpha =
                                        0.4) + labs(x = "", y = "") +  theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> h

max.year  %>% ggplot() + geom_density(aes(x = year, fill = wd), alpha =
                                        0.4) + labs(x = "", y = "")  + scale_fill_brewer(palette = "Set1") + theme_bw() +
  theme(
    legend.background = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) -> i
ggarrange(g, h, i, ncol = 3, common.legend = F) -> year.max
annotate_figure(
  year.max,
  top = text_grob(
    "Year of Peak Groundwater Withdrawals",
    color = "black",
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  ),
  left = text_grob(
    "Relative Frequency",
    color = "black",
    rot = 90,
    hjust = 0.5,
    vjust = 0.5,
    face = "plain",
    size = 15
  )
) -> year.max


ggarrange(withdrawals,
          cummulative.sum,
          year.max,
          nrow = 3,
          common.legend = F) -> fig.7
# ggsave(fig.7,filename="3x3_matrix_groundwater.png",width=10.0,height=10.0)

groundwater <-
  aggregate(value ~ year + scenario, data = groundwater, sum) %>% filter(year >=
                                                                           2010)


getwd()
#setwd( "/Users/graham/Desktop/SSP_Climate" )
files <- list.files(pattern = "*pop.csv")
population <- rbindlist(lapply(files, fread))
population <-
  separate(population, scenario, c('ssp', 'rcp'), sep = "_")
##Divde by 6 as the population data being read in is across 6 scenarios
population <-
  aggregate(value ~ year + ssp + rcp, data = population, sum) %>% colnameReplace("value", "population") %>% mutate(population = population /
                                                                                                                     6)

##Calculate the amount of groundwater extraction per person
per.capita <-
  merge(groundwater, population, by = c("year", "ssp", "rcp")) %>% mutate(capita = (value *
                                                                                      1000000000) / (population * 1000))
per.capita$scenario <-
  paste(
    per.capita$ssp,
    per.capita$rcp,
    per.capita$wd,
    per.capita$hilo,
    per.capita$ex,
    per.capita$model,
    sep = "_"
  )

per.capita$year <- as.factor(per.capita$year)
#per.capita %>% #filter(ssp=="ssp5") %>%
line <- ggplot(data = per.capita) +
  geom_line(aes(x = year, y = value, group = scenario), color = "blue") +
  #ylim(0,200) +
  scale_x_discrete(labels = c(2010, rep("", 7), 2050, rep("", 9), 2100)) +
  labs(title = "",
       x = "Year",
       y = expression(atop(Global ~ groundwater ~ depletion, paste((m ^ 3 ~ person ^
                                                                      -1 ~ year ^ -1)
       )))) +
  colScaleStudy +
  theme_bw() + theme(plot.margin = margin(-12,-3, 1, 1),
                     axis.text.x = element_text(size = 14, face = "bold"))
#ggsave(filename="percapita_for_mohamad_rcp.png",width=10.0,height=6.0)


##########Absolute value analysis
#### Here we create various per capita groundwater plots in which we use both line and density plots
#### Empty figures are used in combination with ggarange to achieve correct formatting.
detach(package:plyr)
max.value.test <-
  per.capita %>% group_by(ssp, scenario) %>% summarise(value = max(value))
plot_topleft <- max.value.test %>%
  ggplot(aes(x = value, fill = ssp))  + geom_density(alpha = 0.4) + labs(title =
                                                                           "", x = "", y = "")  + colFillStudy  + xlim(0, 2000) +
  theme(
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) + theme_bw()
max.year <-
  max.value.test %>% merge(per.capita, by = c("scenario", "value", "ssp"))
max.year$year <- as.factor(max.year$year)
plot_bottomleft <- max.year %>%
  ggplot(aes(x = as.integer(year), fill = ssp))  + geom_density(alpha =
                                                                  0.4) + labs(title = "", x = "", y = "")  + colFillStudy  +
  theme(
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) + theme_bw()
max.value.test <-
  per.capita %>% group_by(rcp, scenario) %>% summarise(value = max(value))
plot_topleft <- max.value.test %>%
  ggplot(aes(x = value, fill = rcp))  + geom_density(alpha = 0.4) + labs(title =
                                                                           "", x = "", y = "")  + colFillStudy  + xlim(0, 2000) +
  theme(
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) + theme_bw()
max.year <-
  max.value.test %>% merge(per.capita, by = c("scenario", "value", "rcp"))
max.year$year <- as.factor(max.year$year)
plot_bottomleft <- max.year %>%
  ggplot(aes(x = as.numeric(year), fill = rcp))  + geom_density(alpha =
                                                                  0.4) + labs(title = "", x = "", y = "")  + colFillStudy  +
  theme(
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  ) + theme_bw()






detach(package:plyr)
max.value.test <-
  per.capita %>% group_by(scenario) %>% summarise(capita = max(capita))
plot_right <- max.value.test %>%
  ggplot(aes(x = capita))  + geom_density(fill = "blue", alpha = 0.4) + labs(title =
                                                                               "", x = "", y = "") + xlim(0, 200) + colFillStudy + coord_flip() +
  theme(
    plot.margin = margin(-12, 1, 1,-3),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  )
max.year <-
  max.value.test %>% merge(per.capita, by = c("scenario", "capita"))
max.year$year <- as.factor(max.year$year)
plot_top <- max.year %>%
  ggplot(aes(x = as.integer(year)))  + geom_density(fill = "blue", alpha =
                                                      0.4) + labs(title = "", x = "", y = "")   +
  theme(
    plot.margin = margin(1, -3, -12, 1),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 15)
  )


empty <- ggplot() + geom_point(aes(1, 1), color = "white") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(1, 1, -12, -3)
  )
library(egg)
p <-
  ggarrange(
    plot_top,
    empty,
    line,
    plot_right,
    ncol = 2,
    widths = c(1, 0.3),
    heights = c(0.7, 2)
  )
#ggsave(filename="percapita_groundwater.png",plot=p,width=6.0,height=6.0)


line <- ggplot(data = per.capita) +
  geom_line(aes(x = year, y = value, group = scenario), color = "blue") +
  ylim(0, 1700) +
  scale_x_discrete(labels = c(2010, rep("", 7), 2050, rep("", 9), 2100)) +
  labs(title = "",
       x = "Year",
       y = expression(atop(Global ~ groundwater ~ depletion, paste((
         km ^ 3 ~ year ^ -1
       ))))) +
  colScaleStudy +
  theme_bw() + theme(
    plot.margin = margin(-40,-10, 1, 1),
    axis.text.x = element_text(size = 14, face = "bold")
  )


max.value <-
  per.capita %>% group_by(ssp, rcp, scenario) %>% summarise(value = max(value))
plot_right <- max.value %>%
  ggplot(aes(x = value, fill = ssp))  + geom_density(alpha = 0.4) + labs(title =
                                                                           "", x = "", y = "") + colFillStudy + coord_flip() + xlim(0, 1700) +
  theme(
    legend.position = "right",
    legend.justification = "center",
    plot.margin = margin(-40, 1, 1,-10),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 10)
  )

max.year <-
  max.value %>% merge(per.capita, by = c("ssp", "rcp", "scenario", "value"))


plot_top <-
  max.year %>% ggplot(aes(x = as.integer(year), fill = ssp))  + geom_density(alpha =
                                                                               0.4) + labs(title = "", x = "", y = "") + colFillStudy  +
  theme(
    legend.position = "none",
    plot.margin = margin(-40, 1, 1,-10),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.text.x.bottom = element_blank(),
    legend.title = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
  ) + theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.text = element_text(size = 12)
  )

empty <- ggplot() + geom_point(aes(1, 1), color = "white") +
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.margin = margin(1, 1, -40, -10)
  )


p <-
  ggarrange(
    plot_top,
    empty,
    line,
    plot_right,
    ncol = 2,
    widths = c(1, 0.3),
    heights = c(0.7, 2)
  )
# ggsave(filename="ssp_groundwater.png",plot=p,width=6.0,height=6.0)



peak <-
  max.year %>% filter(year == 2040 |
                        year == 2045 |
                        year == 2050 | year == 2055 | year == 2060)
peak.and.decline <-
  per.capita %>% filter(year == 2100) %>% colnameReplace("value", "end.of.century.value") %>% merge(max.year, by =
                                                                                                      c("ssp", "rcp",  "model",  "wd",  "hilo",  "ex")) %>% mutate(peak = ((value - end.of.century.value) /
                                                                                                                                                                             value) * 100) %>% filter(peak > 0)
#setwd( "/Users/graham/Desktop/gcam-core" )
groundwater <-
  read.csv("groundwater_production_full_900.csv", sep = ",")
groundwater <-
  aggregate(value ~ year + scenario + groundwater, data = groundwater, sum) %>% filter(year >=
                                                                                         2010)
groundwater <-
  groundwater[!(groundwater$year == 2015 &
                  groundwater$scenario == "gl_md_ex_ssp4_hadg_4p5"),]
#setwd( "/Users/graham/Desktop/SSP_Climate" )

max.basin <-
  groundwater %>% group_by(groundwater, scenario) %>% summarise(value = max(value)) %>% merge(groundwater, by =
                                                                                                c("scenario", "value", "groundwater"))
peak.and.decline.basin <-
  groundwater %>% filter(year == 2100) %>% colnameReplace("value", "end.of.century.value") %>% merge(max.basin, by =
                                                                                                       c("scenario", "groundwater")) %>% mutate(peak = ((value - end.of.century.value) /
                                                                                                                                                          value) * 100) %>% filter(peak > 0)
test <-
  filter(peak.and.decline.basin, year.y >= 2040 & year.y <= 2060)
peak.and.decline.basin$count <- 1
peak.and.decline.basin.total <-
  aggregate(count ~ scenario, data = peak.and.decline.basin, sum)
peak.and.decline.basin.total.basin <-
  aggregate(count ~ groundwater, data = peak.and.decline.basin, sum)
peak.and.decline.basin.total.basin$groundwater <-
  gsub("*.water withdrawals",
       "",
       peak.and.decline.basin.total.basin$groundwater)
peak.and.decline.basin.total.basin <-
  colnameReplace(peak.and.decline.basin.total.basin,
                 "groundwater",
                 "basin.name")

#basin.id <- read.csv("/Users/graham/Desktop/gcam-core/input/gcam-data-system/water-data/mappings/basin_ID.csv",sep=",")

peak <-
  peak.and.decline.basin.total.basin %>% merge(basin.id, by = c("basin.name"), all =
                                                 T) %>%
  mutate(percent = (count / 900) * 100) %>% colnameReplace("basin.id", "id")


plot_GCAM(
  map.basin235,
  col = 'count',
  legend = T,
  proj = eck3,
  gcam_df = peak,
  gcam_key = 'basin.id',
  mapdata_key = "region_id"
) +
  ggplot2::scale_fill_gradientn(
    colors = c("snow1", "gold", "darkorange", "firebrick1"),
    na.value = c("gray97"),
    name = "(# of scenarios, n=900)",
    limits = c(0, 900),
    breaks = c(0, 300, 600, 900)
  ) +
  ggplot2::theme(legend.title =  element_text(
    colour = "black",
    size = 12,
    face = "plain"
  )) +
  ggplot2::guides(fill = guide_colourbar(title.position = "top"))
#ggsave(filename="basin_distribution_percent.png",width=10.0,height=6.0)

peak.and.decline.basin.total %>%
  ggplot(aes(x = count))  + geom_density(fill = "blue", alpha = 0.4) + labs(title =
                                                                              "", x = "", y = "") + xlim(0, 235) + theme_bw()
#ggsave(filename="basin_distribution.png",width=6.0,height=6.0)
peak.and.decline.basin.total.ssp <-
  separate(
    peak.and.decline.basin.total,
    scenario,
    c('wd', 'hilo', 'ex', 'ssp', 'model', 'climate'),
    sep = "_"
  ) %>% mutate(rcp = paste0("rcp", climate))

peak.and.decline.basin.total.ssp %>%
  ggplot(aes(x = count, fill = wd))  + geom_density(alpha = 0.4) + labs(title =
                                                                          "", x = "", y = "") + xlim(0, 235)  + theme_bw()


mean <- mean(peak.and.decline.basin.total$count)
std.dev <- sd(peak.and.decline.basin.total$count)


process_production_data <- function(gcm) {
  read.csv(paste0("ag_production_by_subsector_", gcm, ".csv")) %>%
    filter(sector != "Forest")  %>% filter(year >= 2010) %>%
    mutate(
      gcm = gcm,
      ssp = substr(scenario, 10, 13),
      rcp = substr(scenario, 20, 22)
    ) %>%
    dplyr::select(-Units, -output, -technology, -sector) #%>%
  #aggregate(value ~ year + scenario + subsector,sum)
}

getwd()
#setwd( "/Users/graham/Desktop/gcam-core" )
bind_rows(
  process_production_data("gfdl"),
  process_production_data("hadg"),
  2
  process_production_data("nrsm"),
  process_production_data("miro"),
  process_production_data("ipsl")
) -> ag.prod

ag.prod <-
  aggregate(value ~ year + scenario + subsector, data = ag.prod, sum)
ag.prod$subsector <-
  gsub("Root_Tuber", "RootTuber", ag.prod$subsector)
ag.prod$subsector <-
  gsub("biomass_grass", "biomass-grass", ag.prod$subsector)
ag.prod$subsector <-
  gsub("biomass_tree", "biomass-tree", ag.prod$subsector)
basin.prod <-
  separate(ag.prod, subsector, c('crop', 'basin.name'), sep = "_")
basin.prod <-
  aggregate(value ~ year + scenario  + crop + basin.name, data = basin.prod, sum)

#basin.id <- read.csv("/Users/graham/Desktop/gcam-core/input/gcam-data-system/water-data/mappings/basin_ID_swap.csv",sep=",")
basin.prod <- merge(basin.id, basin.prod, by = c("basin.name"))



#setwd( "/Users/graham/Desktop/gcam-core" )
groundwater <-
  read.csv("groundwater_production_full_900.csv", sep = ",")
groundwater <-
  aggregate(value ~ year + scenario + groundwater, data = groundwater, sum) %>% filter(year >=
                                                                                         2010)
groundwater <-
  groundwater[!(groundwater$year == 2015 &
                  groundwater$scenario == "gl_md_ex_ssp4_hadg_4p5"),]
#setwd( "/Users/graham/Desktop/SSP_Climate" )

max.basin <-
  groundwater %>% group_by(groundwater, scenario) %>% summarise(value = max(value)) %>% merge(groundwater, by =
                                                                                                c("scenario", "value", "groundwater"))
peak.and.decline.basin <-
  groundwater %>% filter(year == 2100) %>% colnameReplace("value", "end.of.century.value") %>% merge(max.basin, by =
                                                                                                       c("scenario", "groundwater")) %>% mutate(peak = ((value - end.of.century.value) /
                                                                                                                                                          value) * 100) %>% filter(peak > 0)
peak.and.decline.basin$groundwater <-
  gsub("*.water withdrawals", "", peak.and.decline.basin$groundwater)
peak.and.decline.basin <-
  colnameReplace(peak.and.decline.basin, "groundwater", "basin.name.old") %>% dplyr::select(scenario, basin.name.old)


test <-
  merge(pdf.basin.prod,
        peak.and.decline.basin,
        by = c("scenario", "basin.name.old"))

total.production <-
  aggregate(value ~ scenario + year + crop, data = basin.prod, sum)
total.production <-
  colnameReplace(total.production, "value", "total.production")

pdf.basin.prod <-
  merge(
    basin.prod,
    total.production,
    by = c("year", "crop", "scenario"),
    all = T
  )

test <-  aggregate(value ~ scenario + year + crop, data = test, sum)
pdf.basin.prod <-
  merge(test,
        total.production,
        by = c("year", "crop", "scenario"),
        all = T)
pdf.basin.prod$percent <-
  (pdf.basin.prod$value / pdf.basin.prod$total.production) * 100


pdf.basin.prod %>% filter(year == 2050) %>%
  ggplot(aes(x = percent))  + geom_density(fill = "blue", alpha = 0.4) + labs(title =
                                                                                "", x = "", y = "") + xlim(0, 100) + facet_wrap(~ crop, ncol = 5) + theme_bw()

pdf.basin.prod %>% filter(year == 2100) %>%
  ggplot(aes(x = percent))  + geom_density(fill = "blue", alpha = 0.4) + labs(title =
                                                                                "", x = "", y = "") + xlim(0, 100) + facet_wrap(~ crop, ncol = 5) + theme_bw()



basin.id <- colnameReplace(basin.id, "basin.id", "id")

pdf <- read.csv("pdf_production.csv", sep = ",")
pdf.crop <-
  pdf %>% filter(crop != "biomass-tree" & crop != "biomass-grass")
pdf.global <-
  aggregate(global.production ~ year + scenario, data = pdf.crop, sum)
pdf.basin <-
  aggregate(value ~ year + scenario, data = pdf.crop, sum)

pdf.crops <-
  merge(pdf.global, pdf.basin, by = c("scenario", "year"))
pdf.crops$percent <-
  (pdf.crops$value / pdf.crops$global.production) * 100
detach(package:plyr)
max <-
  pdf.crops %>% group_by(year) %>% summarise(max = max(percent))
min <-
  pdf.crops %>% group_by(year) %>% summarise(min = min(percent))
mean <-
  pdf.crops %>% group_by(year) %>% summarise(mean = mean(percent))
stdev <-
  pdf.crops %>% group_by(year) %>% summarise(stdev = sd(percent))
pdf.range <- merge(max, min, by = c("year"))


pdf.crops %>% filter(year == 2100) %>%
  ggplot(aes(x = percent))  + geom_density(fill = "blue", alpha = 0.4) + geom_point(data = (pdf.range %>% filter(year ==
                                                                                                                   2100)), aes(x = max, y = 0.09)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                                                                              2100)), aes(
                                                                                                                                                                                                x = max,
                                                                                                                                                                                                y = 0.085,
                                                                                                                                                                                                label = round(max, digits = 0)
                                                                                                                                                                                              )) +
  geom_point(data = (pdf.range %>% filter(year == 2100)), aes(x = min, y =
                                                                0.09)) + geom_text(data = (pdf.range %>% filter(year == 2100)), aes(
                                                                  x = min,
                                                                  y = 0.085,
                                                                  label = round(min, digits = 0)
                                                                )) +
  geom_line(data = (
    pdf.range %>% gather(range, value, max:min) %>% filter(year == 2100)
  ), aes(x = value, y = 0.09)) +
  geom_text(data = (mean %>% filter(year == 2100)), aes(
    x = 85,
    y = 0.085,
    label = paste0("Mean = ", round(mean, digits = 0))
  )) + geom_text(data = (stdev %>% filter(year == 2100)), aes(
    x = 85,
    y = 0.0825,
    label = paste0("S.D. = ", round(stdev, digits = 0))
  )) + labs(title = "", y = "Frequency", x = "Percentage of Total Global Crop Production") + xlim(0, 100) + theme_bw()
#ggsave(filename="pdf_ag_total_crop.png",width=10.0,height=8.0)


pdf.crop <-
  pdf %>% filter(crop == "biomass-tree" | crop == "biomass-grass")
pdf.global <-
  aggregate(global.production ~ year + scenario, data = pdf.crop, sum)
pdf.basin <-
  aggregate(value ~ year + scenario, data = pdf.crop, sum)

pdf.crops <-
  merge(pdf.global, pdf.basin, by = c("scenario", "year")) %>%
  mutate(percent = (value / global.production) * 100)
detach(package:plyr)
max <-
  pdf.crops %>% group_by(year) %>% summarise(max = max(percent))
min <-
  pdf.crops %>% group_by(year) %>% summarise(min = min(percent))
mean <-
  pdf.crops %>% group_by(year) %>% summarise(mean = mean(percent))
stdev <-
  pdf.crops %>% group_by(year) %>% summarise(stdev = sd(percent))
pdf.range <- merge(max, min, by = c("year"))

pdf.crops %>% filter(year == 2100) %>%
  ggplot(aes(x = percent))  + geom_density(fill = "blue", alpha = 0.4) + geom_point(data = (pdf.range %>% filter(year ==
                                                                                                                   2100)), aes(x = max, y = 0.09)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                                                                              2100)), aes(
                                                                                                                                                                                                x = max,
                                                                                                                                                                                                y = 0.085,
                                                                                                                                                                                                label = round(max, digits = 0)
                                                                                                                                                                                              )) +
  geom_point(data = (pdf.range %>% filter(year == 2100)), aes(x = min, y =
                                                                0.09)) + geom_text(data = (pdf.range %>% filter(year == 2100)), aes(
                                                                  x = min,
                                                                  y = 0.085,
                                                                  label = round(min, digits = 0)
                                                                )) +
  geom_line(data = (
    pdf.range %>% gather(range, value, max:min) %>% filter(year == 2100)
  ), aes(x = value, y = 0.09)) +
  geom_text(data = (mean %>% filter(year == 2100)), aes(
    x = 85,
    y = 0.085,
    label = paste0("Mean = ", round(mean, digits = 0))
  )) + geom_text(data = (stdev %>% filter(year == 2100)), aes(
    x = 85,
    y = 0.0825,
    label = paste0("S.D. = ", round(stdev, digits = 0))
  )) + labs(title = "", y = "Frequency", x = "Percentage of Total Global Biomass Production") + xlim(0, 100) + theme_bw()
#ggsave(filename="pdf_ag_total_biomass.png",width=10.0,height=8.0)



detach(package:plyr)
max <-
  pdf %>% group_by(crop, year) %>% summarise(max = max(percent))
min <-
  pdf %>% group_by(crop, year) %>% summarise(min = min(percent))
pdf.range <- merge(max, min, by = c("year", "crop"))

pdf %>% filter(year == 2050) %>%
  ggplot(aes(x = percent))  + geom_density(fill = "blue", alpha = 0.4) + geom_point(data = (pdf.range %>% filter(year ==
                                                                                                                   2050)), aes(x = max, y = 0.09)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                                                                              2050)), aes(
                                                                                                                                                                                                x = max,
                                                                                                                                                                                                y = 0.085,
                                                                                                                                                                                                label = round(max, digits = 0)
                                                                                                                                                                                              )) +
  geom_point(data = (pdf.range %>% filter(year == 2050)), aes(x = min, y =
                                                                0.09)) + geom_text(data = (pdf.range %>% filter(year == 2050)), aes(
                                                                  x = min,
                                                                  y = 0.085,
                                                                  label = round(min, digits = 0)
                                                                )) +
  geom_line(data = (
    pdf.range %>% gather(range, value, max:min) %>% filter(year == 2050)
  ), aes(x = value, y = 0.09)) + labs(title = "", x = "", y = "") + xlim(0, 100) + facet_wrap(~
                                                                                                crop, ncol = 5) + theme_bw()

pdf %>% filter(year == 2100) %>%
  ggplot(aes(x = percent))  + geom_density(fill = "blue", alpha = 0.4) + geom_point(data = (pdf.range %>% filter(year ==
                                                                                                                   2100)), aes(x = max, y = 0.09)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                                                                              2100)), aes(
                                                                                                                                                                                                x = max,
                                                                                                                                                                                                y = 0.085,
                                                                                                                                                                                                label = round(max, digits = 0)
                                                                                                                                                                                              )) +
  geom_point(data = (pdf.range %>% filter(year == 2100)), aes(x = min, y =
                                                                0.09)) + geom_text(data = (pdf.range %>% filter(year == 2100)), aes(
                                                                  x = min,
                                                                  y = 0.085,
                                                                  label = round(min, digits = 0)
                                                                )) +
  geom_line(data = (
    pdf.range %>% gather(range, value, max:min) %>% filter(year == 2100)
  ), aes(x = value, y = 0.09)) + labs(title = "", y = "Frequency", x = "Percentage of Total Global Production") + xlim(0, 100) + facet_wrap(~
                                                                                                                                              crop, ncol = 5) + theme_bw()
#ggsave(filename="pdf_ag.png",width=10.0,height=8.0)

pdf %>% filter(year == 2100) %>%
  ggplot(aes(x = round(percent, digit = 0)))  + geom_bar(fill = "blue") +
  geom_point(data = (pdf.range %>% filter(year == 2100)), aes(x = max, y =
                                                                90)) + geom_text(data = (pdf.range %>% filter(year == 2100)), aes(
                                                                  x = max,
                                                                  y = 85,
                                                                  label = round(max, digits = 0)
                                                                )) +
  geom_point(data = (pdf.range %>% filter(year == 2100)), aes(x = min, y =
                                                                90)) + geom_text(data = (pdf.range %>% filter(year == 2100)), aes(
                                                                  x = min,
                                                                  y = 85,
                                                                  label = round(min, digits = 0)
                                                                )) +
  geom_line(data = (
    pdf.range %>% gather(range, value, max:min) %>% filter(year == 2100)
  ), aes(x = value, y = 90)) +
  labs(title = "", y = "Count", x = "Percentage of Total Global Production") + xlim(0, 100) + ylim(0, 100)  + facet_wrap(~
                                                                                                                           crop, ncol = 5) + theme_bw()
#ggsave(filename="pdf_ag_bar.png",width=10.0,height=8.0)




ggsave(filename = "pdf_ag_water_over_time.png",
       width = 10.0,
       height = 8.0)

pdf.crops %>% filter(year == 2100 & water_f != "Total") %>%
  ggplot()  + geom_density(aes(x = percent, fill = water_f), alpha = 0.9) + geom_point(data = (pdf.range %>% filter(year ==
                                                                                                                      2100 &
                                                                                                                      water_f == "WGHM")), aes(x = max, y = 0.16)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                                                                                              2100 &
                                                                                                                                                                                                              water_f == "WGHM")), aes(
                                                                                                                                                                                                                x = max,
                                                                                                                                                                                                                y = 0.155,
                                                                                                                                                                                                                label = paste0(round(max, digits = 0), "%")
                                                                                                                                                                                                              )) +
  geom_point(data = (pdf.range %>% filter(year == 2100 &
                                            water_f == "WGHM")), aes(x = min, y = 0.16)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                    2100 &
                                                                                                                                    water_f == "WGHM")), aes(
                                                                                                                                      x = min,
                                                                                                                                      y = 0.155,
                                                                                                                                      label = paste0(round(min, digits = 0), "%")
                                                                                                                                    )) +
  geom_line(data = (
    pdf.range %>% gather(range, value, max:min) %>% filter(year == 2100 &
                                                             water_f == "WGHM")
  ), aes(x = value, y = 0.16)) +
  geom_point(data = (pdf.range %>% filter(year == 2100 &
                                            water_f == "PCR-GLOBWB")), aes(x = max, y = 0.135)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                           2100 &
                                                                                                                                           water_f == "PCR-GLOBWB")), aes(
                                                                                                                                             x = max,
                                                                                                                                             y = 0.13,
                                                                                                                                             label = paste0(round(max, digits = 0), "%")
                                                                                                                                           )) +
  geom_point(data = (pdf.range %>% filter(year == 2100 &
                                            water_f == "PCR-GLOBWB")), aes(x = min, y = 0.135)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                           2100 &
                                                                                                                                           water_f == "PCR-GLOBWB")), aes(
                                                                                                                                             x = min,
                                                                                                                                             y = 0.13,
                                                                                                                                             label = paste0(round(min, digits = 0), "%")
                                                                                                                                           )) +
  geom_line(data = (
    pdf.range %>% gather(range, value, max:min) %>% filter(year == 2100 &
                                                             water_f == "PCR-GLOBWB")
  ), aes(x = value, y = 0.135)) +
  geom_point(data = (pdf.range %>% filter(year == 2100 &
                                            water_f == "Total")), aes(x = max, y = 0.185)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                      2100 &
                                                                                                                                      water_f == "Total")), aes(
                                                                                                                                        x = max,
                                                                                                                                        y = 0.18,
                                                                                                                                        label = paste0(round(max, digits = 0), "%")
                                                                                                                                      )) +
  geom_point(data = (pdf.range %>% filter(year == 2100 &
                                            water_f == "Total")), aes(x = min, y = 0.185)) + geom_text(data = (pdf.range %>% filter(year ==
                                                                                                                                      2100 &
                                                                                                                                      water_f == "Total")), aes(
                                                                                                                                        x = min,
                                                                                                                                        y = 0.18,
                                                                                                                                        label = paste0(round(min, digits = 0), "%")
                                                                                                                                      )) +
  geom_line(data = (
    pdf.range %>% gather(range, value, max:min) %>% filter(year == 2100 &
                                                             water_f == "Total")
  ), aes(x = value, y = 0.185)) +
  geom_text(data = (mean %>% filter(year == 2100 &
                                      water_f == "PCR-GLOBWB")), aes(
                                        x = 44,
                                        y = 0.145,
                                        label = paste0("Mean = ", round(mean, digits = 0), "%")
                                      )) + geom_text(data = (stdev %>% filter(year == 2100 &
                                                                                water_f == "PCR-GLOBWB")), aes(
                                                                                  x = 44,
                                                                                  y = 0.14,
                                                                                  label = paste0("S.D. = ", round(stdev, digits = 0), "%")
                                                                                )) +
  geom_text(data = (mean %>% filter(year == 2100 &
                                      water_f == "WGHM")), aes(
                                        x = 26.5,
                                        y = 0.17,
                                        label = paste0("Mean = ", round(mean, digits = 0), "%")
                                      )) + geom_text(data = (stdev %>% filter(year == 2100 &
                                                                                water_f == "WGHM")), aes(
                                                                                  x = 26.5,
                                                                                  y = 0.165,
                                                                                  label = paste0("S.D. = ", round(stdev, digits = 0), "%")
                                                                                )) +
  geom_text(data = (mean %>% filter(year == 2100 &
                                      water_f == "Total")), aes(
                                        x = 39.5,
                                        y = 0.195,
                                        label = paste0("Mean = ", round(mean, digits = 0), "%")
                                      )) + geom_text(data = (stdev %>% filter(year == 2100 &
                                                                                water_f == "Total")), aes(
                                                                                  x = 39.5,
                                                                                  y = 0.19,
                                                                                  label = paste0("S.D. = ", round(stdev, digits = 0), "%")
                                                                                )) +
  labs(title = "", y = "Frequency", x = "Percentage of Total Global Crop Production") + xlim(0, 100) + theme_bw() +
  theme(
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = c(0.84, 0.5),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  guides(point = guide_legend(override.aes = list(linetype = 0)),
         line = guide_legend(override.aes = list(linetype = 0)))
#ggsave(filename="pdf_ag_total_water.png",width=10.0,height=8.0)


pdf <- read.csv("pdf_production.csv", sep = ",")
pdf.crop <-
  pdf %>% filter(crop != "biomass-tree" & crop != "biomass-grass")
pdf.global <-
  aggregate(global.production ~ year + scenario, data = pdf.crop, sum)
pdf.basin <-
  aggregate(value ~ year + scenario, data = pdf.crop, sum)

pdf.crops <-
  merge(pdf.global, pdf.basin, by = c("scenario", "year")) %>%
  mutate(percent = (value / global.production) * 100)
pdf.crops <- pdf.crops %>% mutate(water = substr(scenario, 1, 2))
pdf.crops$water <- gsub("gl", "WGHM", pdf.crops$water)
pdf.crops$water <- gsub("wg", "PCR-GLOBWB", pdf.crops$water)
total.crop <-
  pdf %>% filter(crop != "biomass-tree" & crop != "biomass-grass")
total.global <-
  aggregate(global.production ~ year + scenario, data = total.crop, sum)
total.basin <-
  aggregate(value ~ year + scenario, data = total.crop, sum)

total.crops <-
  merge(total.global, total.basin, by = c("scenario", "year")) %>%
  mutate(percent = (value / global.production) * 100)
total.crops$water <- "Total"
pdf.crops <- rbind(pdf.crops, total.crops)
pdf.crops$water_f <-
  factor(pdf.crops$water, levels = c("WGHM", "PCR-GLOBWB", "Total"))
detach(package:plyr)
max <-
  pdf.crops %>% group_by(year, water_f) %>% summarise(max = max(percent))
min <-
  pdf.crops %>% group_by(year, water_f) %>% summarise(min = min(percent))
mean <-
  pdf.crops %>% group_by(year, water) %>% summarise(mean = mean(percent))
stdev <-
  pdf.crops %>% group_by(year, water_f) %>% summarise(stdev = sd(percent))
pdf.range <- merge(max, min, by = c("year", "water_f"))
mean$water <-
  factor(mean$water, levels = c("WGHM", "PCR-GLOBWB", "Total"))

pdf.crops$year <- as.factor(pdf.crops$year)
mean$year <- as.numeric(mean$year)
pdf.crops %>% filter(water_f != "Total") %>%
  ggplot() + geom_density_ridges(
    aes(
      x = percent,
      y = year,
      group = interaction(year, water_f),
      fill = water_f
    ),
    scale = 3,
    alpha = 0.85
  ) + geom_path(data = mean,
                aes(
                  x = mean,
                  y = year,
                  color = water,
                  linetype = water
                ),
                size = 0.75) + scale_color_manual(values = c("black", "black", "black")) + scale_fill_manual(values =
                                                                                                               c("indianred1", "turquoise3", "black")) + scale_x_continuous(expand = c(0, 0)) + scale_y_discrete(expand =
                                                                                                                                                                                                                   c(0, 1)) + coord_flip() + guides(linetype = FALSE, color = FALSE) + theme_bw() +
  labs(x = "Percentage of Total Crop Production", y = "") + theme(
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 12)
  )
#ggsave(filename="pdf_ag_water_over_time.png",width=10.0,height=8.0)


pdf <- read.csv("pdf_production.csv", sep = ",")
pdf.crop <-
  pdf %>% filter(crop != "biomass-tree" & crop != "biomass-grass")
pdf.global <-
  aggregate(global.production ~ year + scenario, data = pdf.crop, sum)
pdf.basin <-
  aggregate(value ~ year + scenario, data = pdf.crop, sum)

pdf.crops <-
  merge(pdf.global, pdf.basin, by = c("scenario", "year")) %>%
  mutate(percent = (value / global.production) * 100)
pdf.crops <- pdf.crops %>% mutate(water = substr(scenario, 1, 2))
pdf.crops$water <- gsub("gl", "WGHM", pdf.crops$water)
pdf.crops$water <- gsub("wg", "PCR-GLOBWB", pdf.crops$water)
total.crop <-
  pdf %>% filter(crop != "biomass-tree" & crop != "biomass-grass")
total.global <-
  aggregate(global.production ~ year + scenario, data = total.crop, sum)
total.basin <-
  aggregate(value ~ year + scenario, data = total.crop, sum)

total.crops <-
  merge(total.global, total.basin, by = c("scenario", "year")) %>%
  mutate(percent = (value / global.production) * 100)
total.crops$water <- "Total"
pdf.crops <- rbind(pdf.crops, total.crops)
pdf.crops$water_f <-
  factor(pdf.crops$water, levels = c("WGHM", "PCR-GLOBWB", "Total"))

pdf.crops %>% group_by(water, year) %>% summarise(mean = mean(percent), std =
                                                    sd(percent)) -> test
test$water <-
  factor(test$water, levels = c("WGHM", "PCR-GLOBWB", "Total"))
test %>% dplyr::filter(water != "Total") %>%
  ggplot() +
  geom_ribbon(aes(
    ymin = (mean - std),
    ymax = (mean + std),
    x = as.numeric(year),
    group = water,
    fill = water
  ),
  alpha = 0.15) +
  geom_line(
    aes(
      x = as.numeric(year),
      y = (mean - std),
      color = water,
      group = water
    ),
    size = 0.25,
    alpha = 0.50
  ) +
  geom_line(aes(
    x = as.numeric(year),
    y = mean,
    color = water,
    group = water
  ), size = 0.75) +
  geom_line(
    aes(
      x = as.numeric(year),
      y = (mean + std),
      color = water,
      group = water
    ),
    size = 0.25,
    alpha = 0.50
  ) +
  scale_x_continuous(breaks = seq(2010, 2100, 10), expand = c(0, 0)) +
  labs(title = "", x = "", y = "Percentage of Total Crop Production") +
  theme_bw() + theme(
    plot.margin = margin(0, 1, 0, 0.25, "cm"),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.title = element_text(size = 15),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.75, "cm"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )
# ggsave(filename="ribbon_ag_water_over_time.png",width=10.0,height=8.0)











#setwd( "/Users/grah436/Desktop/groundwater_peak_and_decline" )

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
detach(package:plyr)
groundwater <-
  read.csv("groundwater_production_full_900.csv", sep = ",") %>% group_by(scenario, year, groundwater) %>%
  summarise(value = sum(value)) %>% ungroup() %>% filter(year >= 2010) %>% separate(scenario, c('wd', 'hilo', 'ex', 'ssp', 'model', 'climate'), sep =
                                                                                      "_") %>% mutate(rcp = paste0("rcp", climate)) %>%
  mutate(
    wd = gsub("gl", "WGHM", wd),
    wd = gsub("wg", "PCR-GLOBWB", wd),
    hilo = gsub("lo", "5%", hilo),
    hilo = gsub("md", "25%", hilo),
    hilo = gsub("hi", "40%", hilo),
    ex = gsub("ex", "Expansion", ex),
    ex = gsub("rs", "Restricted", ex),
    hilo_f = factor(hilo, levels = c("5%", "25%", "40%")),
    ssp = gsub("ssp", "SSP", ssp),
    rcp = gsub("rcp", "RCP", rcp),
    rcp = gsub("p", ".", rcp),
    model = gsub("gfdl", "GFDL", model),
    model = gsub("hadg", "HadGEM", model),
    model = gsub("ipsl", "IPSL", model),
    model = gsub("miro", "MIROC", model),
    model = gsub("nrsm", "NorESM", model)
  )

groundwater %>% group_by(wd, hilo_f, ex, ssp, model, rcp, groundwater) %>% mutate(cumsum =
                                                                                    cumsum(value)) %>% dplyr::filter(year == 2100) -> cumsum
groundwater %>% dplyr::filter(year == 2100) -> groundwater
groundwater %>% group_by(wd, hilo_f, ex, ssp, rcp, model) %>% mutate(max =
                                                                       max(value)) %>% dplyr::filter(max == value) %>% ungroup() -> max.year


read.csv("groundwater_production_full_900.csv", sep = ",") %>% dplyr::filter(value >
                                                                               0) %>% group_by(scenario, groundwater) %>% mutate(max_value = max(value)) %>% ungroup() %>%
  mutate(peak = if_else((max_value == value) &
                          (year < 2100), 1, 0)) %>% dplyr::filter(peak == 1) %>%
  separate(scenario, c('wd', 'hilo', 'ex', 'ssp', 'model', 'climate'), sep =
             "_") %>% mutate(rcp = paste0("rcp", climate)) %>%
  mutate(
    wd = gsub("gl", "WGHM", wd),
    wd = gsub("wg", "PCR-GLOBWB", wd),
    hilo = gsub("lo", "5%", hilo),
    hilo = gsub("md", "25%", hilo),
    hilo = gsub("hi", "40%", hilo),
    ex = gsub("ex", "Expansion", ex),
    ex = gsub("rs", "Restricted", ex),
    hilo_f = factor(hilo, levels = c("5%", "25%", "40%")),
    ssp = gsub("ssp", "SSP", ssp),
    rcp = gsub("rcp", "RCP", rcp),
    rcp = gsub("p", ".", rcp),
    model = gsub("gfdl", "GFDL", model),
    model = gsub("hadg", "HadGEM", model),
    model = gsub("ipsl", "IPSL", model),
    model = gsub("miro", "MIROC", model),
    model = gsub("nrsm", "NorESM", model),
    groundwater = gsub("-water withdrawals", "", groundwater),
    groundwater = gsub("-", "_", groundwater),
    groundwater = gsub(" ", "_", groundwater)
  ) -> groundwater
