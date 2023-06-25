# peak bubble plot

# rm(list = ls(all=TRUE))

library(tidyverse)


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
  mutate(ssp_rcp = paste0(ssp, "_", rcp)) %>% 
  select(ssp_rcp, ssp, rcp, max, max_yr) ->
  gw_peaks

read_csv("groundwater_production_FINAL.csv") %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year != 1975) %>% 
  group_by(scenario, year) %>% summarise(value = sum(value)) %>%
  summarise(max = max(value),
            max_yr = .$year[which.max(value)],
            taken = sum(value)) %>% 
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>% 
  mutate(ssp_rcp = paste0(ssp, "_", rcp)) %>% 
  select(ssp_rcp, ssp, rcp, max, max_yr, calib, surf, gw) ->
  gw_peaks_detailed


gw_peaks %>% 
  complete(ssp_rcp = combos) %>%
  ggplot(aes(max_yr, max)) +
  ylim(c(0,1600)) +
  labs(y = "Peak withdrawal (km3/yr)",
       x = NULL) -> g

# option 1: facet grid to separate 
g +
  stat_ellipse(geom = "polygon", level = 0.90) +
  facet_wrap(~ssp_rcp, nrow = 5) +
  geom_point(size = 2, alpha = 0.5, color = "red")

# option 2: combined
g +
  stat_ellipse(aes(fill = ssp_rcp),
               geom = "polygon",
               level = 0.90, alpha = 0.5, color = "black")

# option 3: separate by rcp
g +
  stat_ellipse(aes(fill = ssp),
               geom = "polygon",
               level = 0.90, alpha = 0.5, color = "black") +
  facet_wrap(~rcp)


# option 3: separate by ssp
g +
  stat_ellipse(aes(fill = rcp),
               geom = "polygon",
               level = 0.90, alpha = 0.5, color = "black") +
  facet_wrap(~ssp, nrow = 1)


# option 4: kernal density
gw_peaks %>% 
  complete(ssp_rcp = combos) %>% 
  ggplot(aes(max_yr, max)) +
  stat_density2d(aes(fill = ..level..),
                 geom = "polygon",
                 n = 100) +
  geom_point(size = 2, alpha = 0.2, color = "red") +
  facet_wrap(~ssp_rcp, nrow = 5)

# option 4a: kernal density + jitter 
gw_peaks %>% 
  complete(ssp_rcp = combos) %>% 
  ggplot(aes(max_yr, max)) +
  stat_density2d(aes(fill = ..level..),
                 geom = "polygon",
                 n = 100) +
  geom_jitter(size = 2, alpha = 0.2, color = "red") +
  facet_wrap(~ssp_rcp, nrow = 5) 
  


# option 5: raster density
g +
  stat_density2d(aes(fill = ..density..),
                 geom = "raster",
                 contour = FALSE,
                 n = 100) +
  facet_wrap(~ssp_rcp, nrow = 5)

# option 6: jitter

gw_peaks_detailed %>%
  mutate(gw = factor(gw, levels = c("lo", "md", "hi"))) %>% 
  #complete(ssp_rcp = combos) %>% 
  #gather(metric, value, -calib, -ssp, -rcp, -surf, -gw) %>% 
  ggplot(aes(rcp, max, shape = calib)) +
  geom_jitter(aes(fill = gw), alpha = 0.7, width = 0.3) +
  facet_wrap(~ssp, nrow = 1, strip.position = "bottom") +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values =  RColorBrewer::brewer.pal(3, "Spectral")) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text = element_blank(),
        #strip.text.y = element_blank(),
        strip.background = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Global Groundwater Depletion (BCM)",
       x = NULL,
       shape = "Calibration data",
       fill = "Groundwater availability scenario")


gw_peaks_detailed %>%
  mutate(gw = factor(gw, levels = c("lo", "md", "hi"))) %>% 
  #complete(ssp_rcp = combos) %>% 
  #gather(metric, value, -calib, -ssp, -rcp, -surf, -gw) %>% 
  ggplot(aes(rcp, max_yr, shape = calib)) +
  geom_jitter(aes(fill = gw), alpha = 0.7, width = 0.3) +
  facet_wrap(~ssp, nrow = 1, strip.position = "bottom") +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values =  RColorBrewer::brewer.pal(3, "Spectral")) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text = element_blank(),
        #strip.text.y = element_blank(),
        strip.background = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Year of peak depletion",
       x = NULL,
       shape = "Calibration data",
       fill = "Groundwater availability scenario")



gw_peaks_detailed %>%
  mutate(gw = factor(gw, levels = c("lo", "md", "hi"))) %>% 
  #complete(ssp_rcp = combos) %>% 
  #gather(metric, value, -calib, -ssp, -rcp, -surf, -gw) %>% 
  ggplot(aes(rcp, max)) +
  geom_violin(adjust = 0.5, trim = F) +
  facet_wrap(~ssp, nrow = 1, strip.position = "bottom") +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  scale_shape_manual(values = c(21, 24)) +
  scale_fill_manual(values =  RColorBrewer::brewer.pal(3, "Spectral")) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        #axis.text = element_blank(),
        #strip.text.y = element_blank(),
        strip.background = element_blank(),
        panel.background = element_blank()) +
  labs(y = "Global Groundwater Depletion (BCM)",
       x = NULL,
       shape = "Calibration data",
       fill = "Groundwater availability scenario")

