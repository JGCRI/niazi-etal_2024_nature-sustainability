## Initial results - check Gleeson vs watergap behavior!

# plots 3 different kinds of sensitivity-type plots. 

library(tidyverse)

read_csv("groundwater_production_FINAL.csv") %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year != 1975) %>% 
  group_by(year, scenario) %>% summarise(value = sum(value)) %>% ungroup() %>% 
  #tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>% 
  #select(year, gw, gcm, value) %>% 
  #filter(scenario == "wg_md_rs_ssp3_gfdl_6p0") %>% 
  ggplot(aes(year, value, color = scenario)) + geom_line(alpha = 0.5) + 
  theme(legend.position = "none") +
  #facet_grid(~gw) +
  labs(y = "Global groundwater depletion km^3/year", x = NULL) + ylim(c(0, 1600))


read_csv("groundwater_production_wg_md_rs_ssp3_gfdl_6p0.csv") %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year != 1975) %>% 
  group_by(year, scenario) %>% summarise(value = sum(value)) %>% ungroup() %>% 
  #tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>% 
  #select(year, gw, gcm, value) %>% 
  ggplot(aes(year, value, color = scenario)) + geom_line(alpha = 0.5) + 
  theme(legend.position = "none") +
  #facet_grid(~gw) +
  labs(y = "depletion km^3/year", x = NULL) + ylim(c(0, 1600))


read_csv("groundwater_production_FINAL.csv") %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year != 1975) %>% 
  group_by(scenario, year) %>% summarise(value = sum(value)) %>% #aggregates across all basins to calculate global withdrawal 
  summarise(max = max(value),
            max_yr = .$year[which.max(value)],
            taken = sum(value)) %>% 
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) ->
  gw_peaks



# jitter of peaks per category 
gw_peak_plot <- function(color_variable){
  gw_peaks %>% 
    ggplot(aes_string("max_yr", "max", color = color_variable)) + #, shape = color_variable
    #geom_point(size = 2, alpha = 0.7) +
    geom_jitter(size = 2, alpha = 0.7) +
    theme_minimal() +
    labs(y = "Peak withdrawal (km3)",
         x = "Peak year",
         title = paste0("Influence of ", color_variable)) +
    ylim(c(0,1600)) + xlim(c(2000, 2105))
}

gw_peak_plot("calib")
gw_peak_plot("ssp")
gw_peak_plot("gcm")
gw_peak_plot("rcp")
gw_peak_plot("gw")
gw_peak_plot("surf")


#
gw_peak_density <- function(color_variable){
  gw_peaks %>% 
    ggplot(aes_string("max_yr", "max", fill = color_variable)) +
    #geom_point(size = 2, alpha = 0.7) +
    stat_density2d(aes_string(fill = color_variable, alpha = 0.01),
                   geom = "polygon", n = 20, show.legend = F,
                   contour = F) +
    #scale_fill_gradient2(low = "red", high = "red") +
    geom_jitter(pch = 21, color = "black") +
    theme_minimal() +
    labs(y = "Peak withdrawal (km3)",
         x = "Peak year") +
    ylim(c(0,1600)) + xlim(c(2000, 2105))
}

gw_peak_density("calib")
gw_peak_density("ssp")
gw_peak_density("gcm")
gw_peak_density("rcp")
gw_peak_density("gw")
gw_peak_density("surf")

gw_peak_density_focus <- function(color_variable, focus){
  gw_peaks %>% 
    ggplot(aes_string("max_yr", "max", color = color_variable,
                      size = "taken")) +
    #geom_point(size = 2, alpha = 0.7) +
    stat_density2d(data = filter(gw_peaks, ssp == focus),
                   aes(fill = ..level..),
                   geom = "polygon", n = 100, show.legend = F) +
    geom_jitter(alpha = 0.5) +
    scale_fill_gradient(low = "white", high = "grey") +
    theme_minimal() +
    labs(y = "Peak withdrawal (km3)",
         x = "Peak year",
         title = focus) +
    ylim(c(200,1600)) + xlim(c(2000, 2105))
}

gw_peak_density_focus("calib","gl")
gw_peak_density_focus("ssp", "ssp1")
gw_peak_density_focus("gcm","miro")
gw_peak_density_focus("rcp", "8p5")
gw_peak_density_focus("gw","hi")
gw_peak_density_focus("surf","ex")




