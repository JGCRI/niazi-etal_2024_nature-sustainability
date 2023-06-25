# split density plots

# rm(list = ls(all=TRUE))

library(tidyverse)
source("multiplot function.R")

# read data and get peak year
read_csv("groundwater_production_FINAL.csv") -> all_data 

all_data %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year != 1975) %>% 
  group_by(scenario, year) %>% summarise(value = sum(value)) %>%
  summarise(max_yr = .$year[which.max(value)]) %>% 
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) ->
  gw_max_yr

all_data %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year != 1975) %>% 
  group_by(scenario, year) %>% summarise(value = sum(value)) %>%
  summarise(max = max(value)) %>% 
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) ->
  gw_max


all_data %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year != 1975) %>% 
  group_by(scenario, year) %>% summarise(value = sum(value)) %>%
  summarise(max = max(value),
            max_yr = .$year[which.max(value)]) %>% 
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) ->
  gw_peaks



# PDF plot: peak year
plot_gw_density <- function(peak, xtitle, var, panel_title){
  gw_peaks %>% 
    ggplot(aes_string(peak, fill = var)) +
    geom_density(alpha = 0.5, adjust = 2) +
    theme_bw() + 
    #labs(x = NULL, y = NULL) +
    labs(y = "Probability density",
         x = xtitle) +
    #ylim(c(0, 0.08)) +
    theme(#axis.text.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.size = unit(0.4, "cm"),
          legend.position = c(0.8, 0.6),
          #legend.title = element_blank(),
          legend.background = element_blank()) +
    #scale_x_continuous(breaks = seq(2010,2100,10), limits = c(2010, 2100)) +
    labs(title= panel_title)
}


# plot densities of peak year 
multiplot(
  plot_gw_density("max_yr", "Peak Year" , "calib", "(a)"),
  plot_gw_density("max_yr", "Peak Year" , "gw", "(b)"),
  plot_gw_density("max_yr", "Peak Year" , "surf", "(c)"),
  plot_gw_density("max_yr", "Peak Year" , "ssp", "(d)"),
  plot_gw_density("max_yr", "Peak Year" , "gcm", "(e)"),
  plot_gw_density("max_yr", "Peak Year" , "rcp", "(f)"),
  cols = 2
)


# plot densities of peak water 
multiplot(
  plot_gw_density("max", "Peak Withdrawal (km3/yr)" , "calib", "(a)"),
  plot_gw_density("max", "Peak Withdrawal (km3/yr)" , "gw", "(b)"),
  plot_gw_density("max", "Peak Withdrawal (km3/yr)" , "surf", "(c)"),
  plot_gw_density("max", "Peak Withdrawal (km3/yr)" , "ssp", "(d)"),
  plot_gw_density("max", "Peak Withdrawal (km3/yr)" , "gcm", "(e)"),
  plot_gw_density("max", "Peak Withdrawal (km3/yr)" , "rcp", "(f)"),
  cols = 2
)


# CDF plot for peak year
plot_gwy_cdf <- function(var, panel_title){
  gw_max_yr %>% 
    filter(max_yr >= 2010) %>% 
    mutate(gw = factor(gw, levels = c("lo", "md","hi"))) %>% 
    ggplot(aes_string("max_yr", color = var)) +
    stat_ecdf(geom = "smooth", pad = F) +
    stat_ecdf(data = gw_max_yr, aes(max_yr), color = "black", lwd = 1.5, geom = "smooth", pad = F) +
    theme_bw() + 
    labs(y = "Cumulative probability",
         x = "Peak year") +
    scale_x_continuous(breaks = seq(2020,2100,20), limits = c(2010, 2100)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.size = unit(0.4, "cm"),
          legend.position = c(0.85, 0.4)) +
    geom_hline(yintercept = 1.0, lty = 2) +
    labs(title = panel_title) #labs(title = panel_title)
}

multiplot(
  plot_gwy_cdf("calib", "(a)"),
  plot_gwy_cdf("gw", "(b)"),
  plot_gwy_cdf("surf", "(c)"),
  plot_gwy_cdf("ssp", "(d)"),
  plot_gwy_cdf("gcm", "(e)"),
  plot_gwy_cdf("rcp", "(f)"),
  cols = 2
)


# CDF plot for peak water
plot_gw_cdf <- function(var, panel_title){
  gw_max %>% 
    #filter(max >= 2010) %>% 
    mutate(gw = factor(gw, levels = c("lo", "md","hi"))) %>% 
    ggplot(aes_string("max", color = var)) +
    stat_ecdf(geom = "smooth", pad = F) +
    stat_ecdf(data = gw_max, aes(max), color = "black", lwd = 1.5, geom = "smooth", pad = F) +
    theme_bw() + 
    labs(y = "Cumulative probability",
         x = "Peak withdrawal (km3/yr)") +
    #scale_x_continuous(breaks = seq(2020,2100,20), limits = c(2010, 2100)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.key.size = unit(0.4, "cm"),
          legend.position = c(0.85, 0.4)) +
    geom_hline(yintercept = 1.0, lty = 2) +
    labs(title = panel_title) #labs(title = panel_title)
}

multiplot(
  plot_gw_cdf("calib", "(a)"),
  plot_gw_cdf("gw", "(b)"),
  plot_gw_cdf("surf", "(c)"),
  plot_gw_cdf("ssp", "(d)"),
  plot_gw_cdf("gcm", "(e)"),
  plot_gw_cdf("rcp", "(f)"),
  cols = 2
)






