# main density plot with gw depletion trajectories

# rm(list = ls(all=TRUE))

library(tidyverse)
library(egg)
library(stringr)

source("multiplot function.R")

# read gcam output and aggregate to global groundwater depletion
read_csv("groundwater_production_FINAL.csv") -> all_data

all_data %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year >= 2010) %>% 
  group_by(year, scenario) %>%
  summarise(value = sum(value)) %>% ungroup() %>% 
  mutate(value = if_else(value == 0, NA_real_, value)) ->
  gw_dep_2010_2100


# get data required for density plots of peak and peak timing
gw_dep_2010_2100 %>% 
  filter(grepl("_rs_", scenario)) %>% 
  group_by(scenario, year) %>% summarise(value = sum(value)) %>%
  summarise(max = max(value),
            max_yr = .$year[which.max(value)]) ->
  gw_peaks


# plot time series and overlay density plots
gw_dep_2010_2100 %>% 
  #filter(grepl("_rs_", scenario)) %>% 
  ggplot(aes(year, value, group = scenario)) +
  geom_line(alpha = 0.2, color = "blue") + 
  labs(y = NULL, x = NULL) +
  scale_x_continuous(breaks = seq(2010,2100,10), limits = c(2010, 2100)) +
  scale_y_continuous(breaks = seq(0, 1600, 200), limits = c(0, 1600)) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        #plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")
        ) -> gw_peak_traj


gw_dep_2010_2100 %>% #filter(grepl("_rs_", scenario)) %>% 
  ggplot(aes(year, value)) +
  geom_hex(bins = 50)
  

# plot density for x axis (peak year)
gw_peaks %>% 
  ggplot(aes(max_yr)) +
  geom_density(fill = "blue", alpha = 0.5) +
  scale_x_continuous(breaks = seq(2010,2100,10), limits = c(2010, 2100)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #plot.margin = margin(150, 5.5, 5.5, 5.5, "pt"),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()) -> gw_peak_den_yr

# plot density for y axis (peak mag)
gw_peaks %>% 
  ggplot(aes(max)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_bw() + coord_flip() +
  scale_x_continuous(breaks = seq(0, 1600, 200), limits = c(0, 1600)) +
  labs(y = NULL, x = NULL) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #plot.margin = margin(5.5, 150, 5.5, 5.5, "pt"),
        axis.ticks.x = element_blank(),
        panel.border = element_blank()) -> gw_peak_den_mag

empty <- ggplot() + geom_point(aes(1, 1), color = "white") + theme_void()

# full
#multiplot(gw_peak_den_yr, gw_peak_traj, NA, gw_peak_den_mag, cols = 2)

ggarrange(gw_peak_den_yr, empty, gw_peak_traj,  gw_peak_den_mag, ncol=2, widths = c(1,0.2), heights = c(0.7, 2)) 



# highlight trajectories by categories on top of all trajectories   

c(notpndscenarios$scenario)

gw_dep_2010_2100 %>% 
  #filter(grepl("_rs_", scenario)) %>% 
  ggplot(aes(year, value, group = scenario)) +
  geom_line(alpha = 0.4, color = "grey",  lwd = .75) + 
  geom_line(data = filter(gw_peak_traj$data, grepl("wg_hi_ex_ssp3_nrsm_4p5", scenario)), alpha = 0.4, color = "red",  lwd = .5) + # change the variable here
  labs(y = expression(paste("Groundwater Withdrawal (", km ^ 3, yr ^  -1 , ")")), x =  "Year") +
  scale_x_continuous(breaks = seq(2010,2100,10), limits = c(2010, 2100)) +
  scale_y_continuous(breaks = seq(0, 1600, 200), limits = c(0, 1600)) +
  theme_classic() +
  theme(#axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt")
  ) #-> gw_peak_traj_select


# highlight scenarios not showing peak and decline 

