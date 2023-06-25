# HISTOGRAMS

# rm(list = ls(all=TRUE))

# do more analysis on population and gdp in peak and decline basins 

library(tidyverse)

# 1. Percent of world's irrigated food production (by weight) in basin
# ... that will experience peak depletion this century.


read_csv("gcam_basin_ids.csv") %>% 
  rename(basin = GCAM_basin_ID) %>% 
  select(basin, GCAM_basin_name, GLU_name) -> basin_names

read_csv("basin.csv") %>% 
  bind_cols(read_csv("GPW_population.csv")) %>% 
  group_by(basin) %>% summarise(pop_2010 = sum(X2015) * 1e-6) %>% 
  left_join(basin_names) -> pop_by_basin
  

# read data and determine whether peak occurs pre-2100
read_csv("groundwater_production_FINAL.csv") %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year != 1975) %>% 
  group_by(scenario, groundwater) %>%
  summarise(max_yr = .$year[which.max(value)],
            taken = sum(value)) %>% 
  ungroup() %>% 
  mutate(peak_by_2100 = case_when(
    taken == 0 ~ FALSE,
    max_yr > 2095 ~ FALSE,
    taken > 0 & max_yr <= 2095 ~ TRUE)
  ) %>% 
  #tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>% 
  rename(GCAM_basin_name = groundwater) %>%
  mutate(GCAM_basin_name = substr(GCAM_basin_name, 1, nchar(GCAM_basin_name) - 18)) %>% 
  select(scenario, GCAM_basin_name, peak_by_2100) ->
  gw_peak_by_2100

# gw_peak_by_2100 %>% left_join(pop_by_basin) %>% 
#   mutate(pop_peak = pop_2010 * peak_by_2100) %>% 
#   group_by(scenario) %>% summarise(pop_peak = sum(pop_peak)) %>% 
#   ggplot(aes(pop_peak)) + geom_histogram(bins = 20) +
#   labs(y = "Number of scenarios",
#        x = "Population (millions)",
#        title = "Population living in basin that will face peak groundwater depletion in the 21st century")

gw_peak_by_2100 %>% left_join(pop_by_basin) %>% 
  mutate(pop_peak = pop_2010 * peak_by_2100) %>% 
  group_by(scenario) %>% 
  summarise(pop_peak = sum(pop_peak),
            pop_all = sum(pop_2010)) %>% 
  mutate(pop_peak_frac = pop_peak/pop_all,
         pop_nonpeak_frac = 1 - pop_peak_frac) %>% 
  ggplot() + 
  geom_histogram(aes(pop_peak_frac), bins = 20, fill = 'red4') +
  #geom_histogram(aes(pop_nonpeak_frac),bins = 20, fill = 'forestgreen', alpha = 0.75) +
  #geom_vline(aes(xintercept=mean(pop_peak_frac), color="red"), linetype="dashed", size=1) +
  # geom_density(aes(pop_peak_frac), fill = 'red4',alpha = 1) +
  # geom_density(aes(pop_nonpeak_frac), fill = 'yellow1',alpha = 0.75) +
  geom_segment(aes(x = mean(pop_peak_frac)-sd(pop_peak_frac), y = 300, xend = mean(pop_peak_frac)+sd(pop_peak_frac), yend = 300, colour = "black"), lineend = "round" ) +
  geom_point(aes(x=mean(pop_peak_frac), y=300, color="black"), size=2) + 
  geom_label(aes(x=mean(pop_peak_frac), y=270, label = paste("On average", round(mean(pop_peak_frac)*100,1),"% of Global Population \n(",round(mean(pop_peak_frac)*mean(pop_all)/1000,1), "billion) will Experience Groundwater \nStress over the 21st Century"))) +
  labs(y = "Number of Scenarios",
     x = "Fraction of Global Population in 2015",
     title = "Fraction of 2015 Global Population Living in Peak-and-Decline Basins") +
  #  xlim(c(0.2,0.8))+ ylim(c(0,400)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"),
        legend.position = 'none')



bind_rows(
  read_csv("ag_prod_basin_gfdl.csv"),
  read_csv("ag_prod_basin_hadg.csv"),
  read_csv("ag_prod_basin_ipsl.csv"),
  read_csv("ag_prod_basin_miro.csv"),
  read_csv("ag_prod_basin_nrsm.csv")
) %>% filter(tech == "IRR") %>% 
  filter( scenario == "wg_lo_rs_ssp1_gfdl_2p6") %>% 
  select(-scenario, -tech) -> irr_2010

irr_2010 %>% arrange(-Production_Mt) %>% mutate(x = cumsum(Production_Mt),
                                                x / sum(Production_Mt)) %>% 
  print(n = 14)

gw_peak_by_2100 %>% 
  left_join(basin_names) %>% 
  left_join(irr_2010) %>% 
  mutate(irr_peak = Production_Mt * peak_by_2100) %>% 
  group_by(scenario) %>% summarise(irr_peak = sum(irr_peak, na.rm = T)) %>% 
  mutate(irr_peak_pct = 100 * irr_peak / (irr_2010 %>% .$Production_Mt %>% sum())) %>% 
  ggplot(aes(irr_peak_pct)) + geom_histogram(bins = 20) +
  labs(y = "Number of scenarios",
       x = NULL,
       title = "Irrigated production (% of global total) from basins that will face peak groundwater depletion in the 21st century")

gw_peak_by_2100 %>% 
  left_join(basin_names) %>% 
  left_join(irr_2010) %>% 
  mutate(irr_peak = Production_Mt * peak_by_2100) %>% 
  group_by(scenario) %>% summarise(irr_peak = sum(irr_peak, na.rm = T)) %>% 
  mutate(irr_peak_pct = 100 * irr_peak / (irr_2010 %>% .$Production_Mt %>% sum())) %>% 
  ggplot(aes(irr_peak_pct)) +
  stat_ecdf(geom = "step", pad = FALSE) +
  labs(y = "Probability of non-exceedance",
       x = "Percent of irrigated production")

  
gw_peak_by_2100 %>% 
  left_join(basin_names) %>% 
  left_join(irr_2010) %>% 
  mutate(irr_peak = Production_Mt * peak_by_2100) %>% 
  group_by(scenario) %>% summarise(irr_peak = sum(irr_peak, na.rm = T)) %>% 
  mutate(irr_peak_pct = 100 * irr_peak / (irr_2010 %>% .$Production_Mt %>% sum())) %>% 
  mutate(bin = case_when(
    irr_peak_pct > 80 & irr_peak_pct <= 100 ~ "g80_100",
    irr_peak_pct > 60 & irr_peak_pct <= 80 ~ "g60_80",
    irr_peak_pct > 40 & irr_peak_pct <= 60 ~ "g40_60",
    irr_peak_pct > 20 & irr_peak_pct <= 40 ~ "g20_40",
    irr_peak_pct >= 0 & irr_peak_pct <= 20 ~ "g0_20"
  )) %>% 
  group_by(bin) %>% summarise(prob_ex = length(scenario))


pop_by_basin
