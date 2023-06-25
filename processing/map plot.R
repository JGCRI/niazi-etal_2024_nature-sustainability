# probability of 21st century peak

# rm(list = ls(all=TRUE))

library(tidyverse)
library(rgdal)
library(tmap)

basin_ids <- read_csv("gcam_basin_ids.csv") %>%
  select(GCAM_basin_ID, GCAM_basin_name)


# read data
read_csv("groundwater_production_FINAL.csv") %>% 
  select(groundwater, scenario, year, value) -> gw_prod_all

read_csv("surface_water_production_all_.csv") %>% 
  select(runoff = `runoff water`, scenario, year, value) -> sw_prod_all

read_csv("prices_water_withdrawal_all.csv") %>% 
  filter(year == 2100) %>% 
  mutate(basin = substr(market, 1, nchar(market) - 18)) %>% 
  mutate(basin = substr(basin, 1, nchar(basin) / 2)) %>% 
  mutate(price = value * 4.7) %>% 
  # ^^ 1975 - 2019 USD
  select(-market, -Units, -value, -year) ->
  water_price
  

# get proportion of water met by GW for each year

left_join(
  sw_prod_all %>% rename(basin = runoff, sw = value),
  gw_prod_all %>% rename(basin = groundwater, gw = value),
  by = c("basin", "scenario", "year")
) %>% 
  mutate(gw_ratio = gw / (gw + sw)) %>% 
  mutate(basin = substr(basin, 1, nchar(basin) - 18)) %>% 
  select(-sw, -gw) ->
  gw_ratio
  

# identify basins which peak and decline 

gw_prod_all %>% 
  filter(year > 1975) %>% 
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
  rename(basin = groundwater) %>%
  mutate(basin = substr(basin, 1, nchar(basin) - 18)) %>% 
  select(scenario, basin, peak_by_2100, max_yr) %>% 
  left_join(gw_ratio, by = c("scenario", "basin", "max_yr" = "year")) %>% 
  left_join(water_price, by = c("scenario", "basin")) %>% 
  mutate(max_yr = if_else(peak_by_2100 == FALSE, as.integer(2101), as.integer(max_yr))) ->
  gw_peak_by_2100

# scenarios not showing peak and decline 
gw_prod_all %>% 
  select(groundwater, scenario, year, value) %>% 
  filter(year >= 2010) %>% 
  group_by(year, scenario) %>%
  summarise(value = sum(value)) %>% ungroup() %>% 
  group_by(scenario) %>% 
  summarise(max = max(value),
            max_yr = year[which.max(value)]) %>% 
  ungroup() %>% 
  mutate(peak_by_2100 = case_when(
    max == 0 ~ FALSE,
    max_yr > 2095 ~ FALSE,
    max > 0 & max_yr <= 2095 ~ TRUE)
  ) %>% 
  filter(peak_by_2100 == FALSE) -> notpndscenarios

#write.csv(pndscenarios_basins, file = 'pndscenarios_basins.csv')

gw_prod_all %>% 
  filter(year > 1975) %>% 
  group_by(scenario, groundwater) %>%
  summarise(max_yr = .$year[which.max(value)],
            taken = sum(value)) %>% 
  ungroup() %>% 
  mutate(notpeak_by_2100 = case_when(
    taken == 0 ~ FALSE,
    max_yr > 2095 ~ FALSE,
    taken > 0 & max_yr <= 2095 ~ TRUE)
  ) %>% 
  #tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>% 
  rename(basin = groundwater) %>%
  mutate(basin = substr(basin, 1, nchar(basin) - 18)) %>% 
  select(scenario, basin, notpeak_by_2100, max_yr) %>% 
  left_join(gw_ratio, by = c("scenario", "basin", "max_yr" = "year")) %>% 
  left_join(water_price, by = c("scenario", "basin")) %>% 
  mutate(max_yr = if_else(notpeak_by_2100 == FALSE, as.integer(2101), as.integer(max_yr))) ->
  gw_not_peak_by_2100



# calculate number of basins showing peak and decline in each scenario 
gw_peak_by_2100 %>% 
  group_by(scenario) %>% 
  #summarise(pndbasins_num = peak_by_2100==TRUE) %>% 
  summarise(
    pndbasins_num = sum(peak_by_2100),
    pndbasins_prec = sum(peak_by_2100) / length(basin)) %>% 
  ungroup() -> pndbasins_byscenarios

pndbasins_byscenarios %>% 
  summarise(mean_num = mean(pndbasins_num),
            sd_num = sd(pndbasins_num),
            mean_prec = mean(pndbasins_prec),
            sd_prec = sd(pndbasins_prec)) -> pndbasins_byscenarios_summary


# median depletion 
gw_prod_all %>%
  rename(basin = groundwater) %>%
  mutate(basin = substr(basin, 1, nchar(basin) - 18)) %>%
  group_by(basin, scenario) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  summarise(`(c) Median depletion (BCM)` = median(value),
            `(c) Mean depletion (BCM)` = mean(value)) %>% 
  left_join(basin_ids, by = c("basin" = "GCAM_basin_name")) %>% 
  select(-basin) ->
  med_cum_dep

# check for NA values
gw_peak_by_2100 %>% filter(is.na(peak_by_2100))

# compute probability of peaking for each basin
gw_peak_by_2100 %>% 
  group_by(basin) %>%
  summarise(prob = sum(peak_by_2100) / length(scenario),
            med_peak_yr = median(max_yr, na.rm = T),
            gw_ratio = median(gw_ratio),
            price = median(price)) %>% 
  ungroup() ->
  gw_peak_by_2100_prob

gw_peak_by_2100_prob %>% 
  left_join(basin_ids, by = c("basin" = "GCAM_basin_name")) %>%
  #write_csv("gw_key_stats.csv")
  select(GCAM_basin_ID, prob, med_peak_yr, gw_ratio, price) %>% 
  #rename(`(b) Median peak year` = med_peak_yr) %>% 
  replace_na(list(prob = 0)) %>% 
  mutate(`(a) Number (%), peak scenarios` = case_when(
    prob == 0 ~ "0",
    prob < 0.25 ~ "1 - 224 (less than 25%)",
    prob < 0.5 & prob >= 0.25 ~ "225 - 449 (25 - 50 %)",
    prob < 0.75 & prob >= 0.5 ~ "450 - 674 (50 - 75 %)",
    prob < 0.95 & prob >= 0.75 ~ "675 - 855 (75 - 95%)",
    prob >= 0.95 ~ "855 - 900 (more than 95%)"
    )) %>% 
  # mutate(`(b) Median peak year` = case_when(
  #   `(b) Median peak year` >= 1990 & `(b) Median peak year` < 2015 ~ "1990 - 2014",
  #   `(b) Median peak year` >= 2015 & `(b) Median peak year` < 2040 ~ "2015 - 2039",
  #   `(b) Median peak year` >= 2040 & `(b) Median peak year` < 2060 ~ "2040 - 2059",
  #   `(b) Median peak year` >= 2060 & `(b) Median peak year` < 2080 ~ "2060 - 2079",
  #   `(b) Median peak year` >= 2080 & `(b) Median peak year` < 2100 ~ "2080 - 2100",
  #   `(b) Median peak year` >= 2100  ~ "No peak and decline"
  # )) %>% 
  mutate(`(b) Median peak year` = case_when(
    med_peak_yr >= 1990 & med_peak_yr < 2015 ~ "1990 - 2014",
    med_peak_yr >= 2015 & med_peak_yr < 2040 ~ "2015 - 2039",
    med_peak_yr >= 2040 & med_peak_yr < 2060 ~ "2040 - 2059",
    med_peak_yr >= 2060 & med_peak_yr < 2080 ~ "2060 - 2079",
    med_peak_yr >= 2080 & med_peak_yr < 2096 ~ "2080 - 2095",
    med_peak_yr >= 2095  ~ "No peak and decline"
  )) %>% 
  left_join(med_cum_dep, by = "GCAM_basin_ID") %>% 
  mutate(`(c) Median depletion (BCM)` = case_when(
    `(c) Median depletion (BCM)` == 0 ~ "0",
    `(c) Median depletion (BCM)` > 0 & `(c) Median depletion (BCM)` < 200 ~ "0 - 200",
    `(c) Median depletion (BCM)` >= 200 & `(c) Median depletion (BCM)` < 400 ~ "200 - 400",
    `(c) Median depletion (BCM)` >= 400 & `(c) Median depletion (BCM)` < 600 ~ "400 - 600",
    `(c) Median depletion (BCM)` >= 600 & `(c) Median depletion (BCM)` < 800 ~ "600 - 800",
    `(c) Median depletion (BCM)` >= 800 & `(c) Median depletion (BCM)` < 1000 ~ "800 - 1000",
    `(c) Median depletion (BCM)` > 1000  ~ "more than 1000"
  )) %>% 
  mutate(`(d) Peak-year dependence (%)` = case_when(
    gw_ratio == 0 ~ "0",
    gw_ratio > 0 & gw_ratio <= 0.2 ~ "0 - 20",
    gw_ratio > 0.2 & gw_ratio <= 0.4 ~ "20 - 40",
    gw_ratio > 0.4 & gw_ratio <= 0.6 ~ "40 - 60",
    gw_ratio > 0.6 & gw_ratio <= 0.8 ~ "60 - 80",
    gw_ratio > 0.8 ~ "80 - 100"
  )) %>% 
  mutate(`(e) Water price (USD per m3)` = case_when(
    price > 0 & price <= 0.01 ~ "0 - 0.01",
    price > 0.01 & price <= 0.1 ~ "0.01 - 0.1",
    price > 0.1 & price <= 0.5 ~ "0.1 - 0.5",
    price > 0.5 & price <= 1 ~ "0.5 - 1",
    price > 1 & price <= 5 ~ "1 - 5",
    price > 5 ~ "More than $5"
  )) ->
  gw_peak_prob_mapping

#write.csv(gw_peak_prob_mapping, file = "pnd_bybasins_alldatasummary.csv")

basin_ids  %>% right_join(gw_peak_prob_mapping, by = 'GCAM_basin_ID') -> gw_peak_prob_mapping_names

#write.csv(gw_peak_prob_mapping_names, file = "pnd_bybasins_alldatasummary_names.csv")
#write.csv(gw_peak_prob_mapping_names, file = "pnd_bybasins_alldatasummary_names_v1.csv")

#basin_map <- readOGR("../../GCAM/GCAM 235 basin/Global235_CLM_05_dissolve.shp")
#basin_map <- readOGR("GCAM 235 basin/Global235_CLM_05_dissolve.shp")
basin_map <- readOGR("shapefiles/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp")
#basin_map_st <- st_read('shapefiles/glu_boundaries_moirai_combined_3p1_0p5arcmin.shp')

basin_map@data <- left_join(basin_map@data, gw_peak_prob_mapping, by = c("glu_id" = "GCAM_basin_ID")) %>% 
  replace_na(list(`(a) Number (%), peak scenarios` = "0",
                  #`(b) Median peak year` = "2080 - 2100",
                  `(b) Median peak year` = "No peak and decline",
                  `(c) Median depletion (BCM)` = "0",
                  `(d) Peak-year dependence (%)` = "0",
                  `(e) Water price (USD per m3)` = "0 - 0.01"))

tm_shape(basin_map) +
  tm_fill(col = "(a) Number (%), peak scenarios",
          title = "Number of scenarios w/ peak and decline",
          colNA = "gray99",
          palette = rev(RColorBrewer::brewer.pal(7, "RdYlGn"))[2:7]) +
  tm_borders(col = "black", lwd = 0.25) +
  tm_legend(legend.title.size = 1.5,
            legend.text.size = 1,
            legend.outside = T,
            legend.outside.position = "bottom",
            bg.color = "white") +
  tm_layout(frame = F) #+
  #tm_text("Basin_na_2", size = 0.5)

tm_shape(basin_map) +
  tm_fill(col = "(b) Median peak year",
          colNA = "gray99",
          palette = c(RColorBrewer::brewer.pal(7, "BuPu")[6:2], "lightgrey")) +
  tm_borders(col = "black", lwd = 0.25) +
  tm_legend(legend.title.size = 1.5,
            legend.text.size = 1,
            legend.outside = T,
            legend.outside.position = "bottom",
            position=c("left", "bottom"),
            bg.color = "white") +
  tm_layout(frame = F)


tm_shape(basin_map) +
  tm_fill(col = "(c) Median depletion (BCM)",
          colNA = "gray99",
          palette = c("lightgrey", RColorBrewer::brewer.pal(6, "YlOrRd"))) +
  tm_borders(col = "black", lwd = 0.25) +
  tm_legend(legend.title.size = 1.5,
            legend.text.size = 1,
            legend.outside = F,
            legend.outside.position = "bottom",
            bg.color = "white") +
  tm_layout(frame = F)

tm_shape(basin_map) +
  tm_fill(col = "(d) Peak-year dependence (%)",
          colNA = "gray99",
          palette = c("lightgrey", RColorBrewer::brewer.pal(6, "YlOrRd"))) +
  tm_borders(col = "black", lwd = 0.25) +
  tm_legend(legend.title.size = 1.5,
            legend.text.size = 1,
            legend.outside = F,
            legend.outside.position = "bottom",
            bg.color = "white") +
  tm_layout(frame = F)

tm_shape(basin_map) +
  tm_fill(col = "(e) Water price (USD per m3)",
          colNA = "gray99",
          palette = RColorBrewer::brewer.pal(6, "PuBuGn")) +
  tm_borders(col = "black", lwd = 0.25) +
  tm_legend(legend.title.size = 1.5,
            legend.text.size = 1,
            legend.outside = F,
            legend.outside.position = "bottom",
            bg.color = "white") +
  tm_layout(frame = F)





