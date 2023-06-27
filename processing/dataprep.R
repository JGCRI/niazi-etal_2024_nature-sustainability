# Peak and decline of global groundwater resources 
#
# Hassan Niazi, JGCRI USA, 2022 

# Inputs: output of 900 GCAM runs, basin names and mappings 

# load all packages ----
library(tidyverse)


# load data for post-processing and analysis ----

# groundwater 
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
           ssp_rcp = paste0(ssp, "_", rcp)) %>% 
    select(ssp_rcp, ssp, rcp, max, max_yr, calib, surf, gw) ->
    gw_peaks_detailed


# surface water data 



# pre-processing to prepare common data tables used while analysis and plotting ----

# groundwater peaks 



