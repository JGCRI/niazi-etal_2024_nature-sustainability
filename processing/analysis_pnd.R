## Analysis script for peak and decline to generate key stats:
# peaks vs normal data, analysis by simulations vs by regions, peaks and total groundwater withdrawals by region basin and scenario, surface water to gw ratio,

# Hassan Niazi, May 2022

# rm(list = ls(all=TRUE))

library(tidyverse)
library(data.table)
library(RColorBrewer)

## read all data
read_csv("groundwater_production_FINAL.csv") %>% 
  mutate(basin = substr(groundwater, 1, nchar(groundwater) - 18)) %>% 
  select(scenario, region, basin, year, value) -> all_data

dt <- as.data.table(all_data)


# taking average withdrawals across all 60 scenarios for SSP1_RCP6.0 in year 2100 
read_csv("surface_water_production_all_.csv") -> all_sw_data

all_sw_data %>% 
  select('runoff water', scenario, year, value) %>%
  filter(year != 1975) %>% 
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>%
  mutate(ssp = gsub("ssp", "SSP", ssp), rcp = paste0("RCP", rcp), rcp = gsub("p", ".", rcp), ssp_rcp = paste0(ssp, "_", rcp)) %>%
  select(ssp_rcp, ssp, rcp, calib, surf, gw, 'runoff water', year, value) -> sw_data

sw_data %>% filter(ssp == 'SSP1', rcp == 'RCP6.0', year == 2100) %>% 
  summarise(avg_sw_ssp1rcp6 = sum(value)/60) -> SW_avg

all_data %>% select(groundwater, scenario, year, value) %>%
  filter(year != 1975) %>% 
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>%
  mutate(ssp = gsub("ssp", "SSP", ssp), rcp = paste0("RCP", rcp), rcp = gsub("p", ".", rcp), ssp_rcp = paste0(ssp, "_", rcp)) %>%
  select(ssp_rcp, ssp, rcp, calib, surf, gw, groundwater, year, value) -> gw_data

gw_data %>% filter(ssp == 'SSP1', rcp == 'RCP6.0', year == 2100) %>% 
  summarise(avg_gw_ssp1rcp6 = sum(value)/60) -> GW_avg


## PnD for total water

mutate(value = all_data$value + sw_data$value) 


## Pnd for surface water

all_sw_data %>%   
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
  year_sw_total


## New global PnD plot ----

colourCount = length(unique(history$study))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

## Same figure as below after archived with different arrangement (marginals at bottom left instead of top right)

## Global Peak and decline with marginals 
## Main middle plot ----
year_sw_total %>%
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
  geom_line(aes(x = year, y = median), color = "black", size = 1.5) + theme_classic() 
  #geom_line(aes(x = year, y = median), color = "black", size = 1.5, alpha=0.25) +
  #stat_smooth(aes(x = year, y = median),se=FALSE, geom="line", method = 'loess', alpha=1, size=2)+
  #geom_point(data=gw_peaks_detailed, aes(x= mean(max_yr), y=mean(max)), color="black", size=4) + #global mean
  #geom_vline(data=gw_peaks_detailed, aes(xintercept = mean(max_yr)), size = 0.5, color = "grey80", linetype = "dashed")+
  #geom_hline(data=gw_peaks_detailed, aes(yintercept = mean(max)), size = 0.5, color = "grey80", linetype = "dashed")+
  
  #ylim(0, 1200) +
  scale_x_continuous(expand = c(0.0, 0),
                     breaks = c(1960, 1980, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2075, 2080, 2090, 2100),
                     labels = c("1960", "1980", "2000", "2010", "", "2030", "", "2050", "", "", "2075", "", "", "2100")  
                     #breaks = c(1960, 1980, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2075, 2090, 2100),
                     #labels = c("1960", "1980", "2000", "2010", "", "2030", "", "2050", "", "2075", "", "2100")  
  ) + 
  scale_y_continuous(limits = c(2000,7000), expand = c(0, 0)) +
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


## FAULTY -> PREPARE SUM OF VALUES ACROSS YEARS WHEN SELECTING MAX VALUE: When selecting the year in a basin or a scenario which has a max value of withdrawals you need to select a value which is SUMMED over the years or scenarios. You are getting values of lesser magnitudes. For basins sum of all scenarios, for scenarios sum over all basins 
# NOOOOOOOOOOOOOOOOOOOOOOOOOOOOO -> for basins you can't aggregate across anything, because you want to know when does the peak happen in a CERTAIN scenario NOT across all scenarios. Summing across all scenarios does not mean anything practically and physically in the real world 
# But DO sum across all basins when finding GLOBAL peak in a scenario. DO NOT sum when you want to know WHICH BASIN peaks in a certain scenario. 

#all_data %>% group_by(scenario, year) %>% summarise(value = sum(value)) -> peaks


## Peaks and total withdrawals by categories ----

# by region 
arrange(dt[dt[, .I[which.max(value)], by=region]$V1],region) -> dt_byregion      # arrange to enable cbind with tot_basins because group_by arranges data in ascending order 


# this might be flawed because we are summing across all scenarios, which doesn't make sense 
# all_data %>% group_by(region) %>%  summarise(total_byregion = sum(value)) %>% select(total_byregion)-> tot_byregion 
# pnd_byregion = cbind(dt_byregion, tot_byregion)

all_data %>% group_by(scenario, region) %>%  summarise(total_byregion_scen = sum(value)) -> tot_byregion_scen
dt_byregion %>% left_join(tot_byregion_scen, by = c("scenario","region")) -> pnd_byregion_scen

pnd_byregion_scen %>% summarise(pnd_tot_global_R = sum(total_byregion_scen))

# by basin: simple peak and total 
arrange(dt[dt[, .I[which.max(value)], by=basin]$V1],basin) -> dt_bybasin 

# all_data %>% group_by(basin) %>%  
#   summarise(total_bybasin = sum(value)) %>% select(total_bybasin)-> tot_bybasin 
# pnd_bybasin = cbind(dt_bybasin, tot_bybasin) 
all_data %>% group_by(scenario, basin) %>%  summarise(total_bybasin_scen = sum(value)) -> tot_bybasin_scen
dt_bybasin %>% left_join(tot_bybasin_scen, by = c("scenario","basin")) -> pnd_bybasin_scen

pnd_bybasin_scen %>% summarise(pnd_tot_global_B = sum(total_bybasin_scen))



## TODO: Correct the sum across all scenarios issue in lines below this. See above
# by basin: median across all scenarios 
arrange(dt[dt[, .I[which.max(value)], by=basin]$V1],basin) %>% filter(value > 0)  -> dt_bybasin_nonzero

all_data %>% filter(value > 0) %>% group_by(basin) %>%
  summarise(total_bybasin = sum(value),
            mean_year = round(mean(year), 0),
            med_year = median(year),
            mean_withdrawal = mean(value),
            med_withdrawal = median(value)) %>%  
  select(mean_year, med_year, mean_withdrawal, med_withdrawal, total_bybasin) -> tot_bybasin 

pnd_bybasin = cbind(dt_bybasin_nonzero, tot_bybasin) 

# by basin: median of peaks across all scenarios
arrange(dt[dt[, .I[which.max(value)], by=basin]$V1],basin) %>% filter(value > 0)  -> dt_bybasin_nonzero

all_data %>% filter(value > 0) %>% group_by(basin) %>% # sum for total extraction
  summarise(total_bybasin = sum(value)) %>% 
  select(total_bybasin) -> tot_bybasin_nonzero

all_data %>% filter(value > 0) %>% group_by(basin, scenario) %>% # calculate mean and median of peaks
  summarise(max_year = max(year)) %>% 
  ungroup() %>% 
  group_by(basin) %>% 
  summarise(mean_peakyear = round(mean(max_year), 0),
            med_peakyear = median(max_year),
            #mean_peakwithdrawal = mean(value),
            #med_peakwithdrawal = median(value)
            ) %>%  
  select(mean_peakyear, med_peakyear) -> cent_bybasin 

pnd_bybasin_max = cbind(dt_bybasin_nonzero, tot_bybasin_nonzero, cent_bybasin)
#write.csv(pnd_bybasin_max, file = "pnd_bybasin_peaks.csv")



# by scenario
arrange(dt[dt[, .I[which.max(value)], by=scenario]$V1], scenario) -> dt_byscenario
#all_data %>% group_by(scenario) %>%  summarise(total_byscenario = sum(value)) %>% select(total_byscenario)-> tot_byscenario
#pnd_byscenario = cbind(dt_byscenario, tot_byscenario)

all_data %>% filter(value > 0) %>% group_by(scenario) %>%  
  summarise(total_byscenario = sum(value),
            mean_year = round(mean(year), 0),
            med_year = median(year),
            mean_withdrawal = mean(value),
            med_withdrawal = median(value)) %>%  
  select(mean_year, med_year, mean_withdrawal, med_withdrawal, total_byscenario)-> tot_byscenario_all 


pnd_byscenario_all = cbind(dt_byscenario, tot_byscenario_all)

# write .csv files 
#write.csv(pnd_byregion, file = "pnd_byregion.csv")
#write.csv(pnd_bybasin_scen, file = "pnd_bybasinv1.csv")
#write.csv(pnd_byscenario, file = "pnd_byscenario.csv")

#write.xlsx(pnd_byregion, file = "pnd_bycategories.xlsx", sheetName =  "pnd_byregion", append = T)
#write.xlsx(pnd_bybasin, file = "pnd_bycategories.xlsx", sheetName =  "pnd_bybasin", append = T)
#write.xlsx(pnd_byscenario_all, file = "pnd_bycategories.xlsx", sheetName =  "pnd_byscenario", append = T)


## todo
# add sw/gw ratios
# get peak and decline quantification 
# analyze raw data further in different ways 
# along with peaks, get central tendency (mean, median) statistics as well -> could be done in summarise line of tot_cat





########### ARCHIVED ########################################################

## summary by energy regions (peak year, peak water, total water withdrawals)

all_data %>% group_by(region) %>%
  summarise(
    max = max(value),
    max_yr = .$year[which.max(value)],
    total = sum(value)  ) -> df_byregions

#write.csv(df_byregions, file = "pndsummary_byregions.csv")

# summary by basins (peak year, peak water, total water withdrawals)

all_data %>% group_by(basin) %>%
  summarise(
    max = max(value),
    max_yr = .$year[which.max(value)],
    total = sum(value)
  )  -> df_bybasins

#write.csv(df_bybasins, file = "pndsummary_bybasins.csv")


# summary by scenarios (peak year, peak water, total water withdrawals)
all_data %>%
  select(basin, region, scenario, year, value) %>%
  filter(year != 1975) %>%
  group_by(scenario, region, basin, year) %>%
  summarise(value = sum(value)) %>%
  summarise(
    max = max(value),
    max_yr = .$year[which.max(value)],
    basin = .$basin[which.max(value)],
    region = .$region[which.max(value)],
    total = sum(value)
  ) %>%
  tidyr::separate(scenario, c("calib", "gw", "surf", "ssp", "gcm", "rcp")) %>%
  mutate(
    ssp = gsub("ssp", "SSP", ssp),
    rcp = paste0("RCP", rcp),
    rcp = gsub("p", ".", rcp),
    ssp_rcp = paste0(ssp, "_", rcp)
  ) %>%
  select(ssp_rcp, ssp, rcp, region, basin, max, total, max_yr, calib, surf, gw) ->
  df_byscenarios

#write.csv(df_byscenarios, file = "pndsummary_byscenarios.csv")









