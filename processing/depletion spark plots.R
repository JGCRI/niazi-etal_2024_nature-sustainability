#Ag basins!

# rm(list = ls(all=TRUE))

library(tidyverse)
 
# read data and determine whether peak occurs pre-2100
read_csv("groundwater_production_FINAL.csv") %>% 
  select(groundwater, scenario, year, value) %>% 
  mutate(groundwater = substr(groundwater, 1, nchar(groundwater) - 18)) %>% 
  filter(year > 1960) -> full_data

full_data -> all_data

read_csv("gw_key_stats.csv") %>% 
  mutate(med_peak_yr = if_else(med_peak_yr == 2101, "21^^", as.character(med_peak_yr))) -> key_stats


# if basing selected basins on most ag currently...
read_csv("ag_prod_basin_gfdl.csv") %>% 
  filter(tech == "IRR") %>% 
  filter(scenario == "wg_lo_rs_ssp1_gfdl_2p6") %>% 
  select(-scenario, -tech) %>% 
  arrange(-Production_Mt) %>% mutate(x = cumsum(Production_Mt),
                                     prop = x / sum(Production_Mt)) %>% 
  top_n(25, -prop) %>% print(n=25)


# if basing selected basins on most important (vulnerable) basins
#PSPD>50% & GIR > 20% & Irrig in 2010 > 20bcm/yr
key_stats %>% 
  filter(prob > 0.7,
         gw_ratio > 0.2) %>% 
  arrange(-prob) %>% print(n=100)


key_stats %>% filter(grepl("Krishna", basin)) -> ks

full_data %>%
  filter(groundwater == "Krishna") %>%
  group_by(year) %>%
  summarise(med_dep = median(value),
            up_dep = quantile(value, 0.95),
            lo_dep = quantile(value, 0.05),
            up_quar = quantile(value, 0.75),
            lo_quar = quantile(value, 0.25),
            min = min(value),
            max = max(value)) %>%
  ggplot(aes(year)) +
  geom_ribbon(aes(x = year, ymin = min, ymax = max), fill = "red", alpha = 0.15) +
  geom_ribbon(aes(ymin = lo_dep, ymax = up_dep), alpha = 0.2) +
  geom_ribbon(aes(ymin = lo_quar, ymax = up_quar), fill = "dodgerblue", alpha = 0.5) +
  geom_line(aes(y = med_dep), lwd = 1.5) +
  #facet_wrap(~groundwater, ncol = 2, scales = "free_y") +
  xlim(2015, 2100) +
  theme(legend.position = "right",
        #title = element_text(size = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA)) +
  labs(title = "Krishna",
       subtitle = paste0("", round(ks$prob * 100, 0), "%",
                         " | ", ks$med_peak_yr,
                         " | ", round(ks$gw_ratio * 100, 0), "%") ,
                         "     $: ", round(ks$price, 4))

## For all basins 

# prepare basin names
#full_data %>% group_by(groundwater) %>% summarise(value = sum(value)) %>%  filter(value>0) %>%  select(groundwater) %>%   mutate(groundwater=paste0("123",groundwater,"234"))-> basins
#write.csv(basins, file = "basin_names_forplots.csv")

basin_names_all <- c("Adriatic Sea-Greece-Black Sea Coast" , "Africa-East Central Coast" , "Africa-Indian Ocean Coast" , "Africa-North Interior" , "Africa-North West Coast" , "Africa-Red Sea-Gulf of Aden Coast" , "Africa-South Interior" , "Africa-West Coast" , "Amazon" , "Amu Darya" , "Amur" , "Angola-Coast" , "Arabian Peninsula" , "Arabian Sea Coast" , "Arctic Ocean Islands" , "Arkansas White Red" , "Atlantic Ocean Seaboard" , "Australia-East Coast" , "Australia-Interior" , "Australia-North Coast" , "Australia-South Coast" , "Australia-West Coast" , "Baja California" , "Baltic Sea Coast" , "Bay of Bengal-North East Coast" , "Black Sea-North Coast" , "Black Sea-South Coast" , "Bo Hai-Korean Bay-North Coast" , "Brahamani" , "California River" , "Caribbean" , "Caribbean Coast" , "Caspian Sea-East Coast" , "Caspian Sea-South West Coast" , "Caspian Sea Coast" , "Cauvery" , "Central Iran" , "Central Patagonia Highlands" , "Chao Phraya" , "China Coast" , "Churchill" , "Colombia-Ecuador-Pacific Coast" , "Congo" , "Danube" , "Daugava" , "Dead Sea" , "Denmark-Germany Coast" , "Dnieper" , "Dniester" , "Don" , "Douro" , "East Brazil-South Atlantic Coast" , "Eastern Jordan-Syria" , "Ebro" , "Elbe" , "Ems-Weser" , "England and Wales" , "Farahrud" , "Finland" , "Fly" , "France-South Coast" , "France-West Coast" , "Fraser" , "Ganges-Bramaputra" , "Gironde" , "Gobi Interior" , "Godavari" , "Great" , "Great Lakes" , "Grijalva-Usumacinta" , "Guadalquivir" , "Guadiana" , "Gulf of Guinea" , "Gulf of Thailand Coast" , "Hainan" , "Hamun-i-Mashkel" , "Hawaii" , "Helmand" , "Hong-Red River" , "Huang He" , "Hudson Bay Coast" , "Iceland" , "India East Coast" , "India North East Coast" , "India South Coast" , "India West Coast" , "Indus" , "Ireland" , "Irian Jaya Coast" , "Irrawaddy" , "Isthmus of Tehuantepec" , "Italy-East Coast" , "Italy-West Coast" , "Japan" , "Java-Timor" , "Kalimantan" , "Kara Sea Coast" , "Krishna" , "La Plata" , "La Puna Region" , "Lake Balkash" , "Lake Chad" , "Lena" , "Limpopo" , "Loire" , "Lower Colorado River" , "Lower Mississippi River" , "Mackenzie" , "Madasgacar" , "Magdalena" , "Mahandi" , "Mahi" , "Mar Chiquita" , "Mediterranean Sea-East Coast" , "Mediterranean Sea Islands" , "Mediterranean South Coast" , "Mekong" , "Mexico-Interior" , "Mexico-Northwest Coast" , "Mid Atlantic" , "Missouri River" , "Murray-Darling" , "Namibia-Coast" , "Narmada" , "Narva" , "Negro" , "Neman" , "Neva" , "New England" , "New Zealand" , "Niger" , "Nile" , "North and South Korea" , "North Argentina-South Atlantic Coast" , "North Borneo Coast" , "North Brazil-South Atlantic Coast" , "North Chile-Pacific Coast" , "North Gulf" , "Northeast South America-South Atlantic Coast" , "Northern Dvina" , "Northwest Territories" , "Ob" , "Oder" , "Ohio River" , "Orange" , "Orinoco" , "Pacific and Arctic Coast" , "Pacific Central Coast" , "Pacific Northwest" , "Palau and East Indonesia" , "Pampas Region" , "Papaloapan" , "Papua New Guinea Coast" , "Parnaiba" , "Peninsula Malaysia" , "Pennar" , "Persian Gulf Coast" , "Peru-Pacific Coast" , "Philippines" , "Plateau of Tibet Interior" , "Po" , "Poland Coast" , "Red Sea-East Coast" , "Rhine" , "Rhone" , "Rift Valley" , "Rio Balsas" , "Rio Grande River" , "Rio Lerma" , "Rio Verde" , "Russia-Barents Sea Coast" , "Russia-South East Coast" , "Sabarmati" , "Salinas Grandes" , "Salween" , "Sao Francisco" , "Saskatchewan-Nelson" , "Scandinavia-North Coast" , "Scheldt" , "Scotland" , "Seine" , "Senegal" , "Sepik" , "Shebelli-Juba" , "Siberia-North Coast" , "Siberia-West Coast" , "Sinai Peninsula" , "Sittang" , "Solomon Islands" , "South Africa-South Coast" , "South Africa-West Coast" , "South America-Colorado" , "South Argentina-South Atlantic Coast" , "South Atlantic Gulf" , "South Chile-Pacific Coast" , "South China Sea Coast" , "South Pacific Islands" , "Southern Central America" , "Spain-Portugal-Atlantic Coast" , "Spain-South and East Coast" , "Sri Lanka" , "St Lawrence" , "Sulawesi" , "Sumatra" , "Sweden" , "Syr Darya" , "Tagus" , "Taiwan" , "Tapti" , "Tarim Interior" , "Tasmania" , "Tennessee River" , "Texas Gulf Coast" , "Tiber" , "Tigris-Euphrates" , "Tocantins" , "Upper Colorado River" , "Upper Mississippi" , "Ural" , "Uruguay-Brazil-South Atlantic Coast" , "Viet Nam-Coast" , "Volga" , "Volta" , "Wisla" , "Xun Jiang" , "Yangtze" , "Yasai" , "Yenisey" , "Yucatan Peninsula" , "Zambezi" , "Ziya He-Interior")



basin_names_pnd <- c("Adriatic Sea-Greece-Black Sea Coast" ,	"Africa-Indian Ocean Coast" ,	"Africa-North Interior" ,	"Africa-North West Coast" ,	"Africa-Red Sea-Gulf of Aden Coast" ,	"Africa-West Coast" ,	"Amu Darya" ,	"Arabian Peninsula" ,	"Arabian Sea Coast" ,	"Arctic Ocean Islands" ,	"Arkansas White Red" ,	"Australia-East Coast" ,	"Australia-Interior" ,	"Australia-North Coast" ,	"Australia-West Coast" ,	"Baja California" ,	"Black Sea-South Coast" ,	"Bo Hai-Korean Bay-North Coast" ,	"California River" ,	"Caribbean" ,	"Caribbean Coast" ,	"Caspian Sea-East Coast" ,	"Caspian Sea-South West Coast" ,	"Caspian Sea Coast" ,	"Cauvery" ,	"Central Iran" ,	"Chao Phraya" ,	"China Coast" ,	"Congo" ,	"Dead Sea" ,	"Denmark-Germany Coast" ,	"Dniester" ,	"Douro" ,	"Eastern Jordan-Syria" ,	"Ebro" ,	"Elbe" ,	"Ems-Weser" ,	"England and Wales" ,	"Farahrud" ,	"France-South Coast" ,	"Ganges-Bramaputra" ,	"Gobi Interior" ,	"Godavari" ,	"Great" ,	"Guadalquivir" ,	"Guadiana" ,	"Gulf of Guinea" ,	"Hamun-i-Mashkel" ,	"Helmand" ,	"Huang He" ,	"India East Coast" ,	"India North East Coast" ,	"India South Coast" ,	"Indus" ,	"Italy-East Coast" ,	"Italy-West Coast" ,	"Japan" ,	"Krishna" ,	"La Plata" ,	"La Puna Region" ,	"Lake Balkash" ,	"Limpopo" ,	"Loire" ,	"Lower Colorado River" ,	"Lower Mississippi River" ,	"Madasgacar" ,	"Mahandi" ,	"Mahi" ,	"Mar Chiquita" ,	"Mediterranean Sea-East Coast" ,	"Mediterranean Sea Islands" ,	"Mediterranean South Coast" ,	"Mexico-Interior" ,	"Mexico-Northwest Coast" ,	"Mid Atlantic" ,	"Missouri River" ,	"Murray-Darling" ,	"Narmada" ,	"Negro" ,	"Nile" ,	"North and South Korea" ,	"North Argentina-South Atlantic Coast" ,	"North Brazil-South Atlantic Coast" ,	"North Chile-Pacific Coast" ,	"North Gulf" ,	"Northeast South America-South Atlantic Coast" ,	"Oder" ,	"Orange" ,	"Orinoco" ,	"Pacific and Arctic Coast" ,	"Pampas Region" ,	"Papua New Guinea Coast" ,	"Peninsula Malaysia" ,	"Pennar" ,	"Persian Gulf Coast" ,	"Peru-Pacific Coast" ,	"Philippines" ,	"Po" ,	"Poland Coast" ,	"Red Sea-East Coast" ,	"Rhine" ,	"Rio Balsas" ,	"Rio Grande River" ,	"Rio Lerma" ,	"Rio Verde" ,	"Sabarmati" ,	"Sao Francisco" ,	"Scheldt" ,	"Seine" ,	"Senegal" ,	"Sinai Peninsula" ,	"South Africa-South Coast" ,	"South Africa-West Coast" ,	"South America-Colorado" ,	"South Argentina-South Atlantic Coast" ,	"South Atlantic Gulf" ,	"Spain-South and East Coast" ,	"Sweden" ,	"Syr Darya" ,	"Tagus" ,	"Tapti" ,	"Tarim Interior" ,	"Tennessee River" ,	"Texas Gulf Coast" ,	"Tigris-Euphrates" ,	"Upper Colorado River" ,	"Volga" ,	"Volta" ,	"Yasai" ,	"Yucatan Peninsula" ,	"Ziya He-Interior")

key_stats %>% filter(grepl("Krishna", basin)) -> ks

all_data %>% 
  filter(groundwater %in% basin_names_pnd) %>%
  group_by(year, groundwater) %>%
  summarise(med_dep = median(value),
            up_dep = quantile(value, 0.95),
            lo_dep = quantile(value, 0.05),
            up_quar = quantile(value, 0.75),
            lo_quar = quantile(value, 0.25)) %>%
  ggplot(aes(year)) +
  geom_ribbon(aes(ymin = lo_dep, ymax = up_dep), alpha = 0.2) +
  geom_ribbon(aes(ymin = lo_quar, ymax = up_quar), fill = "dodgerblue", alpha = 0.5) +
  geom_line(aes(y = med_dep), lwd = 1.25) +
  facet_wrap(~groundwater, scales = "free_y") +
  #xlim(1990, 2100) +
  scale_x_continuous(expand = c(0.0, 0),
                     breaks = c(1990, 2000, 2010, 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100),
                     labels = c("1990", "", "", "", "", "", "2050", "", "", "", "", "2100") ) + 
  theme_void()+
  theme_classic() +
  theme(legend.position="none",
        #axis.line.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.ticks.x=element_blank(),
        #axis.title.x=element_blank(),
        strip.background = element_blank()
        #axis.title.y=element_line()
        #axis.line.x.bottom = element_blank()
        #panel.border = element_blank(),
        #panel.grid=element_blank()
  )



## Calibration data comparison

full_data %>% 
  mutate(source = if_else(
    grepl("gl_", scenario), "Gl", "WG")
  ) %>% 
  split(.$source) %>% 
  map(function(x){
    x %>% 
      group_by(groundwater, year) %>% 
      summarise(med_dep = median(value),
                up_dep = quantile(value, 0.9),
                lo_dep = quantile(value, 0.1)) %>% 
      ungroup() 
  }) -> split_ts

bind_rows(
  split_ts$Gl %>% mutate(cal = "Gleeson"),
  split_ts$WG %>% mutate(cal = "WaterGap")
) %>% 
  filter(grepl("Indus", groundwater) |
           grepl("Sabar", groundwater) |
           grepl("Gang", groundwater) |
           grepl("Pennar", groundwater)) %>% 
  #mutate(groundwater = substr(groundwater, 1, nchar(groundwater) - 18)) %>% 
  ggplot(aes(year)) +
  geom_line(aes(y = med_dep, col = cal), lwd = 1.5) +
  geom_ribbon(aes(ymin = lo_dep, ymax = up_dep,
                  fill = cal), alpha = 0.5) +
  facet_wrap(~groundwater, ncol = 2, scales = "free_y") +
  xlim(1990, 2100) +
  theme(legend.position = "right",
        #axis.title = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.ticks.x = element_blank(),
        #axis.text.x = element_blank(),
        strip.text.y = element_blank(),
        strip.background = element_blank(),
        panel.background = element_blank(),
        panel.spacing.y = unit(4, "lines")) +
  labs(title = "South Asia")


 
  