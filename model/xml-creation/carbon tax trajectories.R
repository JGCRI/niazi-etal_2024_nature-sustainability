# quick check: are CO2 prices trajectories consistent across water and ssp changes??

library(tidyverse)

process_tax_data <- function(gcm){
  read_csv(paste0("CO2_prices_min_", gcm, ".csv")) %>% 
    filter(grepl("glob", market)) %>% 
    mutate(gcm = !! gcm,
           ssp = substr(scenario, 1, 4),
           rcp = substr(scenario, 5, 9)) %>% 
    select(-Units, -market, -scenario)
}

# plot Min's results
bind_rows(
  process_tax_data("gfdl"),
  process_tax_data("hadgem"),
  process_tax_data("noresm"),
  process_tax_data("miroc"),
  process_tax_data("ipsl"),
  process_tax_data("woclimate"),
) %>% 
  ggplot(aes(year, value, color = gcm)) +
  geom_line() +
  facet_wrap(c("ssp", "rcp"),
             labeller = "label_both",
             scales = "free_y", nrow = 3)



# write out tax trajectories

bind_rows(
  process_tax_data("gfdl"),
  process_tax_data("hadgem"),
  process_tax_data("noresm"),
  process_tax_data("miroc"),
  process_tax_data("ipsl")
  ) -> tax_all_gcms

tax_all_gcms %>% 
  filter(year > 2035,
         ssp == "ssp5", rcp=="rcp85") %>%
  ggplot(aes(year, value, color = gcm)) + geom_line()
  group_by(ssp, rcp, year) %>% summarise(tax = median(value)) %>% ungroup() %>% 
  mutate(out = paste0('<fixedTax year=', year, '>', round(tax, 1), '</fixedTax>')) %>% 
  select(out) %>% write_csv("carbon tax trajectories/gg.csv")

  
  # quick check of GCAM result with new trajectories
  read_csv("CO2_prices_test.csv") %>% 
    filter(grepl("glob", market)) %>% 
    spread(scenario, value) %>% 
    print(n=22)
  

