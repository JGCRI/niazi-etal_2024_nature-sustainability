library(dplyr)


process_production_data <- function(gcm){
  read.csv(paste0("output/ag_production_by_subsector_", gcm, ".csv")) %>% 
    filter(sector!="Forest")  %>% filter(year>=2010) %>%
    mutate(gcm = gcm,
           ssp = substr(scenario, 10, 13),
           rcp = substr(scenario, 20, 22)) %>% 
    dplyr::filter(grepl("IRR",technology)) %>%
    mutate(subsector = gsub("Root_Tuber","RootTuber", subsector),
           subsector = gsub("biomass_grass","biomass-grass",subsector),
           subsector = gsub("biomass_tree","biomass-tree",subsector),
           basin.name=sub(".*_","",subsector)) %>%
    group_by(scenario,year,basin.name) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    group_by(scenario,year) %>%
    mutate(total.prod = summarise(value))
    dplyr::select(-Units, -output, -sector) 
}


bind_rows(
  process_production_data("gfdl"),
  process_production_data("hadg"),
  process_production_data("nrsm"),
  process_production_data("miro"),
  process_production_data("ipsl")
) -> ag.prod


write.csv(ag.prod,"irr_production.csv")




