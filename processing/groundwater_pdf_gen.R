

#rm(list = ls(all=TRUE))

library(dplyr)
source("functions.R")


process_production_data <- function(gcm){
  read.csv(paste0("output/ag_production_by_subsector_", gcm, ".csv")) %>% 
    filter(sector!="Forest")  %>% filter(year>=2010) %>%
    mutate(gcm = gcm,
           ssp = substr(scenario, 10, 13),
           rcp = substr(scenario, 20, 22)) %>% 
    dplyr::select(-Units, -output,  -technology, -sector) 
}


bind_rows(
  process_production_data("gfdl"),
  process_production_data("hadg"),
  process_production_data("nrsm"),
  process_production_data("miro"),
  process_production_data("ipsl")
) -> ag.prod


ag.prod <-aggregate(value ~ year + scenario + subsector, data=ag.prod,sum)
ag.prod$subsector <- gsub("Root_Tuber","RootTuber",ag.prod$subsector)
ag.prod$subsector <- gsub("biomass_grass","biomass-grass",ag.prod$subsector)
ag.prod$subsector <- gsub("biomass_tree","biomass-tree",ag.prod$subsector)
basin.prod <- ag.prod %>% mutate(crop=sub("_.*","",subsector),basin.name=sub(".*_","",subsector))  

basin.id <- read.csv("basin_ID_swap.csv",sep=",")
basin.prod <- merge(basin.id, basin.prod, by=c("basin.name"))

global.production <- aggregate(value ~ crop + scenario + year,basin.prod,sum)
global.production <- colnameReplace(global.production,"value","global.production")

groundwater <- read.csv("groundwater_production_full_900.csv",sep=",")
groundwater <- aggregate(value ~ year + scenario + groundwater,data=groundwater,sum) %>% filter(year>=2010) 
groundwater <- groundwater[!(groundwater$year==2015 & groundwater$scenario=="gl_md_ex_ssp4_hadg_4p5"),] 


max.basin <- groundwater %>% group_by(groundwater,scenario) %>% summarise(value = max(value)) %>% merge(groundwater, by=c("scenario","value","groundwater"))
peak.and.decline.basin <- groundwater %>% filter(year==2100) %>% colnameReplace("value","end.of.century.value") %>% merge(max.basin, by=c("scenario","groundwater") ) %>% mutate(peak = ((value - end.of.century.value)/value)*100) %>% filter(peak>0) 
peak.and.decline.basin$groundwater <- gsub("*.water withdrawals","",peak.and.decline.basin$groundwater) 
peak.and.decline.basin <- colnameReplace(peak.and.decline.basin,"groundwater","basin.name.old") %>% dplyr::select(scenario, basin.name.old)

peak.and.decline <- merge(basin.prod,peak.and.decline.basin,by=c("scenario","basin.name.old"))

pdf.basin.prod <-  aggregate(value ~ scenario + year + crop,data=peak.and.decline,sum)
pdf.basin.prod <- merge(pdf.basin.prod,global.production,by=c("year","crop","scenario"),all=T)
pdf.basin.prod$percent <- (pdf.basin.prod$value / pdf.basin.prod$global.production) *100


write.csv(pdf.basin.prod,"pdf_production.csv")




