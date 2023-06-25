library(dplyr)
name <- "ssp_profit_tests"

queryName <- "profit_rate"

make_query <- function(scenario){
	dbLoc <- paste0("/pic/projects/GCAM/Sean/__GCAM_BATCH/output/db_", scenario)
	queryFile = paste0("queries/", queryName,".xml")
	queryData = paste0("temp_data_files/", queryName,".dat")
	queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile)
	file.remove(queryData)
	queryResult[[1]][[1]] %>% filter(value < 0)
}

bind_rows(make_query("wg_lo_noCC_SSP1_to2010"),
	  make_query("wg_lo_noCC_SSP2_to2010"),
	  make_query("wg_lo_noCC_SSP3_to2010"),
	  make_query("wg_lo_noCC_SSP4_to2010"),
	  make_query("wg_lo_noCC_SSP5_to2010")) %>%
   readr::write_csv(paste0("output/", queryName, "_", name, ".csv"))