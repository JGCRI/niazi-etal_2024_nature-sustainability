library(dplyr)
name <- "ex_rs_test"

queryName <- "groundwater_production"

make_query <- function(scenario){
	dbLoc <- paste0("/pic/projects/GCAM/Sean/__GCAM_BATCH/output/db_", scenario)
	queryFile = paste0("queries/", queryName,".xml")
	queryData = paste0("temp_data_files/", queryName,".dat")
	queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile)
	file.remove(queryData)
	queryResult[[1]][[1]]
}

bind_rows(make_query("wg_md_rs_ssp3_gfdl_6p0"),
          make_query("wg_md_ex_ssp3_gfdl_6p0")) %>%
   readr::write_csv(paste0("output/", queryName, "_", name, ".csv"))