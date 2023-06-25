library(dplyr)

scenario <- "watergap_low_noCC_SSP5_profit_test"
queryName <- "profit_rate"

dbLoc <- paste0("/pic/projects/GCAM/Sean/__GCAM_BATCH/output/db_", scenario)

queryFile = paste0("queries/", queryName,".xml")
queryData = paste0("temp_data_files/", queryName,".dat")

queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile)
readr::write_csv(queryResult[[1]][[1]] %>% filter(value < 0), paste0("output/", queryName, "_", scenario, ".csv"))
file.remove(queryData)