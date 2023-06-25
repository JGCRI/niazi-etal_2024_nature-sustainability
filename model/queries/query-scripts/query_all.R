library(dplyr)

name <- "all"
queryName <- "prices_water_withdrawal"


# get database names
dbs_unfiltered <- list.dirs("../output", recursive = F)
dbs <- dbs_unfiltered[grepl("db", dbs_unfiltered)] %>% substr(., 14, nchar(.))



make_query <- function(scenario){
	dbLoc <- paste0("/pic/projects/GCAM/Sean/__GCAM_BATCH/output/db_", scenario)
	queryFile = paste0("queries/", queryName,".xml")
	queryData = paste0("temp_data_files/", queryName,".dat")
	queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile)
	file.remove(queryData)
	queryResult[[1]][[1]]
}

bind_rows(lapply(dbs, make_query)) %>%
   readr::write_csv(paste0("output/", queryName, "_", name, ".csv"))
