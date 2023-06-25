library(dplyr)

name <- "hadg"
queryName <- "ag_prod_basin"


# get database names
dbs_unfiltered <- list.dirs("../output", recursive = F)
dbs <- dbs_unfiltered[grepl("db", dbs_unfiltered)] %>% substr(., 14, nchar(.))
dbs <- dbs[grepl("hadg", dbs)]



make_query <- function(scenario){
	dbLoc <- paste0("/pic/projects/GCAM/Sean/__GCAM_BATCH/output/db_", scenario)
	queryFile = paste0("queries/", queryName,".xml")
	queryData = paste0("temp_data_files/", queryName,"_hadg.dat")
	queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile)
	file.remove(queryData)

	queryResult[[1]][[1]] %>%
		filter(year == 2010, Units == "Mt") %>%
		mutate(tech = case_when(
			grepl("IRR", technology) ~ "IRR",
			grepl("RFD", technology) ~"RFD")) %>%
		filter(is.na(tech) == F) %>%
		mutate(subsector = if_else(grepl("Root_Tuber", subsector), paste0("RootTuber_", substr(subsector, 12, nchar(subsector))), subsector)) %>%
		tidyr::separate(subsector, "_", into = c("crop", "GLU_name")) %>%
		group_by(scenario, GLU_name, tech) %>%
		summarise(Production_Mt = sum(value)) %>% ungroup()

}

bind_rows(lapply(dbs, make_query)) %>%
   readr::write_csv(paste0("output/", queryName, "_", name, ".csv"))