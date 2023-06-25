scenario <- "GW_5surf"
queryName <- "ag_prod_basin"

dbLoc <- paste0("/pic/projects/GCAM/Sean/", scenario)

queryFile = paste0("queries/", queryName,".xml")
queryData = paste0("temp_data_files/", queryName,".dat")

queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile)
readr::write_csv(queryResult[[1]][[1]], paste0("output/", queryName, "_", scenario, ".csv"))
file.remove(queryData)
