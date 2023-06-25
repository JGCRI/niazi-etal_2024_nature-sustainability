scenario = "min_hadgem"
queryName <- "CO2_prices"

dbLoc <- "/people/chen671/GCAM/gcam-core/output/ssp_rcp_hadgem"


queryFile = paste0("queries/", queryName,".xml")
queryData = paste0("temp_data_files/", queryName,".dat")

queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp1rcp60")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp1rcp45")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp5rcp85")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp3rcp60")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp2rcp45")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp1rcp26")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp2rcp60")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp3rcp45")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp2rcp26")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp5rcp60")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp5rcp26")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp4rcp45")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp5rcp45")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp4rcp60")
queryResult <- rgcam::addScenario(dbLoc, queryData, queryFile = queryFile, scenario = "ssp4rcp26")

all_scenarios <- dplyr::bind_rows(queryResult[[1]][[1]],
				  queryResult[[2]][[1]],
				  queryResult[[3]][[1]],
				  queryResult[[4]][[1]],
				  queryResult[[5]][[1]],
				  queryResult[[6]][[1]],
				  queryResult[[7]][[1]],
				  queryResult[[8]][[1]],
				  queryResult[[9]][[1]],
				  queryResult[[10]][[1]],
				  queryResult[[11]][[1]],
				  queryResult[[12]][[1]],
				  queryResult[[13]][[1]],
				  queryResult[[14]][[1]],
				  queryResult[[15]][[1]])


readr::write_csv(all_scenarios, paste0("output/", queryName, "_", scenario, ".csv"))
file.remove(queryData)
