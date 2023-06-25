
library(dplyr)
library(readr)

# write out all scenario names
expand.grid(c("wg", "gl"),
            c("lo", "md", "hi"),
            c("rs", "ex"),
            paste0("SSP", 1:5),
            #c("hadg", "miro", "nrsm"),
            c("gfdl", "hadg", "ipsl", "miro", "nrsm"),
            c("2p6", "4p5", "6p0", "8p5")) %>%
  as_tibble() %>% 
  mutate(scenario = paste0(Var1, "_",
                           Var2,  "_",
                           Var3, "_",
                           Var4, "_",
                           Var5, "_",
                           Var6)) %>% 
  select(scenario) %>% 
  # remove non-feasible ssp rcp combinations
  filter(!(grepl("8p5", scenario) & grepl("SSP1", scenario)),
         !(grepl("8p5", scenario) & grepl("SSP2", scenario)),
         !(grepl("8p5", scenario) & grepl("SSP3", scenario)),
         !(grepl("8p5", scenario) & grepl("SSP4", scenario)),
         !(grepl("2p6", scenario) & grepl("SSP3", scenario))) ->
  all_scenarios

all_scenarios %>% 
  write_tsv("all_scenarios.txt", col_names = F)

read_csv("x.csv")$x %>% gsub("ssp", "SSP", .) -> scenarios_done

all_scenarios %>% 
  filter(!(scenario %in% scenarios_done)) -> scenarios_remaining

c(rbind(min.x, max.x)) 




  #write_tsv("remaining_scenarios.txt", col_names = F)





all_scenarios %>% 
  filter(grepl("_rs_", scenario)) %>% 
  .$scenario -> gcm_fix


files <- list.files("xmls/")
files_ <- unname(sapply(files, function(x) substr(x, 8, nchar(x) - 4)))



tibble(x = paste0("sbatch -J ", gcm_fix, " $RUN_SCRIPT")) %>% 
  write_tsv("second_batch_225.txt", col_names = F)



tibble(x = paste0("sbatch -J ",
                  scenarios_remaining$scenario,
                  " $RUN_SCRIPT")) -> run_lines

tibble(x = c(rbind(run_lines$x, rep("sleep 2m")))) %>% 
  write_tsv("scen_remaining_sleep.txt", col_names = F)

