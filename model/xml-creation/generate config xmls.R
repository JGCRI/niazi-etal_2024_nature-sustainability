
library(readr)
library(dplyr)

# select ex or rs
calib = "ex"

# BASE LEVEL: start with all five SSP files
# ... gw = md, watergap; gcm = gfdl; rcp = 4p5
base_files <- list.files("xmls/base xmls/")
base_files <- base_files[grepl(calib, base_files)]

# expand out SSPs for all three gw scenarios
# ... i.e., add lo and hi
base_files_add_gw <- function(...){
  
  f <- read_lines(paste0("xmls/base xmls/", ...))
  
  # md
  f %>% 
    write_lines(
      paste0("xmls/level_1/", ...)
    )
  
  # lo
  f %>% 
    gsub("_md", "_lo", .) %>% 
    write_lines(
      gsub("_md_","_lo_",
           paste0("xmls/level_1/", ...))
    )
    
    # hi
  f %>%
    gsub("_md", "_hi", .) %>% 
    write_lines(
      gsub("_md_","_hi_",
           paste0("xmls/level_1/", ...))
      )
}

# create all SSP and gw combinations
sapply(base_files, base_files_add_gw)


# LEVEL 1: take 15 ssp gw combinations and add gcms
l1_files <- list.files("xmls/level_1/")
l1_files_add_gcms <- function(...){
  
  f <- read_lines(paste0("xmls/level_1/", ...))
  
  # gfdl
  f %>% 
    write_lines(
      paste0("xmls/level_2/", ...)
    )
  
  # hadg
  f %>% 
    gsub("water_cc_gfdl", "water_cc_hadgem", .) %>%
    gsub("hydro_gfdl-esm2m", "hydro_hadgem2-es", .) %>% 
    gsub("gfdl_pdssat", "hadgem2_pdssat", .) %>% 
    gsub("_gfdl_", "_hadg_", .) %>% 
    write_lines(
      gsub("_gfdl_","_hadg_",
           paste0("xmls/level_2/", ...))
    )
  
  # ipsl
  f %>% 
    gsub("water_cc_gfdl", "water_cc_ipsl", .) %>%
    gsub("hydro_gfdl-esm2m", "hydro_ipsl-cm5a-lr", .) %>% 
    gsub("gfdl_pdssat", "ipsl_pdssat", .) %>% 
    gsub("_gfdl_", "_ipsl_", .) %>% 
    write_lines(
      gsub("_gfdl_","_ipsl_",
           paste0("xmls/level_2/", ...))
    )
  
  # miro
  f %>% 
    gsub("water_cc_gfdl", "water_cc_miroc", .) %>%
    gsub("hydro_gfdl-esm2m", "hydro_miroc-esm-chem", .) %>% 
    gsub("gfdl_pdssat", "miroc_pdssat", .) %>% 
    gsub("_gfdl_", "_miro_", .) %>% 
    write_lines(
      gsub("_gfdl_","_miro_",
           paste0("xmls/level_2/", ...))
    )
  
  # nrsm
  f %>% 
    gsub("water_cc_gfdl", "water_cc_noresm", .) %>%
    gsub("hydro_gfdl-esm2m", "hydro_noresm1-m", .) %>% 
    gsub("gfdl_pdssat", "noresm1_pdssat", .) %>% 
    gsub("_gfdl_", "_nrsm_", .) %>% 
    write_lines(
      gsub("_gfdl_","_nrsm_",
           paste0("xmls/level_2/", ...))
    )

  
}
sapply(l1_files, l1_files_add_gcms)

# LEVEL 2: take 75 ssp/gw/gcm combos and add rcps
l2_files <- list.files("xmls/level_2/")
l2_files_add_rcps <- function(...){
  
  f <- read_lines(paste0("xmls/level_2/", ...))
  
  # rcp4p5
  f %>% 
    write_lines(
      paste0("xmls/level_3/", ...)
    )
  
  # rcp8p5
  f %>% 
    gsub("4p5", "8p5", .) %>% 
    gsub("path\">1<", "path\">0<", .) %>% 
    write_lines(
      gsub("_4p5","_8p5",
           paste0("xmls/level_3/", ...))
    )
  
  # rcp6p0
  f %>% 
    gsub("4p5", "6p0", .) %>% 
    write_lines(
      gsub("_4p5","_6p0",
           paste0("xmls/level_3/", ...))
    )
  
  # rcp2p6
  f %>% 
    gsub("4p5", "2p6", .) %>% 
    write_lines(
      gsub("_4p5","_2p6",
           paste0("xmls/level_3/", ...))
    )
}
sapply(l2_files, l2_files_add_rcps)

# LEVEL 3: copy all files for _gl_
l3_files <- list.files("xmls/level_3/")
l3_files_add_gl <- function(...){
  
  f <- read_lines(paste0("xmls/level_3/", ...))
  
  # wg
  f %>% 
    write_lines(
      paste0("xmls/level_4/", ...)
    )
  
  # gl
  f %>% 
    gsub("wg_", "gl_", .) %>% 
    gsub("wg.xml", "gl.xml", .) %>% 
    write_lines(
      gsub("wg_","gl_",
           paste0("xmls/level_4/", ...))
    )
  
  
}
sapply(l3_files, l3_files_add_gl)

# Select desired files from L4
l4_files <- tibble(f = list.files("xmls/level_4/"))

l4_files %>%
  filter(!(grepl("8p5", f) & grepl("SSP1", f)),
         !(grepl("8p5", f) & grepl("SSP2", f)),
         !(grepl("8p5", f) & grepl("SSP3", f)),
         !(grepl("8p5", f) & grepl("SSP4", f)),
         !(grepl("2p6", f) & grepl("SSP3", f))) %>%
  .$f -> filtered_file_names

cp_xml <- function(...){
  file.copy(paste0("xmls/level_4/", ...),
            "xmls/level_final")
}

sapply(filtered_file_names, cp_xml)

# clean up level folders
sapply(paste0("xmls/level_1/", l1_files), file.remove)
sapply(paste0("xmls/level_2/", l2_files), file.remove)
sapply(paste0("xmls/level_3/", l3_files), file.remove)
sapply(paste0("xmls/level_4/", l4_files %>% .$f), file.remove)


# correct 6p0 ssp2,3,5
list.files("xmls/level_final/") %>% 
  as_tibble() %>% 
  filter(grepl("6p0", value)) %>% 
  filter(grepl("SSP2", value) | grepl("SSP3", value) | grepl("SSP5", value)) %>%
  .$value -> fix_files

fix_files_6p0 <- function(f){
  
  read_lines(paste0("xmls/level_final/", f)) %>% 
    gsub("target_6p0_spa23.xml", "target_6p0_spa235.xml", .) %>% 
    gsub("target_6p0_spa5.xml", "target_6p0_spa235.xml", .) %>% 
    write_lines(paste0("xmls/level_final/", f))

}

sapply(fix_files, fix_files_6p0)
