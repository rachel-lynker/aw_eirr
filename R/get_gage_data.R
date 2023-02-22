# Angus Watters
# Get Observed streamflows data

# load libraries
library(cdssr)
library(dplyr)

source("R/utils.R")

# save output path
save_path <- "data/streamflow/observed/discharge.csv"

# check if streamflow discharge daily data already exists, otherwise get data
if(file.exists(save_path)) {
  
  message(paste0("Reading data from:\n---> ", save_path))
  
  flow_df <- readr::read_csv(save_path)
  
} else {
  
  # table of gages
  gage_df <- 
    gage_tbl()  %>% 
    dplyr::distinct(river, gage_id, gage_name, source) 
  
  # rm(i, flow_df)
  flow_df <- lapply(1:nrow(gage_df), function(i) {
  
    message(paste0(
      i, "/", nrow(gage_df),
      "\nRiver: ",  gage_df$river[i], 
      "\nUSGS ID: ",  gage_df$gage_id[i],
      "\nAbbrev: ",  gage_df$gage_name[i],
      "\n--------------------------------------------------"
    ))
    
    
    if(gage_df$source[i] == "USGS") {
      
      sw <- cdssr::get_sw_ts(
        usgs_id     = gage_df$gage_id[i],  
        start_date  = '1980-01-01',
        end_date    = Sys.Date()
      )
      
      } else {
        
        sw <- cdssr::get_sw_ts(
          abbrev      = gage_df$gage_name[i],  
          start_date  = '1980-01-01',
          end_date    = Sys.Date()
        )
        
      }
    
    sw
      
  
  }) %>% 
    dplyr::bind_rows()
  
  readr::write_csv(flow_df, save_path)
  
}


