# Angus Watters
# Get Observed streamflows data + boatable days

# load libraries
library(cdssr)
library(dplyr)

source("R/utils.R")

# save output path
save_path <- "data/streamflow/observed/boatable_flows.csv"

# check if streamflow discharge daily data already exists, otherwise get data
if(file.exists(save_path)) {
  
  message(paste0("Reading data from:\n---> ", save_path))
  
  flow_df <- readr::read_csv(save_path)
  
} else {
  
  flow_df <- get_flows(
                  gage_table = gage_tbl(),
                  start_date = "1980-01-01",
                  end_date   = Sys.Date()
                  )
  
  # save out CSV
  readr::write_csv(flow_df, save_path)
  
}


