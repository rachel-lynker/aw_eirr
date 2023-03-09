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
  
  # table with gage info and thresholds 
  gage_table <-  gage_tbl()
  
  # get flows data w/ boatable days calculated
  flow_df <- get_flows(
                  gage_table = gage_table,
                  start_date = "1980-01-01",
                  end_date   = Sys.Date()
                  )

  # save out CSV
  readr::write_csv(flow_df, save_path)
  
} 

# plot monthly total boatable days at each location
# flow_df %>%
# dplyr::mutate(
#   year = lubridate::year(datetime),
#   month = lubridate::month(datetime)
# ) %>%
#   dplyr::group_by(uid, year, month) %>%
#   dplyr::summarise(
#     tot_bd = sum(boatable_days, na.rm = T)
#   ) %>%
#   dplyr::mutate(
#     date = as.Date(paste0(year, "-", month, "-01"))
#   ) %>%
#   ggplot2::ggplot() +
#   ggplot2::geom_col(ggplot2::aes(x = date, y = tot_bd)) +
#   ggplot2::facet_wrap(~uid, scales = "fixed")
