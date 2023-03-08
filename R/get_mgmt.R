# Angus Watters
# collect managment implementation plans data to compare to observed streamflow data

# load libraries
library(dplyr)
library(ggplot2)
library(cdssr)

source("R/utils.R")

flow_df <- readr::read_csv("data/streamflow/observed/boatable_flows.csv")

gage_tbl <- gage_tbl()

rf_ricd()

get_rf_ricd <- function(df) {
  
  flow_df %>% 
    dplyr::left_join(
      dplyr::select(gage_tbl, uid, river),
      by = "uid"
      ) %>% 
    dplyr::filter(river == "Roaring Fork")
  
}
