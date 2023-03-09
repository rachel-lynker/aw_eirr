# Angus Watters
# make and save gage table data

# load libraries
library(dplyr)

source("R/utils.R")

# make gage table from utils.R function
gage_tbl <- gage_tbl()

# save gage table to data/streamflow/gages/
readr::write_csv(gage_tbl, "data/streamflow/gages/gage_table.csv")