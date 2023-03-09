# Angus Watters
# collect managment implementation plans data to compare to observed streamflow data

# load libraries
library(dplyr)
library(ggplot2)
library(cdssr)

source("R/utils.R")
source("R/get_gage_data.R")

# flow_df <- readr::read_csv("data/streamflow/observed/boatable_flows.csv")

gage_table <- gage_tbl()

# Roaring fork RICD dates and flow regime
ricd       <- rf_ricd()

# RICD rules
# 10 total days
# 1 event in May lasting (2 days per event,  580 CFS per day = 2*580 CFS = 1160 CFS over 2 days)
# 2 events in June (4 days per event, 400 CFS per day = 4*400 CFS = 1600 CFS over 4 days)

# calculate RICD Management flows and boatalbe days under this scenario
mgmt <- get_rf_ricd(df = flow_df)



