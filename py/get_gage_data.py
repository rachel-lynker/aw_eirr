import cdsspy
import datetime
import pandas as pd
import numpy as np
from py import utils

# get gage table
# gage_tbl = pd.read_csv("data/streamflow/gages/gage_table.csv",  dtype=str)

# get flow data
flow_df = utils.get_flows(
  gage_table = utils.gage_tbl(),
  start_date = "1980-01-01",
  end_date   = datetime.date.today().strftime("%Y-%m-%d")
  )

# df.loc[df["value"] >= min_threshold & df["value"] <= max_threshold, "value"]

# get_boatable_days <- function(
#     df        = NULL,
#     threshold = NULL
#     ) {
# 
#   message(paste0("calculating boatable days..."))
#   
#   # if no threshold vector is given, get threshold values from gage_tbl() function
#   if(is.null(threshold)) {
#     
#     min_threshold <- gage_tbl()[gage_tbl()$uid == df$uid[1],]$min_threshold
#     max_threshold <- gage_tbl()[gage_tbl()$uid == df$uid[1],]$max_threshold
#     
#   # if threshold vector is given, use min value as lower bound and max value as upper bound
#   } else {
#     
#     min_threshold <- min(threshold)
#     max_threshold <- max(threshold)
#     
#   }
#   
#   # add boatable days tag
#   df$boatable_days <- ifelse(df$value >= min_threshold & df$value <= max_threshold, 1, 0)
#   
#   return(df)
#   
# }



