# Angus Watters
# Get Observed streamflows data

# load libraries
library(dataRetrieval)
library(dplyr)

source("R/utils.R")

# table of gages
gage_df <- 
  gage_tbl()  %>% 
  dplyr::distinct(river, gage_id) %>% 
  na.omit()
gage_df

# loop over USGS gage IDs and pull together data
flows_df <- lapply(1:nrow(gage_df), function(i) {

  message(paste0(
     i, "/", nrow(gage_df),
     "\nRiver: ",  gage_df$river[i], 
     "\nReach: ",  gage_df$reach[i],
     "\nGage ID: ",  gage_df$gage_id[i],
     "\n--------------------------------------------------"
     ))
  
  tryCatch({
    
    df <- dataRetrieval::readNWISdv(
                      siteNumbers = gage_df$gage_id[i],
                      parameterCd = '00060',
                      startDate   = '1980-01-01',
                      endDate     = Sys.Date()
                    )
    df
    
  }, error = function(e)
    
    NULL
  )
  
}) %>% 
  dplyr::bind_rows()


