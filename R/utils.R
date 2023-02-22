#' Summary table of Gage info and flow threshold values
#'
#' @return dataframe containing relevent river reaches, associated USGS IDs and other information
#' @export
#'
#' @examples
gage_tbl <- function() {
  
  dplyr::tibble(
   river = c(
            "Roaring Fork",
            "Roaring Fork",
            "Roaring Fork",
            "Roaring Fork",
            "Roaring Fork",
            "Yampa",
            "Yampa",
            "Yampa",
            "Yampa",
            "Yampa",
            "Cache La Poudre",
            "Cache La Poudre",
            "Cache La Poudre"
            ),
   reach = c(
     "Black Bridge to Veltus Park (Cemetery)", "Basalt to Carbondale",
     "Lower Woody Creek Bridge to Rte. 82 Bridge", "Slaughterhouse",  "Weller Lake to Difficult CG",
     'Yampa River Park to Transit Center', "Transit Center to Pump Station",
     'Little Yampa Canyon', "Cross Mountain Gorge - 85 Rd to Deer Lodge Park Rd", 'Deerlodge Park to Echo Park',
     "Filter Plant",  "Poudre Whitewater Park", "Big South"
     ),
   abbrev = c(
     "ROAGLECO", "ROAEMMCO", NA, NA, "ROADIFCO",
     "YAMSTECO", "YAMELKCO", "YAMCRACO", "YAMAYBCO",
     "YAMDEECO", "CLAFTCCO", "CLAFORCO", "LAPLODCO"
   ),
  desc = c(
    "Roaring Fork River at Glenwood Springs, CO", "Roaring Fork near Emma, CO",
    "Roaring Fork River Blw Maroon Creek Nr Aspen, CO", "Roaring Fork River Blw Maroon Creek Nr Aspen, CO", 
    "Roaring Fork River Ab Difficult Cr Nr Aspen, CO",
    "Yampa River at Steamboat Springs, CO", "Yampa River above Elkhead Creek near Hayden, CO", 
    "Yampa River below Craig, CO", "Yampa River near Maybell, CO",  "Yampa River at Deerlodge Park, CO",
    "CLAFTCCO", "CLAFORCO", "LAPLODCO"
  ),
  gage_id = c(
              "09085000",
              "09081000",
              "09076300",
              "09076300",
              "09073300",
              "09239500",
              "09244490",
              "09247600",
              "09251000",
              "09260050",
              "06752000",
              "06752260", 
              NA
              ),
  min_threshold = c(
    200, 200, 200, 200,100,
    700, 500, 1100, 700,1300,
    522.7273, 354.797, 170.1005
    ),
  max_threshold = c(
    10000,1500, 1400, 2700,1000,
    5000, 5000, 10000, 5000, 25000,
    1000000, 1000000, 1000000
  ),
  source = c(
    "USGS","USGS", "USGS", "USGS", "USGS",
    "USGS", "USGS","USGS", "USGS", "USGS",
    "USGS", "USGS", "CDSS"
  )
  ) %>% 
    dplyr::mutate(
      uid = dplyr::case_when(
        is.na(abbrev)  ~ gage_id,
        is.na(gage_id) ~ abbrev,
        TRUE             ~ abbrev
      )
    ) %>% 
    dplyr::relocate(river, reach, abbrev, gage_id, uid, desc)
  
}


#' Add boatable days column to streamflow dataframe 
#'
#' @param df dataframe with a "value" column representing streamflow in CFS 
#' @param threshold numeric vector of 2 values, a minimum and maximum flow thresholds. The minimum value will represent the minimum flow threshold and the maximum value will represent the maximum flow threshold. For example, c(100, 2000) and c(2000, 100) will result in the same maximum/minimum flow threshold values. If no value is given, min and max threshold values are derived from gage_tbl() function dataframe. Default is NULL.
#'
#' @return streamflow dataframe with an added column indicating whether that day was boatable (1) or not boatable (0)
#' @export
get_boatable_days <- function(
    df        = NULL,
    threshold = NULL
    ) {

  message(paste0("calculating boatable days..."))
  
  # if no threshold vector is given, get threshold values from gage_tbl() function
  if(is.null(threshold)) {
    
    min_threshold <- gage_tbl()[gage_tbl()$uid == df$uid[1],]$min_threshold
    max_threshold <- gage_tbl()[gage_tbl()$uid == df$uid[1],]$max_threshold
    
  # if threshold vector is given, use min value as lower bound and max value as upper bound
  } else {
    
    min_threshold <- min(threshold)
    max_threshold <- max(threshold)
    
  }
  
  # add boatable days tag
  df$boatable_days <- ifelse(df$value >= min_threshold & df$value <= max_threshold, 1, 0)
  
  return(df)
  
}

get_flows <- function(
  gage_table = NULL,
  start_date = "1980-01-01",
  end_date   = Sys.Date()
  ) {
  
  # table of gages
  if(is.null(gage_table)) {
    
    gage_table <- gage_tbl() 
  }

  # loop over each row of the gage dataframe
  flow_df <- lapply(1:nrow(gage_table), function(i) {
    
    message(paste0(
      i, "/", nrow(gage_table),
      "\nRiver: ",  gage_table$river[i], 
      "\nUSGS ID: ",  gage_table$gage_id[i],
      "\nAbbrev: ",  gage_table$abbrev[i]
    ))
    
    # get surface water data 
    sw <- cdssr::get_sw_ts(
      abbrev      = if(is.na(gage_table$abbrev[i])) { NULL } else { gage_table$abbrev[i] },
      usgs_id     = if(is.na(gage_table$gage_id[i])) { NULL } else { gage_table$gage_id[i] },
      start_date  = start_date,
      end_date    = end_date
    )
    
    # add unique identifer column
    sw$uid <- gage_table$uid[i]
    
    # add boatable days column 
    sw <- get_boatable_days(sw)
    
    message(paste0(
      "--------------------------------------------------"
    ))
    
  }) %>% 
    dplyr::bind_rows()
}
