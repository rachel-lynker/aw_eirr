# library(dplyr)
# df <- gage_tbl()
# readr::write_csv(df, "C:/Users/angus/OneDrive/Desktop/github/leetcode/gage_tbl.csv")

#' Summary table of Gage info and flow threshold values
#'
#' @return dataframe containing relevant river reaches, associated USGS IDs and other information
#' @export
#'
#' @examples
gage_tbl <- function() {
  
  dplyr::tibble(
   river = c(
            "Roaring Fork",
            "Roaring Fork",
            "Roaring Fork",
            # "Roaring Fork",
            # "Roaring Fork",
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
     "Lower Woody Creek Bridge to Rte. 82 Bridge/Slaughterhouse", 
     # "Lower Woody Creek Bridge to Rte. 82 Bridge", "Slaughterhouse",  
     "Weller Lake to Difficult CG",
     'Yampa River Park to Transit Center', "Transit Center to Pump Station",
     'Little Yampa Canyon', "Cross Mountain Gorge - 85 Rd to Deer Lodge Park Rd", 'Deerlodge Park to Echo Park',
     "Filter Plant",  "Poudre Whitewater Park", "Big South"
     ),
   abbrev = c(
     "ROAGLECO", "ROAEMMCO", 
     NA, # NA, NA, 
     "ROADIFCO",
     "YAMSTECO", "YAMELKCO", "YAMCRACO", "YAMAYBCO",
     "YAMDEECO", "CLAFTCCO", "CLAFORCO", "LAPLODCO"
   ),
  desc = c(
    "Roaring Fork River at Glenwood Springs, CO", "Roaring Fork near Emma, CO",
    "Roaring Fork River Blw Maroon Creek Nr Aspen, CO", 
    # "Roaring Fork River Blw Maroon Creek Nr Aspen, CO",
    # "Roaring Fork River Blw Maroon Creek Nr Aspen, CO", 
    "Roaring Fork River Ab Difficult Cr Nr Aspen, CO",
    "Yampa River at Steamboat Springs, CO", "Yampa River above Elkhead Creek near Hayden, CO", 
    "Yampa River below Craig, CO", "Yampa River near Maybell, CO",  "Yampa River at Deerlodge Park, CO",
    "CLAFTCCO", "CLAFORCO", "LAPLODCO"
  ),
  gage_id = c(
              "09085000",
              "09081000",
              "09076300",
              # "09076300",
              # "09076300",
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
    200, 200, 
    200, # 200, 200,
    100,
    700, 500, 1100, 700,1300,
    522.7273, 354.797, 170.1005
    ),
  max_threshold = c(
    10000,1500,
    2050, # 1400, 2700, # took the mean of these 2 values
    1000,
    5000, 5000, 10000, 5000, 25000,
    1000000, 1000000, 1000000
  ),
  source = c(
    "USGS","USGS", 
    "USGS", # "USGS", "USGS",
    "USGS",
    "USGS", "USGS","USGS", "USGS", "USGS",
    "USGS", "USGS", "CDSS"
  )
  ) %>% 
    dplyr::mutate(
      uid = dplyr::case_when(
        is.na(abbrev)  ~ gage_id,
        is.na(gage_id) ~ abbrev,
        TRUE           ~ abbrev
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
    threshold = NULL,
    flow_col  = "flow",
    boat_col  = "boat_obs"
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
  
  # flow_col = "flow"
  # as.vector(df[, flow_col])
  # class(as.numeric(as.vector(df[, flow_col])))
  # as.numeric((unlist(as.vector(df[, flow_col]))))
  # df$flow
  # class(df$flow)
  
  # add boatable days tag
  df[[boat_col]] <- ifelse(
                          as.numeric((unlist(as.vector(df[, flow_col])))) >= min_threshold &
                            as.numeric((unlist(as.vector(df[, flow_col])))) <= max_threshold, 
                          1, 
                          0
                        )
  # df$boatable_days <- ifelse(
  #                         as.numeric((unlist(as.vector(df[, flow_col])))) >= min_threshold &
  #                           as.numeric((unlist(as.vector(df[, flow_col])))) <= max_threshold, 
  #                           1, 
  #                           0
  #                         )
  # df$boatable_days <- ifelse(df$flow >= min_threshold & df$flow <= max_threshold, 1, 0)
  
  return(df)
  
}

get_flows <- function(
  gage_table = NULL,
  start_date = "1980-01-01",
  end_date   = Sys.Date(),
  api_key    = NULL
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
      end_date    = end_date,
      api_key     = api_key
    ) %>% 
      dplyr::select(station_num, abbrev, usgs_site_id, datetime, flow = value)
    
    # add unique identifer column
    sw$uid <- gage_table$uid[i]
    
    # reorder columns
    sw <- 
      sw %>% 
      dplyr::left_join(
        dplyr::select(gage_table, uid, river),
        by = "uid"
      ) %>% 
      dplyr::relocate(river, uid, station_num, abbrev, usgs_site_id, datetime, flow)

    # add boatable days column 
    sw <- get_boatable_days(
                          sw, 
                          flow_col = "flow",
                          boat_col = "boat_obs"
                        )
    
    message(paste0(
      "--------------------------------------------------"
    ))
    
    sw
    
  }) %>% 
    dplyr::bind_rows()
}

# Roaring fork RICD CFS management plan flow/date table
rf_ricd <- function() {
  
  data.frame(
    order      = c(1, 2, 3, 4, 5, 6, 7),
    start_mon  = c("mar", "apr", "may", "may", "july", "august", "nov"),
    end_mon    = c("apr", "apr", "may", "july", "july", "oct", "nov"),
    start_day  = c(15, 15, 1, 15, 15, 1, 1),
    end_day    = c(14, 30, 14, 14, 31, 31, 30),
    start_date = c("03-15", "04-15", "05-01", "05-15", "07-15", "08-01", "11-01"),
    end_date   = c("04-14", "04-30", "05-14", "07-14", "07-31", "10-31", "11-30"),
    flow_rate  = c(230, 310, 575, 1000, 575, 310, 230)
  )
}

# RICD rules
# 10 total days
# 1 event in May lasting (2 days per event,  580 CFS per day = 2*580 CFS = 1160 CFS over 2 days)
# 2 events in June (4 days per event, 400 CFS per day = 4*400 CFS = 1600 CFS over 4 days)

# calculate RICD Management flows and boatalbe days under this scenario
get_rf_ricd <- function(df) {
  
  rf <-
    df %>% 
    dplyr::filter(river == "Roaring Fork") %>% 
    dplyr::mutate(
      year  = lubridate::year(datetime),
      month = tolower(lubridate::month(datetime, label = T)),
      day   = lubridate::day(datetime)
    ) %>% 
    dplyr::group_by(usgs_site_id, year) %>% 
    dplyr::mutate(
      mgmt_flow = dplyr::case_when(
        month == "mar" & day >= 15        ~ 230,
        month == "apr" & day <= 14        ~ 230,
        month == "apr" & day >= 15        ~ 310,
        month == "may" & day <= 14        ~ 575,
        month == "may" & day >= 15        ~ 1000,
        month == "june"                   ~ 1000,
        month == "jul" & day <= 14        ~ 1000,
        month == "jul" & day >= 15        ~ 575,
        month %in% c("aug", "sep", "oct") ~ 310,
        month == "nov"                    ~ 230,
        TRUE                              ~ NA_real_
      ),
      is_mgmt = dplyr::case_when(
        !is.na(mgmt_flow)  ~ TRUE,
        TRUE               ~ FALSE
      ),
      mgmt_flow = dplyr::case_when(
        is.na(mgmt_flow) ~ flow,
        TRUE              ~ mgmt_flow
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-year, -month, -day) %>% 
    get_boatable_days(
      flow_col = "mgmt_flow",
      boat_col = "boat_mgmt"
    )
  
  return(rf)
  
  
}

# impute missing values w/ mean
impute_mean <- function(x) {
  replace(x, is.na(x), mean(x, na.rm = TRUE))
}

# convert AF to CFS
convert_af_to_cfs <- function(af) {
  cfs <- (af)*0.5042864
  return(cfs)
}
# Summarize monthly flow data at a site
flow_summary <- function(data, timescale = "monthly") {
  
  if(timescale == "monthly") {
    
    # Calculate initial monthly flow (initial_flow) & change in flow from prior month (dflow)
    flows_month_ts <- 
      data %>% 
      dplyr::mutate(
        year  = lubridate::year(date),
        month = lubridate::month(date),
        day   = lubridate::day(date)
      ) %>% 
      dplyr::mutate(
        date_ym = as.Date(paste0(year, "-", month, "-01")),
      ) %>% 
      # filter(year %in% c(2014, 2015)) %>%
      dplyr::group_by(site, date_ym) %>%
      dplyr::mutate(
        avg_flow     = mean(flow, na.rm = T),
        total_flow   = sum(flow, na.rm = T)
      ) %>% 
      # dplyr::ungroup() %>% 
      dplyr::slice(
        which.min(day),
        which.max(day)
      ) %>%
      dplyr::arrange(site, date) %>% 
      dplyr::mutate(
        days_in_month  = days_in_month(date)
      ) %>% 
      dplyr::ungroup()
    
    # Start of month flows
    month_initial_flow <- 
      flows_month_ts %>% 
      dplyr::group_by(site, date_ym) %>% 
      dplyr::slice(which.min(date)) %>% 
      dplyr::ungroup() %>%
      dplyr::select(site, year, month_start_date = date,  date_ym,
                    days_in_month, initial_flow = flow) 
    
    # End of month flows
    month_end_flow <- 
      flows_month_ts %>% 
      dplyr::group_by(site, date_ym) %>% 
      dplyr::slice(which.max(date)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        nat_end_flow = flow + diversion - release
      ) %>%
      dplyr::select(site, year, month_end_date = date, 
                    date_ym, end_flow = flow, nat_end_flow, diversion, release)
    
    # % of normal initial flows
    normal_flow <- 
      flows_month_ts %>% 
      dplyr::group_by(site, date_ym) %>% 
      dplyr::slice(which.min(date)) %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(site, month) %>% 
      dplyr::mutate(avg_initial_flow = mean(flow, na.rm = T)) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(site, date_ym) %>% 
      dplyr::mutate(initial_flow_normal = flow/avg_initial_flow) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(site, year, date_ym, initial_flow_normal) 
    
    # Replace NA/ Inf w/ Zeros
    is.na(normal_flow) <- sapply(normal_flow, is.infinite)
    normal_flow[is.na(normal_flow)] <- 1.0
    
    # Join Start and end of week flows
    flow_differences <-  
      month_initial_flow %>% 
      dplyr::left_join(
        month_end_flow,
        by = c("site", "year", "date_ym")
      ) %>% 
      dplyr::left_join(
        normal_flow,
        by = c("site", "year", "date_ym")
      ) %>% 
      dplyr::relocate(
        site, year, month_start_date, month_end_date, date_ym, days_in_month,
        initial_flow, end_flow, nat_end_flow, diversion, release
      ) %>% 
      dplyr::mutate(
        dflow         = (end_flow - initial_flow)/days_in_month,
        nat_dflow     = (nat_end_flow - initial_flow)/days_in_month
      ) %>%  
      dplyr::relocate(
        site, year, month_start_date, month_end_date, date_ym, days_in_month,
        initial_flow, end_flow, nat_end_flow, dflow, nat_dflow, initial_flow_normal, diversion, release
      )
    
    # Total flow in week and change in flow over week 
    flow_month_summary <- 
      flows_month_ts %>% 
      dplyr::select(site, date, date_ym, year, avg_flow, total_flow) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(site, date_ym) %>% 
      dplyr::summarize(
        avg_flow   = mean(avg_flow, na.rm = T),
        total_flow = mean(total_flow, na.rm = T)
      ) %>% 
      dplyr::left_join(
        flow_differences, 
        by = c("site", "date_ym")
      ) %>% 
      dplyr::mutate(dplyr::across(where(is.numeric), round, 4)) %>% 
      dplyr::mutate(
        month = lubridate::month(date_ym, label = T),
        season = dplyr::case_when(
          month %in% c("Dec", "Jan", "Feb") ~ "winter",
          month %in% c("Mar", "Apr", "May") ~ "spring",
          month %in% c("Jun", "Jul", "Aug") ~ "summer",
          month %in% c("Sep", "Oct", "Nov") ~ "fall"
        ),
        season = factor(season)
      ) %>%
      dplyr::select(
        site, date = date_ym, month_start_date, month_end_date, season, 
        initial_flow, end_flow, nat_end_flow, dflow, nat_dflow, initial_flow_normal, 
        total_flow, avg_flow, diversion, release) %>% 
      dplyr::ungroup()
    
    # Replace NA/ Inf w/ Zeros
    is.na(flow_month_summary) <- sapply(flow_month_summary, is.infinite)
    flow_month_summary[is.na(flow_month_summary)] <- 0
    
    return(flow_month_summary)
    
  } else if(timescale == "bimonthly") {
    
    # Calculate weekly flows (initial_flow) & change in flow from prior week (dflow)
    flows_bimonth_ts <-  
      data %>% 
      # filter(segment == "Poudre Park") %>%
      mutate(
        year      = lubridate::year(date),
        month     = lubridate::month(date),
        week      = lubridate:::week(date),
        day_num   = lubridate::day(date)
      ) %>% 
      dplyr::mutate(
        date_ym = as.Date(paste0(year, "-", month, "-01")),
      ) %>% 
      dplyr::group_by(year) %>% 
      dplyr::mutate(
        year_days = dplyr::n(),
        bimonth   = rep(1:dplyr::n(), each = 14, length.out = dplyr::n())
      ) 
    # %>%
    # dplyr::filter(year %in% c(2015, 2016))
    
    # Create grouping every 14 days weeks (2 weeks)
    # flows_bimonth_ts$bimonth <- rep(1:nrow(flows_bimonth_ts), each = 14, length.out = nrow(flows_bimonth_ts))
    # 
    flows_bimonth_ts <- 
      flows_bimonth_ts %>% 
      dplyr::group_by(site, year, bimonth) %>%
      dplyr::mutate(
        total_flow      = sum(flow, na.rm = T),
        avg_flow        = mean(flow, na.rm = T),
        days_in_bimonth = dplyr::n()
      ) %>%
      dplyr::group_by(site, year, bimonth) %>%
      # group_by(segment, date_ym) %>%
      dplyr::slice(
        which.min(date),
        which.max(date)
      ) %>%
      dplyr::arrange(site, date) %>%
      dplyr::mutate(
        days_in_month  = lubridate::days_in_month(date),
        flow_diff      = diff(flow)
        # between0       = as.numeric(difftime(date, lag(date,1))), 
        # days_in_week        = ifelse(is.na(between0), 0, between0),
      ) %>%
      dplyr::relocate(site, site, date, date_ym, day_num, week, bimonth,
                      days_in_bimonth, days_in_month, flow, flow_diff, total_flow) %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(site) %>%
      dplyr::arrange(site, date, .by_group	= T) %>%
      dplyr::mutate(    
        dflow            = flow_diff/days_in_bimonth
      ) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(site, site, date, date_ym, day_num, week, bimonth, days_in_bimonth, 
                      days_in_month, flow, flow_diff, total_flow, dflow, diversion, release) 
    
    
    # Start of bimonth flows
    bimonth_initial_flow <- 
      flows_bimonth_ts %>% 
      dplyr::group_by(site, year, bimonth) %>% 
      dplyr::slice(which.min(date)) %>% 
      dplyr::ungroup() %>%
      dplyr::select(site, year, week_start_date = date, diversion, release,
                    day_num, week, bimonth, days_in_bimonth, initial_flow = flow) 
    
    # End of bimonth flows
    bimonth_end_flow <- 
      flows_bimonth_ts %>% 
      dplyr::group_by(site, year, bimonth) %>% 
      dplyr::slice(which.max(date)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        nat_end_flow = flow + diversion - release
      ) %>% 
      dplyr::select(site, year, week_end_date = date,
                    bimonth,  end_flow = flow, nat_end_flow)
    # diversion, release)
    
    # % of normal initial flows
    normal_flow <-
      flows_bimonth_ts %>%
      dplyr::group_by(site, year, bimonth) %>%
      dplyr::slice(which.min(date)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(site, bimonth) %>%
      dplyr::mutate(avg_initial_flow = mean(flow, na.rm = T)) %>%
      dplyr::ungroup() %>%
      # dplyr::group_by(site, date_ym) %>%
      dplyr::mutate(initial_flow_normal = flow/avg_initial_flow) %>%
      dplyr::select(site, year, bimonth, initial_flow_normal)
    
    # Replace NA/ Inf w/ Zeros
    is.na(normal_flow) <- sapply(normal_flow, is.infinite)
    normal_flow[is.na(normal_flow)] <- 1.0
    
    # Join Start and end of week flows
    flow_differences <- 
      dplyr::left_join(
        bimonth_initial_flow,
        bimonth_end_flow,
        by = c("site", "year", "bimonth")
      ) %>% 
      dplyr::left_join(
        normal_flow,
        by = c("site", "year", "bimonth")
      ) %>% 
      dplyr::mutate(
        year  = lubridate::year(week_start_date), 
        month = lubridate::month(week_start_date) 
      ) %>% 
      dplyr::relocate(site, year, month, week_start_date, week_end_date,
                      day_num, bimonth, days_in_bimonth, initial_flow, end_flow, nat_end_flow) %>% 
      dplyr::mutate(
        dflow         = (end_flow - initial_flow)/days_in_bimonth,
        nat_dflow     = (nat_end_flow - initial_flow)/days_in_bimonth
      ) %>% 
      dplyr::select(site, year, month, bimonth,days_in_bimonth,  week_start_date, 
                    week_end_date, initial_flow, end_flow, 
                    nat_end_flow, dflow, nat_dflow, initial_flow_normal, diversion, release) 
    
    # Total flow in bimonth and change in flow over bimonth 
    flow_bimonth_summary <- 
      flows_bimonth_ts %>% 
      dplyr::select(site, date, year, bimonth, total_flow, avg_flow, dflow) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(site, year, bimonth) %>% 
      dplyr::summarize(
        total_flow = mean(total_flow, na.rm = T),
        avg_flow   = mean(avg_flow, na.rm = T)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(
        flow_differences, 
        by = c("site", "year", "bimonth")
      ) %>% 
      dplyr::mutate(dplyr::across(where(is.numeric), round, 4)) %>% 
      dplyr::mutate(
        month = lubridate::month(month, label = T),
        season = case_when(
          month %in% c("Dec", "Jan", "Feb") ~ "winter",
          month %in% c("Mar", "Apr", "May") ~ "spring",
          month %in% c("Jun", "Jul", "Aug") ~ "summer",
          month %in% c("Sep", "Oct", "Nov") ~ "fall"
        ),
        season = factor(season)
      ) %>%
      dplyr::select(site, week_start_date, week_end_date, year, season, month, bimonth, 
                    days_in_bimonth, initial_flow, end_flow, nat_end_flow, dflow, nat_dflow, 
                    initial_flow_normal, total_flow, avg_flow,  diversion, release) %>% 
      dplyr::ungroup()
    
    # Replace NA/ Inf w/ Zeros
    is.na(flow_bimonth_summary) <- sapply(flow_bimonth_summary, is.infinite)
    flow_bimonth_summary[is.na(flow_bimonth_summary)] <- 0
    
    return(flow_bimonth_summary)
    
  } else if(timescale == "weekly") {
    
    # Calculate weekly flows (initial_flow) & change in flow from prior week (dflow)
    flows_week_ts <-  
      data %>% 
      # filter(segment == "Poudre Park") %>%
      dplyr::mutate(
        year      = lubridate::year(date),
        month     = lubridate::month(date),
        week      = lubridate::week(date),
        day_num   = lubridate::day(date)
      ) %>% 
      dplyr::mutate(
        date_ym = as.Date(paste0(year, "-", month, "-01")),
      ) %>% 
      # filter(year %in% c(2015, 2016)) %>%
      dplyr::group_by(site, year, week) %>%
      dplyr::mutate(
        total_flow   = sum(flow, na.rm = T),
        days_in_week = n()
      ) %>%
      dplyr::group_by(site, year, week) %>%
      # group_by(segment, date_ym) %>%
      dplyr::slice(
        which.min(day_num),
        which.max(day_num)
      ) %>%
      dplyr::arrange(site, date) %>%
      dplyr::mutate(
        days_in_month  = lubridate::days_in_month(date),
        flow_diff      = diff(flow)
        # between0       = as.numeric(difftime(date, lag(date,1))), 
        # days_in_week        = ifelse(is.na(between0), 0, between0),
      ) %>%
      dplyr::relocate(site, site, date, date_ym, day_num, week, days_in_week,
                      days_in_month, flow, flow_diff, total_flow) %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(site) %>%
      dplyr::arrange(site, date, .by_group	= T) %>%
      dplyr::mutate(    
        dflow            = flow_diff/days_in_week
      ) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(site, site, date, date_ym, day_num, week,  days_in_week, 
                      days_in_month, flow, flow_diff, total_flow, dflow, diversion, release) 
    
    
    # Start of week flows
    week_initial_flow <- 
      flows_week_ts %>% 
      dplyr::group_by(site, year, week) %>% 
      dplyr::slice(which.min(date)) %>% 
      dplyr::ungroup() %>%
      dplyr::select(site, year, week_start_date = date, 
                    day_num, week, days_in_week, initial_flow = flow) 
    
    # End of week flows
    week_end_flow <- 
      flows_week_ts %>% 
      dplyr::group_by(site, year, week) %>% 
      dplyr::slice(which.max(date)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        nat_end_flow = flow + diversion - release
      ) %>% 
      dplyr::select(site, year, week_end_date = date, 
                    # date_ym, 
                    week, end_flow = flow, nat_end_flow, diversion, release)
    
    # Join Start and end of week flows
    flow_differences <- 
      dplyr::left_join(
        week_initial_flow,
        week_end_flow,
        by = c("site", "year", "week")
      ) %>% 
      dplyr::mutate(
        year  = lubridate::year(week_start_date), 
        month = lubridate::month(week_start_date) 
      ) %>% 
      dplyr::relocate(site, year, month, week_start_date, week_end_date,
                      day_num, week, days_in_week, initial_flow, end_flow, nat_end_flow) %>% 
      dplyr::mutate(
        # dflow = (end_flow - initial_flow)/days_in_week
        dflow         = (end_flow - initial_flow)/days_in_week,
        nat_dflow     = (nat_end_flow - initial_flow)/days_in_week
      ) %>% 
      dplyr::relocate(site, year, month, week_start_date, week_end_date,
                      day_num, week, days_in_week, initial_flow, end_flow, nat_end_flow, dflow, nat_dflow) 
    
    # Total flow in week and change in flow over week 
    flow_week_summary <- 
      flows_week_ts %>% 
      dplyr::select(site, date, year, week, total_flow, dflow) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(site, year, week) %>% 
      dplyr::summarize(
        total_flow = mean(total_flow, na.rm = T)
      ) %>% 
      dplyr::left_join(
        flow_differences, 
        by = c("site", "year", "week")
      ) %>% 
      dplyr::mutate(dplyr::across(where(is.numeric), round, 4)) %>% 
      dplyr::mutate(
        month = lubridate::month(month, label = T),
        season = dplyr::case_when(
          month %in% c("Dec", "Jan", "Feb") ~ "winter",
          month %in% c("Mar", "Apr", "May") ~ "spring",
          month %in% c("Jun", "Jul", "Aug") ~ "summer",
          month %in% c("Sep", "Oct", "Nov") ~ "fall"
        ),
        season = factor(season)
      ) %>%
      dplyr::select(site, week_start_date, week_end_date, year, season, month, week, days_in_week, initial_flow, end_flow, nat_end_flow, dflow, nat_dflow, diversion, release, total_flow) %>% 
      dplyr::ungroup()
    
    # Replace NA/ Inf w/ Zeros
    is.na(flow_week_summary) <- sapply(flow_week_summary, is.infinite)
    flow_week_summary[is.na(flow_week_summary)] <- 0
    
    return(flow_week_summary)
  }
  # } else if(timescale == "daily") {
  #   
  #   # Calculate weekly flows (initial_flow) & change in flow from prior week (dflow)
  #   flows_day_ts <-  
  #     # data %>% 
  #     natural_flow %>% 
  #     # filter(segment == "Poudre Park") %>%
  #     dplyr::mutate(
  #       year      = lubridate::year(date),
  #       month     = lubridate::month(date),
  #       week      = lubridate::week(date),
  #       day_num   = lubridate::day(date)
  #     ) %>% 
  #     dplyr::mutate(
  #       date_ym = as.Date(paste0(year, "-", month, "-01")),
  #     ) %>% 
  #     # filter(year %in% c(2015, 2016)) %>%
  #     dplyr::group_by(site, year, week) %>%
  #     dplyr::mutate(
  #       total_flow   = sum(flow, na.rm = T),
  #       days_in_week = n()
  #     ) %>%
  #     dplyr::ungroup() %>% 
  #     dplyr::arrange(date) %>% 
  #     dplyr::mutate(
  #       dflow = flow - lag(flow),
  #       nat_dflow = nat_flow - lag(nat_flow)
  #     )
  #     dplyr::group_by(site, year, week) %>%
  #     # group_by(segment, date_ym) %>%
  #     dplyr::slice(
  #       which.min(day_num),
  #       which.max(day_num)
  #     ) %>%
  #     dplyr::arrange(site, date) %>%
  #     dplyr::mutate(
  #       days_in_month  = lubridate::days_in_month(date),
  #       flow_diff      = diff(flow)
  #       # between0       = as.numeric(difftime(date, lag(date,1))), 
  #       # days_in_week        = ifelse(is.na(between0), 0, between0),
  #     ) %>%
  #     dplyr::relocate(site, site, date, date_ym, day_num, week, days_in_week,
  #                     days_in_month, flow, flow_diff, total_flow) %>% 
  #     dplyr::ungroup() %>%
  #     dplyr::group_by(site) %>%
  #     dplyr::arrange(site, date, .by_group	= T) %>%
  #     dplyr::mutate(    
  #       dflow            = flow_diff/days_in_week
  #     ) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::relocate(site, site, date, date_ym, day_num, week,  days_in_week, 
  #                     days_in_month, flow, flow_diff, total_flow, dflow, diversion, release) 
  #   
  #   
  #   # Start of week flows
  #   week_initial_flow <- 
  #     flows_week_ts %>% 
  #     dplyr::group_by(site, year, week) %>% 
  #     dplyr::slice(which.min(date)) %>% 
  #     dplyr::ungroup() %>%
  #     dplyr::select(site, year, week_start_date = date, 
  #                   day_num, week, days_in_week, initial_flow = flow) 
  #   
  #   # End of week flows
  #   week_end_flow <- 
  #     flows_week_ts %>% 
  #     dplyr::group_by(site, year, week) %>% 
  #     dplyr::slice(which.max(date)) %>% 
  #     dplyr::ungroup() %>% 
  #     dplyr::mutate(
  #       nat_end_flow = flow + diversion - release
  #     ) %>% 
  #     dplyr::select(site, year, week_end_date = date, 
  #                   # date_ym, 
  #                   week, end_flow = flow, nat_end_flow, diversion, release)
  #   
  #   # Join Start and end of week flows
  #   flow_differences <- 
  #     dplyr::left_join(
  #       week_initial_flow,
  #       week_end_flow,
  #       by = c("site", "year", "week")
  #     ) %>% 
  #     dplyr::mutate(
  #       year  = lubridate::year(week_start_date), 
  #       month = lubridate::month(week_start_date) 
  #     ) %>% 
  #     dplyr::relocate(site, year, month, week_start_date, week_end_date,
  #                     day_num, week, days_in_week, initial_flow, end_flow, nat_end_flow) %>% 
  #     dplyr::mutate(
  #       # dflow = (end_flow - initial_flow)/days_in_week
  #       dflow         = (end_flow - initial_flow)/days_in_week,
  #       nat_dflow     = (nat_end_flow - initial_flow)/days_in_week
  #     ) %>% 
  #     dplyr::relocate(site, year, month, week_start_date, week_end_date,
  #                     day_num, week, days_in_week, initial_flow, end_flow, nat_end_flow, dflow, nat_dflow) 
  #   
  #   # Total flow in week and change in flow over week 
  #   flow_week_summary <- 
  #     flows_week_ts %>% 
  #     dplyr::select(site, date, year, week, total_flow, dflow) %>% 
  #     dplyr::ungroup() %>% 
  #     dplyr::group_by(site, year, week) %>% 
  #     dplyr::summarize(
  #       total_flow = mean(total_flow, na.rm = T)
  #     ) %>% 
  #     dplyr::left_join(
  #       flow_differences, 
  #       by = c("site", "year", "week")
  #     ) %>% 
  #     dplyr::mutate(dplyr::across(where(is.numeric), round, 4)) %>% 
  #     dplyr::mutate(
  #       month = lubridate::month(month, label = T),
  #       season = dplyr::case_when(
  #         month %in% c("Dec", "Jan", "Feb") ~ "winter",
  #         month %in% c("Mar", "Apr", "May") ~ "spring",
  #         month %in% c("Jun", "Jul", "Aug") ~ "summer",
  #         month %in% c("Sep", "Oct", "Nov") ~ "fall"
  #       ),
  #       season = factor(season)
  #     ) %>%
  #     dplyr::select(site, week_start_date, week_end_date, year, season, month, week, days_in_week, initial_flow, end_flow, nat_end_flow, dflow, nat_dflow, diversion, release, total_flow) %>% 
  #     dplyr::ungroup()
  #   
  #   # Replace NA/ Inf w/ Zeros
  #   is.na(flow_week_summary) <- sapply(flow_week_summary, is.infinite)
  #   flow_week_summary[is.na(flow_week_summary)] <- 0
  #   
  #   return(flow_week_summary)
  # }
}


reaches <- function(reach = "all") {
  
  reach_segment <- c(
    "Big South", "Bridges", "Filter Plant", "Grandpas Gorge",
    "Lower Mishawaka","Poudre Park", "Poudre Whitewater Park", "Spencer Heights", 
    "The Narrows", "Upper Mishawaka", "White Mile Run"
  )  
  
  sites <- c( 
    "LAPLODCO", "Pineview model", "CLAFTCCO", "Pineview model",
    "Pineview model", "Pineview model","USGS_06752260","Pineview model",
    "Pineview model", "Pineview model", "Pineview model")
  
  
  max_acceptable <- c(
    1000000.00, 1000000.00, 1000000.00, 1000000.00, 
    1000000.00, 1000000.00, 1000000.00, 1000000.00, 
    1495.26, 1000000.00, 1000000.00
  )
  
  min_acceptable <- c(
    170.1005,  484.5036,  522.7273,  854.2068, 
    564.2158,  521.6029, 354.7970,  665.3055, 
    338.8532,  545.3782, 1012.5719
  )
  
  reach_flow_pref <- tibble(
    segment        = reach_segment, 
    site           = sites, 
    min_acceptable = min_acceptable, 
    max_acceptable = max_acceptable
  )
  
  if (reach == "all") {
    logger::log_info("Boating flow preferences\nReach: all ")
    return(reach_flow_pref)
    
  } else if(reach %in% reach_segment) {
    
    logger::log_info("Boating flow preferences\nReach: {reach}")
    
    reach_flow_pref <- 
      reach_flow_pref %>%
      filter(segment == reach)
    
    return(reach_flow_pref)
    
  } else if(!reach %in% reach_segment) {
    
    logger::log_info("Reach {reach} not avaliable\n Enter reach = \'all'\ or one of:\n {list(reach_segment)}")
    
  }
}

# get reservoir flows in daily CFS
get_reservoir_flows <- function(end_date = Sys.Date()) {
  
  start_date <- end_date %m-% months(6)
  
  # Reservoirs and WDIDs
  res_structures <- data.frame(
    structure  =  c('long_draw', 'chambers', 'joe_wright', 'peterson', 'barnes_meadow'),
    wdid       =  c("0303676", "0303679", "0303678", "0303677", "0303683")
  )
  
  reservoir_lst <- list()
  
  # Loop through reservoir WDIDs and pull data from CDSS using getCDSSDiversionFlow()
  for (i in 1:length(res_structures$wdid)) {
    
    logger::log_info("Downloading reservoir data:\n{res_structures$structure[i]}\nWDID: {res_structures$wdid[i]}")
    
    # get releases data for reservoir
    release <- getCDSSDiversionFlow(
      wdid      = res_structures$wdid[i],
      data_type = "release",
      timescale = "monthly"
    ) %>%
      dplyr::select(date, release)
    
    # get flow data for reservoir and sum flow by month 
    diversion <- getCDSSDiversionFlow(
      wdid      = res_structures$wdid[i], 
      data_type = "flow",
      timescale = "monthly" 
    ) %>% 
      dplyr::select(date, diversion = flow)
    
    # Join diversion/release and flow data
    res_data <- 
      diversion %>% 
      dplyr::left_join(
        release,
        by = c("date")
      ) %>% 
      dplyr::mutate(
        structure = res_structures$structure[i],
        source    = "CDSS"
      ) %>% 
      dplyr::select(
        structure, date, diversion, release, source
      ) %>%
      cleaner::na_replace(release, diversion) %>% 
      dplyr::group_by(structure) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        dvolume               = (diversion - release)        # dS = diversion - releases
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::relocate(structure, date, diversion, release, dvolume, source)
    
    reservoir_lst[[i]] <- res_data
    
  }
  
  reservoirs <- 
    reservoir_lst %>% 
    dplyr::bind_rows() %>% 
    dplyr::select(structure, date, diversion, release, dvolume) %>% 
    tidyr::pivot_wider(
      id_cols     = c(tidyselect::matches("structure"), date),
      names_from  = "structure",
      names_glue  = "{structure}_{.value}",
      values_from = c(diversion, release, dvolume),
      values_fn   = mean
    ) 
  
  
  # Generate daily dates using start/end dates 
  days <- seq.Date(
    from =  min(reservoirs$date),
    to   =  end_date,
    by   = "day"
  ) %>% 
    dplyr::tibble() %>% 
    stats::setNames(c("date")) %>% 
    dplyr::mutate(
      month   = lubridate::month(date),
      year    = lubridate::year(date),
      date_ym = as.Date(paste0(year, "-", month, "-01"))
    ) %>% 
    dplyr::select(date, date_ym)
  
  reservoir_daily <- 
    days %>% 
    dplyr::left_join(
      reservoirs,
      by = c("date_ym" = "date")
    ) %>% 
    dplyr::group_by(date_ym) %>%
    dplyr::mutate(
      days_in_month   = n()
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-date_ym) %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(
      dplyr::across(dplyr::everything(), ~ convert_af_to_cfs(.)/days_in_month)
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::select(-days_in_month) %>% 
    tidyr::pivot_longer(cols = c(-date)) %>% 
    dplyr::mutate(
      flow_type = dplyr::case_when(
        grepl("diversion", name) ~ "diversion",
        grepl("release", name)   ~ "release",
        grepl("dvolume", name)   ~ "dvolume"
      ),
      structure = dplyr::case_when(
        grepl("long_draw", name)     ~ "long_draw",
        grepl("chambers", name)      ~ "chambers",
        grepl("joe_wright", name)    ~ "joe_wright",
        grepl("barnes_meadow", name) ~ "barnes_meadow",
        grepl("peterson", name)      ~ "peterson"
      )
    ) %>% 
    dplyr::select(date, structure, flow_type, flow = value) %>% 
    dplyr::tibble() %>% 
    dplyr::mutate(
      month = lubridate::month(date)
    ) %>% 
    dplyr::group_by(structure, flow_type, month) %>% 
    dplyr::mutate(flow = impute_mean(flow)) %>%  # Impute monthly site
    dplyr::ungroup() %>% 
    dplyr::select(-month) %>% 
    replace(is.na(.), 0) %>%   # replace NA values w/ 0
    tidyr::pivot_wider(
      id_cols     = c(tidyselect::matches("structure"), tidyselect::matches("flow_type"), date),
      names_from  = c(tidyselect::matches("flow_type"), tidyselect::matches("structure")),
      values_from = flow
    )
  
  return(reservoir_daily)
  
}

# Join daily site flow data w/ reservoir opts and combine to get natural flows
aggregate_natural_flows <- function(data) {
  
  natural_flows <-
    data %>% 
    dplyr::mutate(                    # replace negative pineview flows w/ 0
      flow = dplyr::case_when(
        flow < 0 ~ 0,
        TRUE     ~ flow
      )
    ) %>% 
    dplyr::mutate(
      diversion = dplyr::case_when(
        site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (diversion_long_draw + diversion_chambers + diversion_joe_wright + 
                                                                         diversion_peterson + diversion_barnes_meadow),
        site == "LAPLODCO"                                          ~ (diversion_long_draw + diversion_peterson)
      ),
      release = dplyr::case_when(
        site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (release_long_draw + release_chambers + release_joe_wright + 
                                                                         release_peterson + release_barnes_meadow),
        site == "LAPLODCO"                                          ~ (release_long_draw + release_peterson)
      ),
      dvolume = dplyr::case_when(
        site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (dvolume_long_draw + dvolume_chambers + dvolume_joe_wright + 
                                                                         dvolume_peterson + dvolume_barnes_meadow),
        site == "LAPLODCO"                                          ~ (dvolume_long_draw + dvolume_peterson)
      ),
      nat_flow = dplyr::case_when(
        site %in% c("Pineview model", "USGS_06752260", "CLAFTCCO")  ~ (flow + dvolume),
        site == "LAPLODCO"                                          ~ (flow + dvolume)
      )
    ) %>% 
    dplyr::select(site, date, flow, nat_flow, diversion, release, dvolume)
  
  return(natural_flows)
  
}

get_model_inputs <- function(
    segment    = "Filter Plant",
    start_date = "1900-01-01",
    end_date   = Sys.Date(),
    timescale  = "monthly",
    res_data   = NULL
)  {
  
  if(is.null(res_data)) {
    
    # Retrieve reservoir data from CDSS API
    reservoir_summary <- get_reservoir_flows()
    
  } else {
    
    # user inputted reservoir data
    reservoir_summary <- res_data
  }
  
  # Pineview model segments
  pineview_segments <- c("Bridges", "Poudre Park", "Lower Mishawaka", "Upper Mishawaka","Spencer Heights", "The Narrows", "Grandpas Gorge","White Mile Run")
  # start_date <- end_date %m-% months(6)
  
  if(segment %in% pineview_segments) {
    
    # logger::log_info('Retrieving flow data for {segment}\n{start_date} to {end_date}')
    
    logger::log_info('Retrieving flow data - Poudre Canyon Mouth')
    # get flow data for Poudre Canyon mouth
    poudre_canyon_mouth <- GetCDSSStationFlow(
      start_date  = start_date,
      end_date    = end_date,
      site_abbrev = 'CLAFTCCO',
      timescale   = "daily"
    ) %>% 
      dplyr::select(date, canyon = flow)
    
    # logger::log_info('Retrieving flow data - Poudre Valley Canal')
    # # get flow data for Poudre Valley Canal
    # poudre_valley_canal <- getCDSSDiversionFlow(
    #   wdid        = '0300907'
    # ) %>% 
    #   group_by(date) %>% 
    #   summarize(valley = mean(flow,na.rm = T)) %>% 
    #   ungroup() %>% 
    #   dplyr::select(date, valley)
    # # rename(valley = flow) 
    # 
    # logger::log_info('Retrieving flow data - North Fork Poudre River')
    # # get flow data for North Fork Poudre river
    # north_fork <- GetCDSSStationFlow(
    #   start_date  = start_date,
    #   end_date    = end_date,
    #   site_abbrev = 'CLANSECO',
    #   timescale   = "daily"
    # ) %>% 
    #   dplyr::select(date, northfk = flow)
    # 
    # logger::log_info('Retrieving flow data - North Poudre Supply Canal')
    # 
    # # get flow data for the North Poudre Supply Canal
    # poudre_supply_canal <- getCDSSDiversionFlow(
    #   wdid        = '0300905'
    # ) %>% 
    #   filter(
    #     date >= start_date,
    #     date <= end_date
    #   ) %>% 
    #   dplyr::select(date, supply = flow)
    
    logger::log_info('Calculating mass balance model for {segment}')
    
    poudre_park_model <- 
      poudre_canyon_mouth %>%
      # left_join(north_fork,          by = 'date') %>%
      # left_join(poudre_valley_canal, by = 'date') %>%
      # left_join(poudre_supply_canal, by = 'date') %>%
      replace_na(list(canyon = 0)) %>%
      # replace_na(list(canyon = 0, valley = 0, supply = 0, northfk = 0 )) %>%
      mutate(
        # model = canyon - northfk + supply + valley,
        model = canyon,
        site  = "Pineview model"
      ) %>% 
      dplyr::select(site, date, flow = model)
    # ggplot() +
    #   geom_line(data = poudre_park_model, aes(x = date, y = flow))
    # Calculate natural flows 
    natural_flow <- 
      poudre_park_model %>% 
      dplyr::left_join(
        reservoir_summary,
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      flow_summary(timescale = timescale) 
    
    
    return(natural_flow)
    
  } else if(segment == "Poudre Whitewater Park") {
    
    logger::log_info('Retrieving flow data - {segment}')
    
    # Poudre Whitewater Park
    flow_usgs <- dataRetrieval::readNWISdv(
      siteNumbers = '06752260', 
      parameterCd = '00060',
      startDate   = '1976-01-01',
      endDate     = end_date
    ) %>%
      dplyr::rename(flow = 'X_00060_00003', date = Date) %>%
      dplyr::mutate(site = 'USGS_06752260') %>%
      dplyr::select(site, date, flow)
    
    # Calculate natural flows 
    natural_flow <- 
      flow_usgs %>% 
      dplyr::left_join(
        reservoir_summary,
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      flow_summary(timescale = timescale) 
    
    return(natural_flow)
    
  } else if(segment == "Filter Plant") {
    
    logger::log_info('Retrieving flow data - {segment}')
    
    # Filter plant
    flow_filter_plant <- GetCDSSStationFlow(
      start_date  = start_date,
      end_date    = end_date,
      site_abbrev = 'CLAFTCCO',
      timescale   = "daily"
    ) %>%  
      dplyr::mutate(site   = 'CLAFTCCO') %>% 
      dplyr::select(site, date, flow)
    
    # Calculate natural flows using reservoir diversions/releases
    natural_flow <- 
      flow_filter_plant %>% 
      dplyr::left_join(
        reservoir_summary,
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      flow_summary(timescale = timescale) 
    
    return(natural_flow)
    
  } else if(segment == "Big South") {
    
    logger::log_info('Retrieving flow data - {segment}')
    
    # Big South
    flow_big_south <- GetCDSSStationFlow(
      start_date  = start_date,
      end_date    = end_date,
      site_abbrev = 'LAPLODCO',
      timescale   = "daily"
    ) %>%  
      dplyr::group_by(date) %>% 
      dplyr::summarize(flow = mean(flow, na.rm = T)) %>% 
      dplyr::ungroup() %>% 
      dplyr:: mutate(date = as.POSIXct(date, format =  "%Y/%m/%d")) %>% 
      dplyr::select(date, flow) %>% 
      tidyr::complete(
        date = seq.POSIXt(min(date), max(date),  by = "days"
        ))  %>% 
      dplyr::mutate(
        month     = lubridate::month(as.Date(as.character(date)))
      ) %>% 
      dplyr::group_by(month) %>%
      dplyr::mutate(
        flow      = round(impute_mean(flow), 4)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        site = 'LAPLODCO',
        date = as.Date(date)
      ) %>% 
      dplyr::select(site, date, flow)
    
    # Calculate natural flows using reservoir diversions/releases
    natural_flow <- 
      flow_big_south %>% 
      dplyr::left_join(
        reservoir_summary,
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      flow_summary(timescale = timescale) 
    
    return(natural_flow)
    
  } else {
    logger::log_info('\n\nsegment argument must equal one of segments:\nBridges\nPoudre Park\nLower Mishawaka\nUpper Mishawaka\nSpencer Heights\nThe Narrows\nGrandpas Gorge\nWhite Mile Run\nFilter Plant\nBig South\nPoudre Whitewater Park')
  }
}

# Calculate monthly influence of reservervoir operations on flows, the average percentage increase/decrease to flows from reservoir operations by month
calc_res_op_pct <- function(agg_nat_flows, time_res = "month") {
  
  if(time_res == "month") {
    
    res_operations_pct <- 
      agg_nat_flows %>%
      # natural_flow %>% 
      dplyr::mutate(
        year  = lubridate::year(date),
        month = lubridate::month(date, label = T)
      ) %>% 
      dplyr::group_by(site, year, month) %>% 
      dplyr::summarise(across(where(is.numeric), sum, na.rm = T)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
      dplyr::mutate(res_op_pct = (nat_flow - flow)/flow) %>%
      dplyr::mutate(
        res_op_pct = dplyr::case_when(
          flow == 0 ~ 1,
          TRUE      ~ res_op_pct
        )
      ) %>% 
      dplyr::mutate(
        res_op_pct = dplyr::case_when(
          res_op_pct > 1 ~ 1,
          TRUE           ~ res_op_pct
        )
      ) %>% 
      dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
      dplyr::select(-year) %>% 
      dplyr::group_by(site, month) %>%   
      dplyr::summarise(across(where(is.numeric), mean, na.rm = T)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
      dplyr::select(site, month, res_op_pct)
    
    return(res_operations_pct)
  } else if(time_res == "week") {
    # ggplot() +
    #   geom_col(data = res_operations_pct, aes(x = week, y = res_op_pct))
    daily_flows %>% 
      ggplot() +
      geom_col(aes(x = date, y = flow)) +
      facet_wrap(~site)
    res_operations_pct <- 
      # agg_nat_flows %>%
      natural_flow %>%
      dplyr::mutate(
        year  = lubridate::year(date),
        month = lubridate::month(date, label = T),
        week  = lubridate::week(date),
        day   = lubridate::day(date)
      ) %>% 
      dplyr::mutate(res_op_pct = (nat_flow - flow)/flow) %>%
      dplyr::group_by(site, week) %>% 
      # dplyr::summarise(across(where(is.numeric), mean, na.rm = T)) %>%
      dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
      dplyr::mutate(
        res_op_pct = dplyr::case_when(
          flow == 0 & nat_flow == 0 ~ 0,
          flow == 0 & nat_flow > 0  ~ 1,
          TRUE                      ~ res_op_pct
        )
      ) %>%
      dplyr::mutate(
        res_op_pct = dplyr::case_when(
          res_op_pct > 1 ~ 1,
          TRUE           ~ res_op_pct
        )
      ) %>%
      dplyr::summarise(across(where(is.numeric), mean, na.rm = T)) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
      dplyr::select(site, week, res_op_pct)
    
    return(res_operations_pct)
    
  } else if(time_res == "day") {
    
    # res_operations_pct %>%
    #   ggplot() +
    #   geom_line(aes(x = day, y = res_op_pct)) +
    #   scale_y_continuous(limits = c(-1, 1)) +
    #   geom_hline(yintercept = 0, col = "red") +
    #   facet_wrap(~month)
    
    res_operations_pct <- 
      agg_nat_flows %>%
      dplyr::mutate(
        year  = lubridate::year(date),
        month = lubridate::month(date, label = T),
        day   = lubridate::day(date)
      ) %>% 
      dplyr::mutate(res_op_pct = (nat_flow - flow)/flow) %>%
      dplyr::group_by(site, month, day) %>% 
      # dplyr::summarise(across(where(is.numeric), mean, na.rm = T)) %>%
      dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
      dplyr::mutate(
        res_op_pct = dplyr::case_when(
          flow == 0 & nat_flow == 0 ~ 0,
          flow == 0 & nat_flow > 0  ~ 1,
          TRUE                      ~ res_op_pct
        )
      ) %>% 
      dplyr::mutate(
        res_op_pct = dplyr::case_when(
          res_op_pct > 1 ~ 1,
          TRUE           ~ res_op_pct
        )
      ) %>%
      dplyr::summarise(across(where(is.numeric), mean, na.rm = T)) %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
      dplyr::select(site, month, day, res_op_pct)
    
    return(res_operations_pct)
    
  }
}

get_reservoir_factor <- function(
    segment    = "Filter Plant",
    start_date = "1900-01-01",
    end_date   = Sys.Date(),
    res_data   = NULL,
    time_res   = "month"
)  {
  
  if(is.null(res_data)) {
    
    # Retrieve reservoir data from CDSS API
    reservoir_summary <- get_reservoir_flows()
    
  } else {
    
    # user inputted reservoir data
    reservoir_summary <- res_data
  }
  
  # Pineview model segments
  pineview_segments <- c("Bridges", "Poudre Park", "Lower Mishawaka", "Upper Mishawaka","Spencer Heights", "The Narrows", "Grandpas Gorge","White Mile Run", "Filter Plant")
  # start_date <- end_date %m-% months(6)
  
  if(segment %in% pineview_segments) {
    
    # logger::log_info('Retrieving flow data for {segment}\n{start_date} to {end_date}')
    
    logger::log_info('Retrieving flow data - Poudre Canyon Mouth')
    # get flow data for Poudre Canyon mouth
    poudre_canyon_mouth <- GetCDSSStationFlow(
      start_date  = start_date,
      end_date    = end_date,
      site_abbrev = 'CLAFTCCO',
      timescale   = "daily"
    ) %>% 
      dplyr::select(date, flow) %>% 
      replace_na(list(flow = 0)) %>%  
      dplyr::mutate(
        site  = "Pineview model"
      ) %>% 
      dplyr::select(site, date, flow)
    
    
    
    logger::log_info('Calculating mass balance model for {segment}')
    
    # Calculate natural flows 
    natural_flow <- 
      poudre_canyon_mouth %>% 
      dplyr::left_join(
        reservoir_summary,
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      dplyr::mutate(
        nat_flow = dplyr::case_when(
          nat_flow < 0 ~ 0,
          TRUE         ~ nat_flow
        )
      ) %>% 
      calc_res_op_pct(time_res   = time_res)
    
    return(natural_flow)
    
  } else if(segment == "Poudre Whitewater Park") {
    
    logger::log_info('Retrieving flow data - {segment}')
    
    # Poudre Whitewater Park
    flow_usgs <- dataRetrieval::readNWISdv(
      siteNumbers = '06752260', 
      parameterCd = '00060',
      startDate   = '1976-01-01',
      endDate     = end_date
    ) %>%
      dplyr::rename(flow = 'X_00060_00003', date = Date) %>%
      dplyr::mutate(site = 'USGS_06752260') %>%
      dplyr::select(site, date, flow)
    
    # Calculate natural flows 
    natural_flow <- 
      flow_usgs %>% 
      dplyr::left_join(
        reservoir_summary,
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      dplyr::mutate(
        nat_flow = dplyr::case_when(
          nat_flow < 0 ~ 0,
          TRUE         ~ nat_flow
        )
      ) %>% 
      calc_res_op_pct(time_res   = time_res)
    
    return(natural_flow)
    
  }  else if(segment == "Big South") {
    
    logger::log_info('Retrieving flow data - {segment}')
    
    # Big South
    flow_big_south <- GetCDSSStationFlow(
      start_date  = start_date,
      end_date    = end_date,
      site_abbrev = 'LAPLODCO',
      timescale   = "daily"
    ) %>%  
      dplyr::group_by(date) %>% 
      dplyr::summarize(flow = mean(flow, na.rm = T)) %>% 
      dplyr::ungroup() %>% 
      dplyr:: mutate(date = as.POSIXct(date, format =  "%Y/%m/%d")) %>% 
      dplyr::select(date, flow) %>% 
      tidyr::complete(
        date = seq.POSIXt(min(date), max(date),  by = "days"
        ))  %>% 
      dplyr::mutate(
        month     = lubridate::month(as.Date(as.character(date)))
      ) %>% 
      dplyr::group_by(month) %>%
      dplyr::mutate(
        flow      = round(impute_mean(flow), 4)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        site = 'LAPLODCO',
        date = as.Date(date)
      ) %>% 
      dplyr::select(site, date, flow)
    
    # Calculate natural flows using reservoir diversions/releases
    natural_flow <- 
      flow_big_south %>% 
      dplyr::left_join(
        reservoir_summary, 
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      dplyr::mutate(
        nat_flow = dplyr::case_when(
          nat_flow < 0 ~ 0, 
          TRUE         ~ nat_flow
        )
      ) %>% 
      calc_res_op_pct(time_res = time_res)
    
    # natural_flow %>%
    #   tibble::tibble() %>%
    #   dplyr::filter(date >="2005-04-01", date <= "2006-09-01") %>%
    #   tidyr::pivot_longer(cols = c(nat_flow, flow)) %>%
    #   ggplot() +
    #   geom_line(aes(x = date, y = value, col = name, alpha = 0.6), size = 1)
    
    
    return(natural_flow)
    
  } else {
    logger::log_info('\n\nsegment argument must equal one of segments:\nBridges\nPoudre Park\nLower Mishawaka\nUpper Mishawaka\nSpencer Heights\nThe Narrows\nGrandpas Gorge\nWhite Mile Run\nFilter Plant\nBig South\nPoudre Whitewater Park')
  }
}

get_daily_flows <- function(
    segment    = "Filter Plant",
    start_date = "1900-01-01",
    end_date   = Sys.Date(),
    res_data   = NULL
)  {
  
  if(is.null(res_data)) {
    
    # Retrieve reservoir data from CDSS API
    reservoir_summary <- get_reservoir_flows()
    
  } else {
    
    # user inputted reservoir data
    reservoir_summary <- res_data
  }
  
  # Pineview model segments
  pineview_segments <- c("Bridges", "Poudre Park", "Lower Mishawaka", "Upper Mishawaka","Spencer Heights", "The Narrows", "Grandpas Gorge","White Mile Run", "Filter Plant")
  # start_date <- end_date %m-% months(6)
  
  if(segment %in% pineview_segments) {
    
    # logger::log_info('Retrieving flow data for {segment}\n{start_date} to {end_date}')
    
    logger::log_info('Retrieving flow data - Poudre Canyon Mouth')
    # get flow data for Poudre Canyon mouth
    poudre_canyon_mouth <- GetCDSSStationFlow(
      start_date  = start_date,
      end_date    = end_date,
      site_abbrev = 'CLAFTCCO',
      timescale   = "daily"
    ) %>% 
      dplyr::select(date, flow) %>% 
      replace_na(list(flow = 0)) %>%  
      dplyr::mutate(
        site  = "Pineview model"
      ) %>% 
      dplyr::select(site, date, flow)
    
    logger::log_info('Calculating mass balance model for {segment}')
    
    # Calculate natural flows 
    natural_flow <- 
      poudre_canyon_mouth %>% 
      dplyr::left_join(
        reservoir_summary,
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      dplyr::mutate(
        nat_flow = dplyr::case_when(
          nat_flow < 0 ~ 0,
          TRUE         ~ nat_flow
        )
      ) 
    
    return(natural_flow)
    
  } else if(segment == "Poudre Whitewater Park") {
    
    logger::log_info('Retrieving flow data - {segment}')
    
    # Poudre Whitewater Park
    flow_usgs <- dataRetrieval::readNWISdv(
      siteNumbers = '06752260', 
      parameterCd = '00060',
      startDate   = '1976-01-01',
      endDate     = end_date
    ) %>%
      dplyr::rename(flow = 'X_00060_00003', date = Date) %>%
      dplyr::mutate(site = 'USGS_06752260') %>%
      dplyr::select(site, date, flow)
    
    # Calculate natural flows 
    natural_flow <- 
      flow_usgs %>% 
      dplyr::left_join(
        reservoir_summary,
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      dplyr::mutate(
        nat_flow = dplyr::case_when(
          nat_flow < 0 ~ 0,
          TRUE         ~ nat_flow
        )
      )
    
    return(natural_flow)
    
  }  else if(segment == "Big South") {
    
    logger::log_info('Retrieving flow data - {segment}')
    
    # Big South
    flow_big_south <- GetCDSSStationFlow(
      start_date  = start_date,
      end_date    = end_date,
      site_abbrev = 'LAPLODCO',
      timescale   = "daily"
    ) %>%  
      dplyr::group_by(date) %>% 
      dplyr::summarize(flow = mean(flow, na.rm = T)) %>% 
      dplyr::ungroup() %>% 
      dplyr:: mutate(date = as.POSIXct(date, format =  "%Y/%m/%d")) %>% 
      dplyr::select(date, flow) %>% 
      tidyr::complete(
        date = seq.POSIXt(min(date), max(date),  by = "days"
        ))  %>% 
      dplyr::mutate(
        month     = lubridate::month(as.Date(as.character(date)))
      ) %>% 
      dplyr::group_by(month) %>%
      dplyr::mutate(
        flow      = round(impute_mean(flow), 4)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        site = 'LAPLODCO',
        date = as.Date(date)
      ) %>% 
      dplyr::select(site, date, flow)
    
    # Calculate natural flows using reservoir diversions/releases
    natural_flow <- 
      flow_big_south %>% 
      dplyr::left_join(
        reservoir_summary,
        by = "date"
      ) %>% 
      aggregate_natural_flows() %>% 
      dplyr::mutate(
        nat_flow = dplyr::case_when(
          nat_flow < 0 ~ 0,
          TRUE         ~ nat_flow
        )
      ) 
    return(natural_flow)
    
  } else {
    logger::log_info('\n\nsegment argument must equal one of segments:\nBridges\nPoudre Park\nLower Mishawaka\nUpper Mishawaka\nSpencer Heights\nThe Narrows\nGrandpas Gorge\nWhite Mile Run\nFilter Plant\nBig South\nPoudre Whitewater Park')
  }
}
