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
  gage_name = c(
    "Roaring Fork River at Glenwood Springs, CO", "Roaring Fork near Emma, CO",
    "Roaring Fork River Blw Maroon Creek Nr Aspen, CO", "Roaring Fork River Blw Maroon Creek Nr Aspen, CO", 
    "Roaring Fork River Ab Difficult Cr Nr Aspen, CO",
    "Yampa River at Steamboat Springs, CO", "Yampa River above Elkhead Creek near Hayden, CO", 
    "Yampa River below Craig, CO", "Yampa River near Maybell, CO",  "Yampa River at Deerlodge Park, CO",
    "CLAFTCCO", NA, "LAPLODCO"
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
    "USGS",
    "USGS",
    "USGS",
    "USGS",
    "USGS",
    "USGS",
    "USGS",
    "USGS",
    "USGS",
    "USGS",
    "USGS",
    "USGS",
    "CDSS"
  )
  )
  
}

# get_boatable_days <- function(df)
