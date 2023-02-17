gage_tbl <- function() {
  
  dplyr::tibble(
   river = c(
            "Crystal",
            "Crystal",
            "Crystal",
            "Crystal",
            "Crystal",
            "Roaring Fork",
            "Roaring Fork",
            "Roaring Fork",
            "Roaring Fork",
            "Roaring Fork",
            "Yampa",
            "Yampa",
            "Yampa",
            "Yampa",
            "Yampa"
            ),
   reach = c(
     "Avalanche Creek to B.R.B. Campground",
     "Penny Hot Springs to Avalanche Creek",
     "Marble to Redstone",
     "Road to Crystal to Beaver Lake",
     "Crystal Mill Falls to Crystal Gorge",
     "Black Bridge to Veltus Park (Cemetery)",
     "Basalt to Carbondale",
     "Lower Woody Creek Bridge to Rte. 82 Bridge",
     "Slaughterhouse",
     "Weller Lake to Difficult CG",
     'Yampa River Park to Transit Center',
     "Transit Center to Pump Station",
     'Little Yampa Canyon',
     "Cross Mountain Gorge - 85 Rd to Deer Lodge Park Rd",
     'Deerlodge Park to Echo Park'
     ),
  gage_id = c(
              "09081600",
              "09081600",
              NA,
              "09081600",
              "09081600",
              "09085000",
              "09081000",
              "09076300",
              "09076300",
              "09073300",
              "09239500",
              "09244490",
              "09247600",
              "09251000",
              "09260050"
              )
  )
  
}
