import cdsspy
import datetime
import pandas as pd
import numpy as np

def gage_tbl():

  tbl = pd.DataFrame({
   'river' :["Roaring Fork",
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
            ],
   'reach' : [
     "Black Bridge to Veltus Park (Cemetery)", "Basalt to Carbondale",
     "Lower Woody Creek Bridge to Rte. 82 Bridge", "Slaughterhouse",  "Weller Lake to Difficult CG",
     'Yampa River Park to Transit Center', "Transit Center to Pump Station",
     'Little Yampa Canyon', "Cross Mountain Gorge - 85 Rd to Deer Lodge Park Rd", 'Deerlodge Park to Echo Park',
     "Filter Plant",  "Poudre Whitewater Park", "Big South"
     ],
   'abbrev' :[
     "ROAGLECO", "ROAEMMCO", None, None, "ROADIFCO",
     "YAMSTECO", "YAMELKCO", "YAMCRACO", "YAMAYBCO",
     "YAMDEECO", "CLAFTCCO", "CLAFORCO", "LAPLODCO"
   ],
  'desc' : [
    "Roaring Fork River at Glenwood Springs, CO", "Roaring Fork near Emma, CO",
    "Roaring Fork River Blw Maroon Creek Nr Aspen, CO", "Roaring Fork River Blw Maroon Creek Nr Aspen, CO", 
    "Roaring Fork River Ab Difficult Cr Nr Aspen, CO",
    "Yampa River at Steamboat Springs, CO", "Yampa River above Elkhead Creek near Hayden, CO", 
    "Yampa River below Craig, CO", "Yampa River near Maybell, CO",  "Yampa River at Deerlodge Park, CO",
    "CLAFTCCO", "CLAFORCO", "LAPLODCO"
  ],
  'gage_id' :[
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
              None
              ],
  'min_threshold' : [
    200, 200, 200, 200,100,
    700, 500, 1100, 700,1300,
    522.7273, 354.797, 170.1005
    ],
  'max_threshold' : [
    10000,1500, 1400, 2700,1000,
    5000, 5000, 10000, 5000, 25000,
    1000000, 1000000, 1000000
  ],
  'source' : [
    "USGS","USGS", "USGS", "USGS", "USGS",
    "USGS", "USGS","USGS", "USGS", "USGS",
    "USGS", "USGS", "CDSS"
  ]
  }
  )

  # add unique ID column  
  tbl["uid"] = np.where(pd.isnull(tbl["abbrev"]) , tbl["gage_id"], tbl["abbrev"])
  
  # rearrange table columns
  tbl = tbl[['river', 'reach', 'abbrev', 'gage_id', 'uid', 'desc', 'min_threshold', 'max_threshold', 'source']]
  
  return tbl

def get_flows(
        gage_table = None, 
        start_date = "1980-01-01",
        end_date   = None
        ):
          
    if gage_table is None:
        gage_table = gage_tbl()
      
    # if no end date is given, default to current date
    if end_date is None: 
        end_date   = datetime.date.today().strftime("%Y-%m-%d")
    
    # empty dataframe to concatenate surface water data from multiple locations
    sw_df = pd.DataFrame()

    for i in range(len(gage_table)):
        # if pandas dataframe abbrev col val is Nan, assign abbrev to None, otherwise use abbrev value
        if pd.isnull(gage_table.loc[i, ].abbrev):
            abbrev = None
        else:
            abbrev = gage_table.loc[i, ].abbrev

        # if pandas dataframe gage_id col val is Nan, assign gage_id to None, otherwise use gage_id value
        if pd.isnull(gage_table.loc[i, ].gage_id):
            usgs_id = None
        else:
            usgs_id = gage_table.loc[i, ].gage_id

        print(
            i + 1, "/", len(gage_table), 
            "\nRiver: ", gage_table.loc[i, ].river, 
            "\nUSGS ID: ", usgs_id, 
            "\nAbbrev: ", abbrev
            )

        # get surface water data 
        sw = cdsspy.get_sw_ts(
            abbrev     = abbrev, 
            usgs_id    = usgs_id,
            start_date = start_date,
            end_date   = end_date
            )
        
        # add unique identifer column
        sw["uid"] = gage_table.loc[i, ].uid
        
        # add boatable_days column to surface water observations
        sw = get_boatable_days(sw)
        
        # bind output dataframe to single output df
        sw_df = pd.concat([sw_df, sw])
        
        print("--------------------------------------------------")
    
    return(sw_df)

def get_boatable_days(
  df        = None,
  threshold = None
  ):
    
  # # input dataframe
  # df = flow_df
  # # threshold vals
  # threshold = [200, 1500]
  
  # if no threshold vector is given, get threshold values from gage_tbl() function
  if threshold is None:

    min_threshold = gage_tbl()[gage_tbl()["uid"] == df["uid"].values[0]].min_threshold.values[0]
    max_threshold = gage_tbl()[gage_tbl()["uid"] == df["uid"].values[0]].max_threshold.values[0]
  
  else: 
    
    # if threshold vector is given, use min value as lower bound and max value as upper bound
    min_threshold = min(threshold)
    max_threshold = max(threshold)

  # add boatable days 
  df["boatable_days"] = np.where((df["value"] >= min_threshold) & (df["value"] <= max_threshold), 1, 0)
  
  return df

# Roaring fork RICD CFS management plan flow/date table
def rf_ricdf():
  tbl = pd.DataFrame({
    'order' :[1, 2, 3, 4, 5, 6, 7],
    'start_mon' :["mar", "apr", "may", "may", "july", "august", "nov"],
    'end_mon' :["apr", "apr", "may", "july", "july", "oct", "nov"],
    'start_day' :[15, 15, 1, 15, 15, 1, 1],
    'end_day' :[14, 30, 14, 14, 31, 31, 30],
    'start_date' :["03-15", "04-15", "05-01", "05-15", "07-15", "08-01", "11-01"],
    'end_date' :["04-14", "04-30", "05-14", "07-14", "07-31", "10-31", "11-30"],
    'flow_rate' :[230, 310, 575, 1000, 575, 310, 230]
    })
    
  return tbl

