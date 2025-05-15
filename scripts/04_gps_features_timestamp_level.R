
### load libraries -----------------------------------------------------------------------------------------------------------
require(geosphere)
setDTthreads(2)


### function to compute timestamp-level information --------------------------------------------------------------------------

# compute distance between points and based on this the speed, acceleration, jerk, snap,
# absolute bearing and difference in bearing
# output is data.table with added columns
timestamp_features <- function(geolocations, # dt with geolocations
                               distance = "haversine", # can be either 'haversine' or 'vincenty'
                               timediff_flag_sec = 300) { # flag timestamps with more than 300 secs (5 min) no measurements
  
  # ensure data is in DT format and ordered by event_id/timestamp
  setDT(geolocations)
  setorder(geolocations, event_id, timestamp)
  
  # compute distance between consecutive geolocations (default method = "haversine")
  if(distance == "haversine") {
    geolocations[, distance_between_points := distHaversine(p1 = matrix(c(shift(lon), shift(lat)), ncol = 2),
                                                            p2 = matrix(c(lon, lat), ncol = 2)),
                 by = event_id]
  } else if (distance == "vincencty") {
    geolocations[, distance_between_points := distVincentyEllipsoid(p1 = matrix(c(shift(lon), shift(lat)), ncol = 2),
                                                                    p2 = matrix(c(lon, lat), ncol = 2)),
                 by = event_id]
  } else {
    stop("distance should be 'haversine' (default) or 'vincenty'")
  }
  
  # compute time difference and flag large differences
  geolocations[, `:=`(
    timediff_sec = as.numeric(difftime(timestamp, shift(timestamp), units = "secs")),
    timediff_hours = as.numeric(difftime(timestamp, shift(timestamp), units = "hours"))
  ), by = event_id]
  geolocations[, large_timediff_flag := timediff_sec > timediff_flag_sec]
  geolocations[is.na(large_timediff_flag), large_timediff_flag := FALSE]
  
  # compute speed, acceleration, jerk, and snap
  geolocations[, speed_ms := distance_between_points / timediff_sec]
  geolocations[, speed_kmh := speed_ms * 3.6]
  geolocations[, acceleration := (speed_kmh - shift(speed_kmh)) / timediff_hours, by = event_id]
  geolocations[, jerk := (acceleration - shift(acceleration)) / timediff_hours, by = event_id]
  geolocations[, snap := (jerk - shift(jerk)) / timediff_hours, by = event_id]
  
  # compute bearing
  geolocations[, bearing_abs := ifelse(speed_ms > 0.8, # only compute bearing when device is in motion
                                       bearing(p1 = matrix(c(shift(lon), shift(lat)), ncol = 2),
                                               p2 = matrix(c(lon, lat), ncol = 2)), NA),
               by = event_id]
  
  # fill missing values with next/previous bearing values
  geolocations[, bearing_abs := nafill(bearing_abs, type = "locf"), by = event_id]
  geolocations[, bearing_abs := nafill(bearing_abs, type = "nocb"), by = event_id]
  
  # compute shortest angular distance to get difference in bearing
  geolocations[, bearing_diff := ifelse(is.na(bearing_abs) | is.na(shift(bearing_abs)), 0,
                                        abs((bearing_abs - shift(bearing_abs) + 180) %% 360 - 180)),
               by = event_id]
  
  return(geolocations)
}


### add timestamp level information to geolocation data ----------------------------------------------------------------------

# select geolocations associated with tracks
geolocations_feat <- geolocations_prepped[event_id %in% events_feat$event_id]

# add timestamp-level features
geolocations_feat <- timestamp_features(geolocations_feat,
                                        distance = "haversine",
                                        timediff_flag_sec = 120) # 2 minutes


### add timestamp level information to external validation data --------------------------------------------------------------

# select geolocations associated with tracks
geolocations_uu_feat <- geolocations_uu[event_id %in% events_uu_feat$event_id]

# add timestamp-level features
geolocations_uu_feat <- timestamp_features(geolocations_uu_feat,
                                           distance = "haversine",
                                           timediff_flag_sec = 120) # 2 minutes

