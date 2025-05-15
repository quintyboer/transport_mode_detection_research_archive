

### function to compute temporal features with gps data ----------------------------------------------------------------------

# adds previous stop purpose & duration, up to 3 lagged trip duration & mode, 
# daily trip number and cumulative travel time/distance, whether a trip was made during rush hour (+ proportion),
# day of the week, user baselines for duration/distance and mean/median/sd/iqr/p90/095 speed,
# and mode-specific user baselines for walk/car/bike speed
# output: events dt with gps & temporal features
create_temporal_features <- function(events, # events dt with gps features
                                     with_stops = FALSE, # whether to include stop information (not available for UU data)
                                     events_stops = NULL) { # if with_stops, the events dt that has both tracks & stops
  
  # add features based on past stop information
  if(with_stops) {
    
    events_temp <- copy(events_stops)
  
    # indicator whether previous event was a stop
    events_temp[, prev_stop_bin := shift(label) == "stop", by = user_id]
  
    # previous stop label and duration
    events_temp[, `:=`(
      prev_stop_label = fcase(prev_stop_bin, as.character(shift(label_stop)),
                              is.na(shift(label)), "first_event",
                              shift(label) == "track", "no_stop"),
      prev_stop_duration = fifelse(prev_stop_bin, shift(duration_mins), NA)
    ), by = user_id]
    
    # one-hot encoding of previous stop label
    stop_labels <- model.matrix(~ factor(prev_stop_label) - 1, data = events_temp)
    colnames(stop_labels) <- paste0("prev_stop_", sub("factor\\(prev_stop_label\\)", "", colnames(stop_labels)))
    events_temp <- cbind(events_temp, as.data.table(stop_labels))
    
    # add features to gps data
    cols_to_merge <- c("event_id", grep("^prev_stop", names(events_temp), value = TRUE))
    events <- merge(events,
                        events_temp[, ..cols_to_merge],
                        by = "event_id",
                        all.x = TRUE)
    }
  
  # previous track labels & duration
  events[, `:=`(
    label_track_1 = fifelse(is.na(shift(label_track, 1)), "first_track", as.character(shift(label_track, 1))),
    label_track_2 = fifelse(is.na(shift(label_track, 2)), "first_tracks", as.character(shift(label_track, 2))),
    label_track_3 = fifelse(is.na(shift(label_track, 3)), "first_tracks", as.character(shift(label_track, 3))),
    duration_mins_1 = shift(duration_mins, 1),
    duration_mins_2 = shift(duration_mins, 2),
    duration_mins_3 = shift(duration_mins, 3)
  ), by = user_id]
  
  # one-hot encoding of lagged track labels
  for(i in 1:3) {
    col <- paste0("label_track_", i)
    track_labels <- model.matrix(~ factor(get(col)) -1, data = events)
    colnames(track_labels) <- paste0("track_lag", i, "_",
                                     sub("factor\\(get\\(col\\)\\)", "", colnames(track_labels)))
    events <- cbind(events, as.data.table(track_labels))
  }
  
  # daily features: trip number and cumulative duration/distance
  events[, `:=`(
    daily_trip_number = seq_len(.N),
    cumulative_duration = shift(cumsum(duration_mins), fill = 0),
    cumulative_distance = shift(cumsum(tot_distance), fill = 0)
  ), by = .(user_id, start_day)]
  
  # rush hour features
  # times based on ANWB: morning rush from 6:30 to 9:30, evening rush from 15:30 to 19:00
  morning_start <- as.ITime("06:30")
  morning_end <- as.ITime("09:30")
  evening_start <- as.ITime("15:30")
  evening_end <- as.ITime("19:00")
  
  # add start time (without date) as separate column
  events[, `:=`(
    start_time_i = as.ITime(start_time),
    end_time_i = as.ITime(end_time)
  )]
  
  # indicator whether trip is made during rush hour, and proportion of trip during rush hour
  events[, `:=`(
    morning_rush_bin = (start_time_i >= morning_start & start_time_i <= morning_end) | 
      (end_time_i >= morning_start & end_time_i <= morning_end),
    evening_rush_bin = (start_time_i >= evening_start & start_time_i <= evening_end) | 
      (end_time_i >= evening_start & end_time_i <= evening_end),
    morning_rush_prop = pmax(0, as.numeric(pmin(end_time_i, morning_end) - pmax(start_time_i, morning_start))) / duration_secs,
    evening_rush_prop = pmax(0, as.numeric(pmin(end_time_i, evening_end) - pmax(start_time_i, evening_start))) / duration_secs
  )]
  
  # day of the week indicator
  weekday <- model.matrix(~ lubridate::wday(start_time, label = TRUE) - 1, data = events)
  colnames(weekday) <- c("day_sun", "day_mon", "day_tue", "day_wed", "day_thu", "day_fri", "day_sat")
  events <- cbind(events, as.data.table(weekday))
  
  # user baselines (cumulative mean) for speed
  events[, `:=`(
    cummean_avg_speed = shift(cummean(avg_speed_kmh)),
    cummean_p90_speed = shift(cummean(p90_speed_kmh)),
    cummean_p95_speed = shift(cummean(p95_speed_kmh)),
    cummean_med_speed = shift(cummean(med_speed_kmh)),
    cummean_std_speed = shift(cummean(std_speed_kmh)),
    cummean_iqr_speed = shift(cummean(iqr_speed_kmh)),
    
  # user baselines (cumulative mean) for duration/distance
    cummean_duration_mins = shift(cummean(duration_mins)),
    cummean_tot_distance = shift(cummean(tot_distance))
  ), by = user_id]
  
  # compute difference of current track with user baseline
  events[, `:=`(
    diff_avg_speed = avg_speed_kmh - cummean_avg_speed,
    diff_p90_speed = p90_speed_kmh - cummean_p90_speed,
    diff_p95_speed = p95_speed_kmh - cummean_p95_speed,
    diff_med_speed = med_speed_kmh - cummean_med_speed,
    diff_std_speed = std_speed_kmh - cummean_std_speed,
    diff_iqr_speed = iqr_speed_kmh - cummean_iqr_speed,
    diff_duration_mins = duration_mins - cummean_duration_mins,
    diff_tot_distance = tot_distance - cummean_tot_distance
  ), by = user_id]
  
  # mode-specific user baseline speed (car, walk, bike)
  events[label_track == "walk",
             cummean_speed_walk := shift(cummean(avg_speed_kmh)),
             by = user_id]
  events[label_track == "bike",
             cummean_speed_bike := shift(cummean(avg_speed_kmh)),
             by = user_id]
  events[label_track == "car",
             cummean_speed_car := shift(cummean(avg_speed_kmh)),
             by = user_id]
  
  # fill forward last observed value
  events[, `:=`(
    cummean_speed_walk = nafill(cummean_speed_walk, type = "locf"),
    cummean_speed_bike = nafill(cummean_speed_bike, type = "locf"),
    cummean_speed_car = nafill(cummean_speed_car, type = "locf")
  ), by = user_id]
  
  # compute difference from mode-specific baseline
  events[, `:=`(
    speed_diff_walk = avg_speed_kmh - cummean_speed_walk,
    speed_diff_bike = avg_speed_kmh - cummean_speed_bike,
    speed_diff_car = avg_speed_kmh - cummean_speed_car
  ), by = user_id]
  
  # change logical columns to numeric
  if(with_stops) events[, prev_stop_bin := as.numeric(prev_stop_bin)]
  events[, `:=`(
    morning_rush_bin = as.numeric(morning_rush_bin),
    evening_rush_bin = as.numeric(evening_rush_bin)
  )]
  
  
  # remove unnecessary columns
  if(with_stops) events[, prev_stop_label := NULL]
  events[, c("label_track_1", "label_track_2", "label_track_3",
                 "start_time_i", "end_time_i") := NULL]
  
  return(events)
}


### function to compute temporal features with osm data ----------------------------------------------------------------------

# adds user baselines for normalized number of overlap with public transport stations & routes
# output: events dt with gps, osm & temporal features
create_temporal_osm_features <- function(events) { # events dt with osm data
  
  # add user baseline (cumulative mean) for normalized num. encounters with public transport routes/stations
  events[, `:=`(
      cummean_norm_num_bus_stations = shift(cummean(normalized_num_bus_stations)),
      cummean_norm_num_metro_stations = shift(cummean(normalized_num_metro_stations)),
      cummean_norm_num_train_stations = shift(cummean(normalized_num_train_stations)),
      cummean_norm_num_tram_stations = shift(cummean(normalized_num_tram_stations)),
      cummean_norm_num_bus_routes = shift(cummean(normalized_num_bus_routes)),
      cummean_norm_num_metro_routes = shift(cummean(normalized_num_metro_routes)),
      cummean_norm_num_train_routes = shift(cummean(normalized_num_train_routes)),
      cummean_norm_num_tram_routes = shift(cummean(normalized_num_tram_routes))
    ), by = user_id]

    # compute difference from user baseline
    events[, `:=`(
      diff_norm_num_bus_stations = normalized_num_bus_stations - cummean_norm_num_bus_stations,
      diff_norm_num_metro_stations = normalized_num_metro_stations - cummean_norm_num_metro_stations,
      diff_norm_num_train_stations = normalized_num_train_stations - cummean_norm_num_train_stations,
      diff_norm_num_tram_stations = normalized_num_tram_stations - cummean_norm_num_tram_stations,
      diff_norm_num_bus_routes = normalized_num_bus_routes - cummean_norm_num_bus_routes,
      diff_norm_num_metro_routes = normalized_num_metro_routes - cummean_norm_num_metro_routes,
      diff_norm_num_train_routes = normalized_num_train_routes - cummean_norm_num_train_routes,
      diff_norm_num_tram_routes = normalized_num_tram_routes - cummean_norm_num_tram_routes
    ), by = user_id]
    
    return(events)
  }


### add temporal features to events ------------------------------------------------------------------------------------------

# gps + temp features
events_gps_temp <- copy(events_gps)
events_gps_temp <- create_temporal_features(events_gps_temp, with_stops = TRUE, events_all)

# gps + osm + temp features
events_gps_osm_temp <- copy(events_gps_osm)
events_gps_osm_temp <- create_temporal_features(events_gps_osm_temp, with_stops = TRUE, events_all)
events_gps_osm_temp <- create_temporal_osm_features(events_gps_osm_temp)


# add temporal features to external validation data --------------------------------------------------------------------------

# gps + temp
events_uu_gps_temp <- copy(events_uu_gps)
events_uu_gps_temp <- create_temporal_features(events_uu_gps_temp, with_stops = FALSE)

# gps + osm + temp
events_uu_gps_osm_temp <- copy(events_uu_gps_osm)
events_uu_gps_osm_temp <- create_temporal_features(events_uu_gps_osm_temp, with_stops = FALSE)
events_uu_gps_osm_temp <- create_temporal_osm_features(events_uu_gps_osm_temp)

