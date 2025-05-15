
### function to filter event data --------------------------------------------------------------------------------------------

# this function filters out events from the data, based on whether you want to include labeled or 'other' tracks and
# stops, maximum track duration, and minimum n geolocations. ALso option to collapse bike and e-bike categories.
# output is filtered data.table 
filter_events <- function(events, # dt with events
                          include_stops = FALSE, # include stops
                          include_unlabeled_tracks = FALSE, # include unlabeled tracks
                          include_misc_tracks = FALSE, # include miscellaneous transport labels
                          bike_collapse = TRUE, # combine bike and ebike transport labels
                          max_duration_tracks = 10, # maximum track duration in hours
                          min_obs_tracks = 10) { # minimum N geolocations for tracks

  setDT(events)
  
  # remove stops
  if(include_stops == FALSE) events <- events[label == "track"]
  
  # remove unlabeled tracks
  if(include_unlabeled_tracks == FALSE) events <- events[label_track != "unlabeled"]
  
  # remove miscellaneous transport modes
  if(include_misc_tracks == FALSE) events <- events[label_track != "other"]
  
  # combine bike and ebike
  if(bike_collapse == TRUE) events <- events[label_track %in% c("bike", "ebike"), label_track := "bike"]
  
  # remove tracks longer than max_duration_track and with less than min_obs_track
  events <- events[N >= min_obs_tracks & !(label == "track" & duration_hours > max_duration_tracks)]

  return(events)
}


### filter event data for prediction -----------------------------------------------------------------------------------------

events_pred <- filter_events(events_prepped,
                             include_stops = FALSE,
                             include_unlabeled_tracks = FALSE,
                             include_misc_tracks = FALSE,
                             bike_collapse = TRUE,
                             max_duration_tracks = 10,
                             min_obs_tracks = 10)


### filter event data for gps/osm features -----------------------------------------------------------------------------------

events_feat <- filter_events(events_prepped,
                             include_stops = FALSE,
                             include_unlabeled_tracks = TRUE,
                             include_misc_track = TRUE,
                             bike_collapse = TRUE,
                             max_duration_tracks = 100,
                             min_obs_tracks = 5)


### filter event data for temporal features ----------------------------------------------------------------------------------

events_all <- filter_events(events_prepped,
                            include_stops = TRUE,
                            include_unlabeled_tracks = TRUE,
                            include_misc_track = TRUE,
                            bike_collapse = TRUE,
                            max_duration_tracks = 100,
                            min_obs_track = 0)


### save data ----------------------------------------------------------------------------------------------------------------

# needed to load into python for osm feature computation
fwrite(events_feat, "data/events_feat.csv")
fwrite(geolocations_feat, "data/geolocations_feat.csv")

