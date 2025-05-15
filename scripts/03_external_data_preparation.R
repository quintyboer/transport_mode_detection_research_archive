
# load data ------------------------------------------------------------------------------------------------------------------

# read in geolocations
uu_anonymized <- fread("external_validation_data_detailed.csv") # paths & filenames have been changed for this archive
uu_labeled <- fread("external_validation_data_labeled.csv")

# read in diaries/events
uu_diaries <- fread("external_validation_events.csv")


### prepare diary data -------------------------------------------------------------------------------------------------------

diaries_uu <- copy(uu_diaries)

# ensure start/endtime have correct format & column name
diaries_uu[, start_time := as.POSIXct(start_timestamp)]
diaries_uu[, end_time := as.POSIXct(end_timestamp)]

# arrange events by user and start time
setorder(diaries_uu, user_id, start_time)

# remove events with missing start or end times
diaries_uu <- diaries_uu[!is.na(diaries_uu$start_time) & !is.na(diaries_uu$end_time),]

# change mode to label_track and edit labels
diaries_uu[, label_track := tolower(mode)]

# add event duration, start/end day, and general label
diaries_uu[, `:=`(
  duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")),
  duration_mins = as.numeric(difftime(end_time, start_time, units = "mins")),
  duration_secs = as.numeric(difftime(end_time, start_time, units = "secs")),
  start_day = date(start_time),
  end_day = date(end_time),
  label = "track"
)]

# compute number of labeled tracks per user (excluding "other")
diaries_uu[, n_labeled_tracks := sum(label_track %in% c("bike", "bus", "car", "ferry", "ebike", "tram", "train", "walk", "metro")),
               by = user_id]

# create additional user id column for merging with geolocation data
diaries_uu[, user_id_x := user_id]

# drop unused columns
diaries_uu[, c("start_timestamp", "end_timestamp", "mode", "user_id") := NULL]

# add event_id
diaries_uu[, event_id := .I]


### prepare geolocation data -------------------------------------------------------------------------------------------------

geolocations_uu <- copy(uu_anonymized)

# ensure timestamp column is in correct format and arrange observations by user and timestamp
geolocations_uu[, timestamp := as.POSIXct(timestamp)]
setorder(geolocations_uu, user_id, timestamp)
uu_labeled[, timestamp := as.POSIXct(timestamp)]
setorder(uu_labeled, user_id, timestamp)

# add mode & label from labeled data
geolocations_uu[, mode := as.character(mode)]
geolocations_uu[unique(uu_labeled), `:=`(
  stop = i.stop,
  mode = i.mode
), on = c("user_id", "timestamp", "longitude", "latitude")]

# change column names
setnames(geolocations_uu, c("longitude", "latitude", "Altitude"), c("lon", "lat", "altitude"))

# remove duplicate timestamps by taking the geolocation with best (lowest) accuracy
geolocations_uu <- geolocations_uu[geolocations_uu[, .I[which.min(accuracy)], by = .(user_id, timestamp)]$V1]


### match diaries with geolocations ------------------------------------------------------------------------------------------

# create additional user_id column for matching
geolocations_uu[, user_id_x := fifelse(user_id %in% c("Utrecht1", "Utrecht2", "Utrecht12"), "UtrechtX", user_id)]

# assign event_id to the geolocations and remove unmatched
geolocations_uu[diaries_uu,
                on = .(user_id_x, timestamp >= start_time, timestamp <= end_time),
                event_id := i.event_id]
geolocations_uu <- geolocations_uu[!is.na(event_id)]

# assign unique event_id's to Utrecht1/2/12
geolocations_uu[, event_id_unique := as.character(event_id)]
geolocations_uu[user_id_x == "UtrechtX", event_id_unique := paste0(event_id, ".", gsub("Utrecht", "", user_id))]


### create events data -------------------------------------------------------------------------------------------------------

# create geolocation summary
events_uu <- geolocations_uu[, .(
  user_id = first(user_id),
  start_time_updated = min(timestamp),
  end_time_updated = max(timestamp),
  N = .N,
  event_id = first(event_id)
), by = event_id_unique]

# merge with diary
events_uu <- merge(events_uu, diaries_uu, by = c("event_id"), all.x = TRUE)

# rename event_id columns
events_uu[, event_id := as.numeric(event_id_unique)]
events_uu[, event_id_unique := NULL]
geolocations_uu[, event_id := as.numeric(event_id_unique)]
geolocations_uu[, event_id_unique := NULL]

# correct column order
setcolorder(events_uu, "N", after = ncol(events_uu))


### filter event data for prediction -----------------------------------------------------------------------------------------

events_uu_pred <- filter_events(events_uu,
                             include_stops = FALSE,
                             include_unlabeled_tracks = FALSE,
                             include_misc_tracks = FALSE,
                             bike_collapse = TRUE,
                             max_duration_tracks = 10,
                             min_obs_tracks = 10)


### filter event data for feature creation -----------------------------------------------------------------------------------

events_uu_feat <- filter_events(events_uu,
                             include_stops = FALSE,
                             include_unlabeled_tracks = TRUE,
                             include_misc_track = TRUE,
                             bike_collapse = TRUE,
                             max_duration_tracks = 100,
                             min_obs_tracks = 5)

### save data ----------------------------------------------------------------------------------------------------------------

# needed to load into python for osm feature computation
fwrite(events_uu_feat, "data/events_uu_feat.csv")
fwrite(geolocations_uu_feat, "data/geolocations_uu_feat.csv")
