
### load in libraries and data -----------------------------------------------------------------------------------------------
require(dplyr)
require(lubridate)
require(data.table)
setDTthreads(2)

# read in geolocation points
geolocation_data <- fread("geolocation_data.csv") # paths & filenames have been changed for this archive

# read in event data (stops and tracks)
event_data <- fread("event_data.csv")


### remove incomplete event data ---------------------------------------------------------------------------------------------

# copy of original data.table for prepping
events_prepped <- copy(event_data)

# ensure start/endtime are in correct format and remove events with missing start/end
events_prepped[, start_time := as.POSIXct(start_time)]
events_prepped[, end_time := as.POSIXct(end_time)]
events_prepped <- events_prepped[!is.na(events_prepped$start_time) & !is.na(events_prepped$end_time),]

# remove events from users of which no geolocation data is available
events_prepped <- events_prepped[user_id %in% unique(geolocations_prepped_2223_v2$user_id)]

# remove events that are neither tracks or stops
events_prepped <- events_prepped[label %in% c("track", "stop")]


### clean event data ---------------------------------------------------------------------------------------------------------

# arrange events by user and start time
setorder(events_prepped, user_id, start_time)
  
# translate labels and collapse car modes
events_prepped[, `:=`(
  label_track = fcase(
    label_track %in% c("auto.bestel", "auto.bstr", "auto.pas"), "car",
    label_track == "fiets","bike",
    label_track == "efiets","ebike",
    label_track == "trein", "train",
    label_track == "voet", "walk",
    label_track == "anders", "other",
    label_track == "" & label == "track", "unlabeled",
    label_track == "" & label != "track", "no_track",
    default = label_track
  ),
  label_stop = fcase(
    label_stop == "halenbrengen.goederen", "pickup_goods",
    label_stop == "halenbrengen.pers", "pickup_people",
    label_stop == "onderwijs", "education",
    label_stop == "thuis", "home",
    label_stop == "visitie", "visit",
    label_stop == "werk.bet", "paid_work",
    label_stop == "werk.onb", "unpaid_work",
    label_stop == "winkelen", "shopping",
    label_stop == "overstap", "transfer",
    label_stop == "overig", "other",
    label_stop == "" & label == "stop", "unlabeled",
    label_stop == "" & label != "stop", "no_stop",
    default = label_stop
  ),
   
  # add event duration and date
  duration_hours = as.numeric(difftime(end_time, start_time, units = "hours")),
  duration_mins = as.numeric(difftime(end_time, start_time, units = "mins")),
  duration_secs = as.numeric(difftime(end_time, start_time, units = "secs")),
  start_day = date(start_time),
  end_day = date(end_time)
)]

# compute number of labeled tracks per user (excluding "other")
events_prepped[, n_labeled_tracks := sum(label_track %in% c("bike", "bus", "car", "ebike", "tram", "train", "walk", "metro")),
               by = user_id]

# remove users with no labeled tracks
events_prepped <- events_prepped[n_labeled_tracks > 0]

# generate original event_id for remaining events
events_prepped[, event_id := .I]


### prepare geolocation data -------------------------------------------------------------------------------------------------

# copy of original data.table for pre-processing
geolocations_prepped <- copy(geolocation_data)

# ensure timestamp column is in correct format and arrange observations by user and timestamp
geolocations_prepped[, timestamp := as.POSIXct(timestamp)]
setorder(geolocations_prepped, user_id, timestamp)

# remove duplicate timestamps by taking the geolocation with best (lowest) accuracy
geolocations_prepped <- geolocations_prepped[geolocations_prepped[, .I[which.min(accuracy)], by = .(user_id, timestamp)]$V1]

# match geolocations to events and remove unmatched
geolocations_prepped[events_prepped,
                     on = .(user_id, timestamp >= start_time, timestamp <= end_time),
                     event_id := i.event_id]


### add N matched geolocations to event data ---------------------------------------------------------------------------------

events_prepped[, N := geolocations_prepped[, .N, by = event_id][.SD, on = "event_id", N]]
events_prepped[is.na(N), N := 0]
