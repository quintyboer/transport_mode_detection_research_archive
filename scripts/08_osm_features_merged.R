
### load in osm datasets -----------------------------------------------------------------------------------------------------

osm_datasets <- list(
  
  # (normalized) counts of intersection with public transport routes
  bus_route_counts = fread("osm_bus_route_counts.csv"), # paths have been removed for research archive
  metro_route_counts = fread("osm_metro_route_counts.csv"),
  train_route_counts = fread("osm_train_route_counts.csv"),
  tram_route_counts = fread("osm_tram_route_counts.csv"),
  bike_route_counts = fread("osm_bike_route_counts.csv"),
  
  # avg/min/max/sd distance to public transport routes (intersections only)
  bus_route_prox = fread("osm_bus_route_prox.csv"),
  metro_route_prox = fread("osm_metro_route_prox.csv"),
  train_route_prox = fread("osm_train_route_prox.csv"),
  tram_route_prox = fread("osm_tram_route_prox.csv"),
  bike_route_prox = fread("osm_bike_route_prox.csv"),
  
  # (normalized) counts of intersections with public transport stations 
  bus_station_counts = fread("osm_bus_station_counts.csv"),
  metro_station_counts = fread("osm_metro_station_counts.csv"),
  train_station_counts = fread("osm_train_station_counts.csv"),
  tram_station_counts = fread("osm_tram_station_counts.csv"),
  
  # proximity to public transport stations and parking amenities and trip start/end
  public_transport_stations_prox = fread("osm_public_transport_stations_prox.csv"),
  parking_amenities_prox = fread("osm_parking_amenities_prox.csv"),
  
  # (normalized) counts of intersections with traffic indicators
  traffic_crossing_counts = fread("osm_traffic_crossing_counts.csv"),
  traffic_mini_roundabout_counts = fread("osm_traffic_mini_roundabout_counts.csv"),
  traffic_motorway_junction_counts = fread("osm_traffic_motorway_junction_counts.csv"),
  traffic_signals_counts = fread("osm_traffic_signals_counts.csv"),
  traffic_speed_camera_counts = fread("osm_traffic_speed_camera_counts.csv"),
  traffic_stop_counts = fread("osm_traffic_stop_counts.csv"),
  traffic_street_lamp_counts = fread("osm_traffic_street_lamp_counts.csv"),
  traffic_turning_circle_counts = fread("osm_traffic_turning_circle_counts.csv")
)


### add osm features to events -----------------------------------------------------------------------------------------------

# create copy of events_gps to add osm features to
events_gps_osm <- copy(events_gps)

# merge osm datasets based on event_id
for(dt in osm_datasets) events_gps_osm <- merge(events_gps_osm, dt, by = "event_id", all.x = TRUE)


### Replace missing values in osm data ---------------------------------------------------------------------------------------

# replace counts and normalized counts with 0
replace_with_zero <- colnames(events_gps_osm %>% select(starts_with(c("num_", "normalized_"))))
setnafill(events_gps_osm, cols = replace_with_zero, fill = 0)


### load in osm datasets for external validation data ------------------------------------------------------------------------

uu_osm_datasets <- setNames(
  lapply(names(osm_datasets), function(name) fread(paste0("osm_", name, "_uu.csv"))),
  names(osm_datasets))


### add osm features to events (uu data) -------------------------------------------------------------------------------------

# create copy of events_uu_gps to add osm features to
events_uu_gps_osm <- copy(events_uu_gps)


# merge osm datasets based on event_id
for(dt in osm_datasets) {
  
  # create new dt for previously empty dt's (mini roundabout counts)
  if (nrow(dt) == 0) {
    empty_dt <- data.table(event_id = events_uu_gps_osm$event_id)
    for (col in setdiff(names(dt), "event_id")) {
      empty_dt[[col]] <- as.numeric(NA)
    }
    dt <- empty_dt
  }
  
  # merge dt
  events_uu_gps_osm <- merge(events_uu_gps_osm, dt, by = "event_id", all.x = TRUE)
}


### Replace missing values in osm data (uu) ----------------------------------------------------------------------------------

# replace counts and normalized counts with 0
setnafill(events_uu_gps_osm, cols = replace_with_zero, fill = 0)

# restructure NA columns (no observations with train overlap)
events_uu_gps_osm$dist_to_train_station_start <- as.numeric(events_uu_gps_osm$dist_to_train_station_start)
events_uu_gps_osm$dist_to_train_station_end <- as.numeric(events_uu_gps_osm$dist_to_train_station_end)

