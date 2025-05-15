
### load libraries -----------------------------------------------------------------------------------------------------------

require(e1071) # skewness and kurtosis
setDTthreads(2)


### function to compute summary statistics -----------------------------------------------------------------------------------

# computes mean, median, min, max, sd, skewness, kurtosis, interquartile range, coefficient of variaton,
# autocorrelation coefficient, and 1/5/10/90/95/99th percentile for a numeric vector
# output: list with summary statistics
summary_stats <- function(x, # numeric vector
                          column_name) { # variable name
  
  # remove NAs
  x <- na.omit(x)
  
  # compute statistics and store in list with names 'statistic_variable'
  stats <- setNames(
    list(avg = ifelse(length(x) > 0, mean(x), as.numeric(NA)),
         med = median(x),
         min = ifelse(length(x) > 0, min(x), as.numeric(NA)),
         max = ifelse(length(x) > 0, max(x), as.numeric(NA)),
         std = sd(x),
         skw = ifelse(length(unique(x)) > 1, skewness(x), as.numeric(NA)),
         kur = ifelse(length(unique(x)) > 1, kurtosis(x), as.numeric(NA)),
         iqr = IQR(x),
         cv = ifelse(length(x) > 0, sd(x) / mean(x), as.numeric(NA)),
         acc = acf(x, lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2],
         p90 = quantile(x, 0.9),
         p95 = quantile(x, 0.95),
         p99 = quantile(x, 0.99),
         p10 = quantile(x, 0.1),
         p05 = quantile(x, 0.05),
         p01 = quantile(x, 0.01)
    ), paste0(c("avg", "med", "min", "max", "std", "skw", "kur", "iqr", "cv", "acc",
                "p90", "p95", "p99", "p10", "p05", "p01"), "_", column_name))
  return(stats)
}


### function to compute outlier information ----------------------------------------------------------------------------------

# defines outliers based on 1.5 IQR and computes proportion and mean of outliers
# output: list with outlier statistics
outlier_stats <- function(x, # numeric vector
                          column_name, # variable name
                          both = TRUE) { # compute outliers above upper and below lower boundaries
                                         # if FALSE: only compute upper (ex: for speed) 
  
  # remove NAs
  x <- na.omit(x)
  
  # define outlier boundaries
  threshold_low <- quantile(x, 0.25) - 1.5*IQR(x)
  threshold_high <- quantile(x, 0.75) + 1.5*IQR(x)
  
  # compute proportion/mean and output in list with names 'statistic_variable'
  if(both) { # both upper and lower outliers
    outliers <- setNames(
      list(
        prop_out_low = ifelse(length(x) > 0, mean(x < threshold_low), 0),
        prop_out_high = ifelse(length(x) > 0, mean(x > threshold_high), 0),
        avg_out_low = ifelse(length(x[x < threshold_low]) > 0, mean(x[x < threshold_low]), 0),
        avg_out_high = ifelse(length(x[x > threshold_high]) > 0, mean(x[x > threshold_high]), 0)
      ), paste0(c("prop_out_low", "prop_out_high", "avg_out_low", "avg_out_high"), "_", column_name))
  } else { # only upper outliers (ex: speed)
    outliers <- setNames(
      list(
        prop_out_high = ifelse(length(x) > 0, mean(x > threshold_high), 0),
        avg_out_high = ifelse(length(x[x > threshold_high]) > 0, mean(x[x > threshold_high]), 0)
      ), paste0(c("prop_out_high", "avg_out_high"), "_", column_name))
  }
  return(outliers)
}


### function to add gps summary stats to events ------------------------------------------------------------------------------

# computes summary statisics for geolocation columns speed, acceleration, jerk, snap, difference bearing, altitude and accuracy
# as well as total difference traveled, proportion of observations with large time differences,
# and proportion of speed & bearing difference within pre-specified intervals
# output: events dt with added columns for aggregated summary stats
create_gps_features <- function(events, # dt with events
                                geolocations) { # dt with geolocations
  
  # ensure input data is data.table
  setDT(events)
  setDT(geolocations)
  
  # group geolocations by event_id and store in separate data.table
  gps_summary <- geolocations[, c(
    
    # compute summary statistics for timestamp-level features
    summary_stats(speed_kmh, "speed_kmh"),
    summary_stats(acceleration, "acceleration"),
    summary_stats(jerk, "jerk"),
    summary_stats(snap, "snap"),
    summary_stats(bearing_diff, "bearing_diff"),
    summary_stats(altitude, "altitude"),
    summary_stats(accuracy, "accuracy"),
    
    # compute outlier information for speed & derivatives
    outlier_stats(speed_kmh, "speed_kmh", both = FALSE),
    outlier_stats(acceleration, "acceleration", both = TRUE),
    outlier_stats(jerk, "jerk", both = TRUE),
    outlier_stats(snap, "snap", both = TRUE),
    
    # compute other features
    list(
      
      # proportion of obs with large time gaps (quality check)
      prop_large_timediff = mean(large_timediff_flag, na.rm = T),
      
      # total distance traveled
      tot_distance = sum(distance_between_points, na.rm = T),
      
      # proportion of bearing diff in ranges
      prop_bearing_diff_0_5 = mean(bearing_diff > 0 & bearing_diff <= 5, na.rm = T),
      prop_bearing_diff_5_10 = mean(bearing_diff > 5 & bearing_diff <= 10, na.rm = T),
      prop_bearing_diff_10_15 = mean(bearing_diff > 10 & bearing_diff <= 15, na.rm = T),
      prop_bearing_diff_15_30 = mean(bearing_diff > 15 & bearing_diff <= 30, na.rm = T),
      prop_bearing_diff_30_45 = mean(bearing_diff > 30 & bearing_diff <= 45, na.rm = T),
      prop_bearing_diff_45_90 = mean(bearing_diff > 45 & bearing_diff <= 90, na.rm = T),
      prop_bearing_diff_90 = mean(bearing_diff > 90),
      
      # proportion of speed in ranges
      prop_speed_0_3 = mean(speed_kmh > 0 & speed_kmh <= 3, na.rm = T),
      prop_speed_3_8 = mean(speed_kmh > 3 & speed_kmh <= 8, na.rm = T),
      prop_speed_8_15 = mean(speed_kmh > 8 & speed_kmh <= 15, na.rm = T),
      prop_speed_15_30 = mean(speed_kmh > 15 & speed_kmh <= 30, na.rm = T),
      prop_speed_30_50 = mean(speed_kmh > 30 & speed_kmh <= 50, na.rm = T),
      prop_speed_50_80 = mean(speed_kmh > 50 & speed_kmh <= 80, na.rm = T),
      prop_speed_80_120 = mean(speed_kmh > 80 & speed_kmh <= 120, na.rm = T),
      prop_speed_120 = mean(speed_kmh > 120, na.rm = T)
    )
  ), by = event_id]
  
  # merge gps summary stats with events dataset
  events <- merge(events, gps_summary, by = "event_id", all.x = TRUE)
  return(events)
}


### compute gps features -----------------------------------------------------------------------------------------------------

events_gps <- create_gps_features(events_feat, geolocations_feat)


### compute gps features for external validation data ------------------------------------------------------------------------

events_uu_gps <- create_gps_features(events_uu_feat, geolocations_uu_feat)

