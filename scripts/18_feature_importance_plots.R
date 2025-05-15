
### variable importance of RF with all features ------------------------------------------------------------------------------

# retrieve variable importance, sorted by feature and by importance
rf_varimp_byfeat <- as.data.table(varImp(rf_all_final)$importance, keep.rownames = "Feature")
rf_varimp <- as.data.table(varImp(rf_all_final)$importance %>% arrange(desc(Overall)), keep.rownames = "Feature")

# top 20 most important variables
rf_varimp_20 <- rf_varimp[1:20]


### create rf variable importance plot ---------------------------------------------------------------------------------------

# add full label of top 20 features
rf_feature_labels <- c(p95_speed_kmh = "95th percentile speed",
                       p90_speed_kmh = "90th percentile speed",
                       iqr_speed_kmh = "Interquartile range speed",
                       std_speed_kmh = "Std. dev. speed",
                       avg_speed_kmh = "Mean speed",
                       prop_speed_30_50 = "Proportion of speed between 30-50 km/h",
                       p99_speed_kmh = "99th percentile speed",
                       med_speed_kmh = "Median speed",
                       prop_speed_15_30 = "Proportion of speed between 15-30 km/h",
                       prop_speed_3_8 = "Proportion of speed between 3-8 km/h",
                       prop_speed_50_80 = "Proportion of speed between 50-80 km/h",
                       track_lag1_car = "Previous transport mode car",
                       prop_speed_80_120 = "Proportion of speed between 80-120 km/h",
                       max_speed_kmh = "Maximum speed",
                       duration_mins = "Trip duration in minutes",
                       tot_distance = "Total distance traveled",
                       prop_speed_8_15 = "Proportion of speed between 8-15 km/h",
                       track_lag1_bike = "Previous transport mode bike",
                       mean_distance_bus = "Mean distance to bus route",
                       std_distance_bus = "Std. dev. to bus route")

rf_varimp_20[, feature_label := rf_feature_labels[Feature]]

# create feature importance barplot of 20 most important rf variables
plot_rf_varimp <- ggplot(rf_varimp_20, aes(x = reorder(feature_label, Overall), y = Overall, fill = Overall)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis(option = "D", direction = -1) +
  labs(x = "Feature",
       y = "Permutation variable importance") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8),
    legend.position = "none"
  )

plot_rf_varimp

# save plot
save_plot(plot_rf_varimp, filename = "output/fig6_rf_feature_importance.png")


### variable importance of XGB with all features -----------------------------------------------------------------------------

# retrieve variable importance, sorted by gain
xgb_varimp <- as.data.table(xgb.importance(model = xgb_all_final))
xgb_varimp_20 <- xgb_varimp[1:20]


### create xgb var imp plot --------------------------------------------------------------------------------------------------

# add full label of top 20 features
xgb_feature_labels <- c(
  mean_distance_metro = "Mean distance to metro route",
  p90_speed_kmh = "90th percentile speed",
  mean_distance_bus = "Mean distance to bus route",
  mean_distance_tram = "Mean distance to tram route",
  min_distance_train = "Minimum distance to train route",
  prop_speed_15_30 = "Proportion of speed between 15-30 km/h",
  p95_speed_kmh = "95th percentile speed",
  speed_diff_walk = "Difference from user baseline walk speed",
  speed_diff_bike = "Difference from user baseline bike speed",
  avg_speed_kmh = "Mean speed",
  track_lag1_car = "Previous transport mode car",
  speed_diff_car = "Difference from user baseline car speed",
  prop_speed_8_15 = "Proportion of speed between 8-15 km/h", # not in previous varimp plot
  dist_to_metro_station_end = "Distance to nearest metro station at trip end", # not in previous
  min_distance_bus = "Minimum distance to bus route",
  iqr_speed_kmh = "Interquartile range speed", # not in previous
  dist_to_tram_station_start = "Distance to nearest tram station at trip start",
  num_train_stations = "Normalized number of train stations on route",
  prop_speed_3_8 = "Proportion of speed between 3-8 km/h",
  tot_distance = "Total distance traveled") # not in previous

# in previous plot: max_distance_tram, min_distance_tram, duration_mins, std_distance_metro

xgb_varimp_20[, Feature := xgb_feature_labels[Feature]]

# create plot
plot_xgb_varimp <- ggplot(xgb_varimp_20, aes(x = reorder(Feature, Gain), y = Gain, fill = Gain)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis(option = "D", direction = -1) +
  labs(x = "Feature",
       y = "Gain (Improvement in Accuracy)") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8),
    legend.position = "none"
  )

save_plot(plot_xgb_varimp, filename = "output/fig7_xgb_feature_importance.png")

