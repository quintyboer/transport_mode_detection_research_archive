
### random forest results with UU data ---------------------------------------------------------------------------------------

results_uu_rf_gps <- get_rf_results(testdata_uu_gps, rf_gps_final)
results_uu_rf_osm <- get_rf_results(testdata_uu_osm, rf_osm_final)
results_uu_rf_temp <- get_rf_results(testdata_uu_temp, rf_temp_final)
results_uu_rf_all <- get_rf_results(testdata_uu_all, rf_all_final)


# store evaluation metrics (adjust factor levels to include car for label_track and ferry for predicted)
metrics_uu_rf_gps <- get_metrics(factor(results_uu_rf_gps$predicted,
                                        levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")),
                                 factor(results_uu_rf_gps$label_track,
                                        levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")))
metrics_uu_rf_osm <-  get_metrics(factor(results_uu_rf_osm$predicted,
                                         levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")),
                                  factor(results_uu_rf_osm$label_track,
                                         levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")))
metrics_uu_rf_temp <-  get_metrics(factor(results_uu_rf_temp$predicted,
                                          levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")),
                                   factor(results_uu_rf_temp$label_track,
                                          levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")))
metrics_uu_rf_all <- get_metrics(factor(results_uu_rf_all$predicted,
                                        levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")),
                                 factor(results_uu_rf_all$label_track,
                                        levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")))


### create xgb matrices for UU test data -------------------------------------------------------------------------------------

# xgb test matrix with gps features
testmatrix_uu_gps <- xgb.DMatrix(data = as.matrix(testdata_uu_gps[, !"label_track"]))

# xgb test matrix with gps + osm features
testmatrix_uu_osm <- xgb.DMatrix(data = as.matrix(testdata_uu_osm[, !"label_track"]))

# xgb test matrix with gps + temporal features
testmatrix_uu_temp <- xgb.DMatrix(data = as.matrix(testdata_uu_temp[, !"label_track"]))

# xgb test matrix with all features
testmatrix_uu_all <- xgb.DMatrix(data = as.matrix(testdata_uu_all[, !"label_track"]))


### XGB results with UU data -------------------------------------------------------------------------------------------------

results_uu_xgb_gps <- get_xgb_results(testmatrix_uu_gps, testdata_uu_gps, xgb_gps_final)
results_uu_xgb_osm <- get_xgb_results(testmatrix_uu_osm, testdata_uu_osm, xgb_osm_final)
results_uu_xgb_temp <- get_xgb_results(testmatrix_uu_temp, testdata_uu_temp, xgb_temp_final)
results_uu_xgb_all <- get_xgb_results(testmatrix_uu_all, testdata_uu_all, xgb_all_final)


# store evaluation metrics (adjust factor levels to include car for label_track and ferry for predicted)
metrics_uu_xgb_gps <- get_metrics(factor(results_uu_xgb_gps$predicted,
                                        levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")),
                                 factor(results_uu_xgb_gps$label_track,
                                        levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")))
metrics_uu_xgb_osm <-  get_metrics(factor(results_uu_xgb_osm$predicted,
                                         levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")),
                                  factor(results_uu_xgb_osm$label_track,
                                         levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")))
metrics_uu_xgb_temp <-  get_metrics(factor(results_uu_xgb_temp$predicted,
                                          levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")),
                                   factor(results_uu_xgb_temp$label_track,
                                          levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")))
metrics_uu_xgb_all <- get_metrics(factor(results_uu_xgb_all$predicted,
                                        levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")),
                                 factor(results_uu_xgb_all$label_track,
                                        levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk")))
