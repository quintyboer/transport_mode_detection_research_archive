
### load libraries -----------------------------------------------------------------------------------------------------------

require(caret) # data splitting and RF training pipeline

  
### split data into train & test (75/25 ratio) -------------------------------------------------------------------------------

# seed for reproducibility
set.seed(4845242)

# block by user to assign them to train or test
train_index <- createDataPartition(unique(events_pred$user_id), p = 0.75, list = FALSE)
train_users <- unique(events_pred$user_id)[train_index]
test_users <- unique(events_pred$user_id)[-train_index]


### create train/test data for different feature sets ------------------------------------------------------------------------

# add gps features to prediction data (events_pred) and create train/test data
traindata_gps <- events_pred[events_gps, on = colnames(events_pred), nomatch = NULL][user_id %in% train_users]
testdata_gps <- events_pred[events_gps, on = colnames(events_pred), nomatch = NULL][user_id %in% test_users]

# add gps + osm features and create train/test data
traindata_osm <- events_pred[events_gps_osm, on = colnames(events_pred), nomatch = NULL][user_id %in% train_users]
testdata_osm <- events_pred[events_gps_osm, on = colnames(events_pred), nomatch = NULL][user_id %in% test_users]

# add gps + temporal features and create train_test data
traindata_temp <- events_pred[events_gps_temp, on = colnames(events_pred), nomatch = NULL][user_id %in% train_users]
testdata_temp <- events_pred[events_gps_temp, on = colnames(events_pred), nomatch = NULL][user_id %in% test_users]

# add all features and create train/test data
traindata_all <- events_pred[events_gps_osm_temp, on = colnames(events_pred), nomatch = NULL][user_id %in% train_users]
testdata_all <- events_pred[events_gps_osm_temp, on = colnames(events_pred), nomatch = NULL][user_id %in% test_users]


### cross-validation splits (for hyperparameter tuning) ----------------------------------------------------------------------

set.seed(4845242)

# 10 cross-validation folds with blocking/grouping by user id (training indices)
cv_folds <- groupKFold(events_pred[user_id %in% train_users]$user_id, k = 10)

# convert to test indices (for xgboost)
xgb_folds <- lapply(cv_folds, function(train_id) {
  setdiff(seq_len(nrow(events_pred[user_id %in% train_users])), train_id)
})


### remove columns not used in model input -----------------------------------------------------------------------------------

remove_columns <- c("user_id", "start_time", "end_time", "label", "label_stop",
                    "fase", "VariantCode", "invitedstudyduration", "n_geolocations",
                    "duration_hours", "duration_secs", "start_day", "end_day",
                    "n_labeled_tracks", "event_id")

dt_names <- c("traindata_gps", "testdata_gps", "traindata_osm", "testdata_osm",
             "traindata_temp", "testdata_temp", "traindata_all", "testdata_all")

# remove columns
for(dt_name in dt_names){
  get(dt_name)[, (remove_columns) := NULL]
  
  # change label_track to factor
  get(dt_name)[, label_track := factor(label_track)]
}


### compute inverse class weights --------------------------------------------------------------------------------------------

# n obs in traindata and n classes
n_obs_train <- nrow(events_pred[user_id %in% train_users])
n_obs_test <- nrow(events_pred[user_id %in% test_users])
n_classes <- length(unique(events_pred$label_track))

# compute inverse class weight
class_weights <- n_obs_train / (n_classes * table(events_pred[user_id %in% train_users]$label_track))
weights <- class_weights[events_pred[user_id %in% train_users]$label_track]


### number of features per dataset -------------------------------------------------------------------------------------------

# all columns minus the target label_track
n_feat_gps <- ncol(traindata_gps)-1
n_feat_osm <- ncol(traindata_osm)-1
n_feat_temp <- ncol(traindata_temp)-1
n_feat_all <- ncol(traindata_all)-1


### prepare external validation data -----------------------------------------------------------------------------------------

# create correct label_track factor levels (include car)
events_uu_pred[, label_track := factor(label_track, levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk"))]


# add gps features
testdata_uu_gps <- events_uu_pred[events_uu_gps, on = colnames(events_uu_pred), nomatch = NULL]

# add gps + osm features
testdata_uu_osm <- events_uu_pred[events_uu_gps_osm, on = colnames(events_uu_pred), nomatch = NULL]

# add gps + temporal features
testdata_uu_temp <- events_uu_pred[events_uu_gps_temp, on = colnames(events_uu_pred), nomatch = NULL]

# add all features
testdata_uu_all <- events_uu_pred[events_uu_gps_osm_temp, on = colnames(events_uu_pred), nomatch = NULL]


# select columns to remove
remove_columns_uu <- c("event_id", "user_id", "start_time_updated", "end_time_updated",
                       "start_time", "end_time", "duration_hours", "duration_secs", "start_day",
                       "end_day", "label", "n_labeled_tracks", "user_id_x")

dt_names_uu <- c("testdata_uu_gps", "testdata_uu_osm", "testdata_uu_temp", "testdata_uu_all")

# remove unnecessary columns
for(dt_name_uu in dt_names_uu){
  get(dt_name_uu)[, (remove_columns_uu) := NULL]
  
  # change label_track from char to factor with correct levels (explicitly include car)
  get(dt_name_uu)[, label_track := factor(label_track, levels = c("bike", "bus", "car", "ferry", "metro", "train", "tram", "walk"))]
}

# add missing (empty) columns (for UU data: previous stop duration and purpose, car lags and cummeans)
testdata_uu_temp[, setdiff(names(testdata_temp), names(testdata_uu_temp)) := as.numeric(NA)]
testdata_uu_all[, setdiff(names(testdata_all), names(testdata_uu_all)) := as.numeric(NA)]

# remove ferry lag columns
testdata_uu_temp[, c("track_lag1_ferry", "track_lag2_ferry", "track_lag3_ferry") := NULL]
testdata_uu_all[, c("track_lag1_ferry", "track_lag2_ferry", "track_lag3_ferry") := NULL]

# reorder columns
setcolorder(testdata_uu_temp, names(testdata_temp))
setcolorder(testdata_uu_all, names(testdata_all))

