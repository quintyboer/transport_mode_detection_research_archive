
### function to reformat and store xgb results in dt ---------------------------

# get predictions and pseudo-probabilties from an xgb model and dataset
# input: data in xgb.DMatrix format, data in original dt format, and trained xgboost model (from xgb.train)
# output: dt with the original model features, label, predicted probabilities & predicted class
get_xgb_results <- function(testdata_matrix, # format: xgb.DMatrix
                            testdata_original, # format: data.table
                            xgb_model) {
  
  # store class labels for column names
  class_labels <- names(class_weights)
  
  # get xgb predictions (probabilities)
  probs <- predict(xgb_model, testdata_matrix)
  
  # reformat xgb output into dt
  preds <- matrix(probs, nrow = n_classes, ncol = length(probs)/n_classes) %>%
    t() %>%
    data.table() %>%
    setNames(paste0(class_labels, "_prob"))
  
  # create new columns with first pred + prob and second pred + prob
  preds <- preds %>%
    rowwise() %>%
    mutate(class_probs = list(c_across(everything())),
           sorted_indices = list(order(class_probs, decreasing = TRUE)),
           predicted = factor(class_labels[sorted_indices[1]], levels = class_labels),
           second_choice = factor(class_labels[sorted_indices[2]], levels = class_labels),
           first_prob = class_probs[sorted_indices[1]],
           second_prob = class_probs[sorted_indices[2]]) %>%
    select(-class_probs, -sorted_indices) %>%
    data.table()
  
  # combine prediction results with original testdata
  results <- cbind(testdata_original, preds)
  
  # add new columns with correct prediction indicator (1st and 2nd choice)
  results[, `:=`(
    correct = as.character(label_track) == as.character(predicted),
    second_correct = as.character(label_track) == as.character(second_choice)
  )]
  
  # add columns based on choosing the 2nd best pred when first prob is lower than .5
  results[, predict_adjusted_.5 := fifelse(first_prob < 0.5, second_choice, predicted)]
  
  # add columns based on choosing the 2nd best pred when first prob is lower than .5 and first pred is incorrect
  results[, predict_adjusted_.5_incorrect := fifelse(first_prob < 0.5 & !correct, second_choice, predicted)]

  return(results)
}


### training data results ------------------------------------------------------

results_train_xgb_gps <- get_xgb_results(trainmatrix_gps, traindata_gps, xgb_gps_final)
results_train_xgb_osm <- get_xgb_results(trainmatrix_osm, traindata_osm, xgb_osm_final)
results_train_xgb_temp <- get_xgb_results(trainmatrix_temp, traindata_temp, xgb_temp_final)
results_train_xgb_all <- get_xgb_results(trainmatrix_all, traindata_all, xgb_all_final)

# save train data results
fwrite(results_train_xgb_gps, "/results_train_xgb_gps.csv")
fwrite(results_train_xgb_osm, "/results_train_xgb_osm.cv")
fwrite(results_train_xgb_temp, "/results_train_xgb_temp.cv")
fwrite(results_train_xgb_all, "/results_train_xgb_all.cv")

# store evaluation metrics
metrics_train_xgb_gps <- get_metrics(results_train_xgb_gps$predicted, results_train_xgb_gps$label_track)
metrics_train_xgb_osm <- get_metrics(results_train_xgb_osm$predicted, results_train_xgb_osm$label_track)
metrics_train_xgb_temp <- get_metrics(results_train_xgb_temp$predicted, results_train_xgb_temp$label_track)
metrics_train_xgb_all <- get_metrics(results_train_xgb_all$predicted, results_train_xgb_all$label_track)


### test data results ----------------------------------------------------------

results_test_xgb_gps <- get_xgb_results(testmatrix_gps, testdata_gps, xgb_gps_final)
results_test_xgb_osm <- get_xgb_results(testmatrix_osm, testdata_osm, xgb_osm_final)
results_test_xgb_temp <- get_xgb_results(testmatrix_temp, testdata_temp, xgb_temp_final)
results_test_xgb_all <- get_xgb_results(testmatrix_all, testdata_all, xgb_all_final)

# save test results
fwrite(results_test_xgb_gps, "/results_test_xgb_gps.csv")
fwrite(results_test_xgb_osm, "/results_test_xgb_osm.cv")
fwrite(results_test_xgb_temp, "/results_test_xgb_temp.cv")
fwrite(results_test_xgb_all, "/results_test_xgb_all.cv")

# store evaluation metrics
metrics_test_xgb_gps <- get_metrics(results_test_xgb_gps$predicted, results_test_xgb_gps$label_track)
metrics_test_xgb_osm <- get_metrics(results_test_xgb_osm$predicted, results_test_xgb_osm$label_track)
metrics_test_xgb_temp <- get_metrics(results_test_xgb_temp$predicted, results_test_xgb_temp$label_track)
metrics_test_xgb_all <- get_metrics(results_test_xgb_all$predicted, results_test_xgb_all$label_track)

