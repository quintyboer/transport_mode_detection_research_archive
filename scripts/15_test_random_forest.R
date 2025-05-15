
### function to compute confusion matrix based metrics -----------------------------------------------------------------------

# input: 2 vectors with the predicted and true values (as factors)
# output: list with macro-averaged f1, (balanced) accuracy, recall, specificity, precision,
# per class f1, per class balanced accuracy, and confusion matrix

get_metrics <- function(pred, true) {
  conf_matrix <- confusionMatrix(pred, true)
  precision <- conf_matrix$byClass[, "Precision"]
  recall <- conf_matrix$byClass[, "Recall"] # sensitivity / TPR
  specificity <- conf_matrix$byClass[, "Specificity"] # TNR
  f1_scores <- 2 * (precision * recall) / (precision + recall)
  f1_scores[is.nan(f1_scores)] <- 0
  macro_f1 <- mean(f1_scores)
  mean_recall <- mean(recall)
  balanced_accuracy_classes <- (recall + specificity) / 2
  mean_balanced_accuracy <- mean(balanced_accuracy_classes)
  accuracy <- conf_matrix$overall["Accuracy"]
  conf_matrix_table <- table(pred, true)
  
  return(list(
    accuracy = accuracy,
    balanced_accuracy = mean_balanced_accuracy,
    mean_recall = mean_recall,
    macro_f1 = macro_f1,
    f1_scores = f1_scores,
    balanced_accuracy_classes = balanced_accuracy_classes,
    recall = recall,
    specificity = specificity,
    precision = precision,
    confusion_matrix = conf_matrix_table
  ))
}


### function to store rf results in dt ---------------------------------------------------------------------------------------

# get predictions and pseudo-probabilties from an rf model and dataset
# input: data in dt format and trained rf model (from ranger/caret train function)
# output: dt with the original model features, label, predicted probabilities & predicted class
get_rf_results <- function(testdata, # format: data.table
                           rf_model) {
  
  # store class labels for column names
  class_labels <- names(class_weights)
  
  # get predicted class
  preds <- predict(rf_model, testdata, na.action = na.pass)
  
  # get predicted probabilities and change column names
  probs <- predict(rf_model, testdata, type = "prob", na.action = na.pass) %>%
    setNames(paste0(class_labels, "_prob"))
  
  # create new columns with first pred + prob and second pred + prob
  probs <- probs %>%
    rowwise() %>%
    mutate(class_probs = list(c_across(everything())),
           sorted_indices = list(order(class_probs, decreasing = TRUE)),
           predicted = 0,
           second_choice = factor(class_labels[sorted_indices[2]], levels = class_labels),
           first_prob = class_probs[sorted_indices[1]],
           second_prob = class_probs[sorted_indices[2]]) %>%
    select(-class_probs, -sorted_indices) %>%
    data.table()
  probs$predicted <- preds
  
  # combine prediction results with original testdata
  results <- cbind(testdata, probs)
  
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


### training data results ----------------------------------------------------------------------------------------------------

results_train_rf_gps <- get_rf_results(traindata_gps, rf_gps_final)
results_train_rf_osm <- get_rf_results(traindata_osm, rf_osm_final)
results_train_rf_temp <- get_rf_results(traindata_temp, rf_temp_final)
results_train_rf_all <- get_rf_results(traindata_all, rf_all_final)


# store evaluation metrics
metrics_train_rf_gps <- get_metrics(results_train_rf_gps$predicted, results_train_rf_gps$label_track)
metrics_train_rf_osm <- get_metrics(results_train_rf_osm$predicted, results_train_rf_osm$label_track)
metrics_train_rf_temp <- get_metrics(results_train_rf_temp$predicted, results_train_rf_temp$label_track)
metrics_train_rf_all <- get_metrics(results_train_rf_all$predicted, results_train_rf_all$label_track)


### test data results --------------------------------------------------------------------------------------------------------

results_test_rf_gps <- get_rf_results(testdata_gps, rf_gps_final)
results_test_rf_osm <- get_rf_results(testdata_osm, rf_osm_final)
results_test_rf_temp <- get_rf_results(testdata_temp, rf_temp_final)
results_test_rf_all <- get_rf_results(testdata_all, rf_all_final)


# store evaluation metrics
metrics_test_rf_gps <- get_metrics(results_test_rf_gps$predicted, results_test_rf_gps$label_track)
metrics_test_rf_osm <- get_metrics(results_test_rf_osm$predicted, results_test_rf_osm$label_track)
metrics_test_rf_temp <- get_metrics(results_test_rf_temp$predicted, results_test_rf_temp$label_track)
metrics_test_rf_all <- get_metrics(results_test_rf_all$predicted, results_test_rf_all$label_track)
