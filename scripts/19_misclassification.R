
### look at some misclassifications in rf --------------------------------------

# rf confusion matrix (all features)
metrics_test_rf_all$confusion_matrix

check_columns_rf <- c("N", "med_accuracy", "avg_accuracy", "duration_mins", rf_varimp_20$Feature)

# walk, but predicted train
results_test_rf_all[label_track == "walk" & predicted == "train",
                     ..check_columns_rf]

# train but predicted walk
results_test_rf_all[label_track == "train" & predicted == "walk",
                     ..check_columns_rf]

# walk, but predicted car
results_test_rf_all[label_track == "walk" & predicted == "car",
                     ..check_columns_rf]

# car, but predicted walk
results_test_rf_all[label_track == "car" & predicted == "walk",
                     ..check_columns_rf]


### look at some misclassifications in xgb -------------------------------------

# select columns to look at (some general + 20 most important)
check_columns_xgb <- c("N", "med_accuracy", "avg_accuracy", "duration_mins", xgb_varimp[1:20, Feature])

# walk, but predicted train
results_test_xgb_all[label_track == "walk" & predicted == "train",
                     ..check_columns_xgb]
mean(results_test_xgb_all[label_track == "walk" & predicted == "train"]$avg_speed_kmh)
mean(results_test_xgb_all[label_track == "walk"]$avg_speed_kmh)

# train but predicted walk
results_test_xgb_all[label_track == "train" & predicted == "walk",
                     ..check_columns_xgb]
mean(results_test_xgb_all[label_track == "train" & predicted == "walk"]$avg_speed_kmh)
mean(results_test_xgb_all[label_track == "train"]$avg_speed_kmh)

# walk, but predicted car
results_test_xgb_all[label_track == "walk" & predicted == "car",
                     ..check_columns_xgb]
mean(results_test_xgb_all[label_track == "walk" & predicted == "car"]$avg_speed_kmh)
mean(results_test_xgb_all[label_track == "walk"]$avg_speed_kmh)

# car, but predicted walk
results_test_xgb_all[label_track == "car" & predicted == "walk",
                     ..check_columns_xgb]
mean(results_test_xgb_all[label_track == "car" & predicted == "walk"]$avg_speed_kmh)
mean(results_test_xgb_all[label_track == "car"]$avg_speed_kmh)


### combine rf and xgb results (all features) into a single dt -----------------

# select results columns (probs, pred, 2nd best, first/second prob, adjusted pred)
results_rf <- results_test_rf_all[,320:334]
results_xgb <- results_test_xgb_all[,320:334]

# add model to column name for distinction
setnames(results_rf, paste0("rf_", names(results_rf)))
setnames(results_xgb, paste0("xgb_", names(results_xgb)))

# combine results with testdata
results <- cbind(testdata_all, results_rf, results_xgb)


### compare misclassification between xgb and rf -------------------------------

results[, `:=`(
  correct_rf_not_xgb = rf_correct & !xgb_correct,
  correct_xgb_not_rf = xgb_correct & !rf_correct,
  correct_both = xgb_correct & rf_correct,
  correct_neither = !xgb_correct & !rf_correct
  )]

# misclassification in rf
nrow(results[rf_correct == FALSE]) # 133 misclassifications in rf
nrow(results[rf_correct == FALSE])/nrow(results) # 0.126 (inverse accuracy)
table(results[rf_correct == FALSE]$label_track) # by label
table(results[rf_correct == FALSE]$label_track)/table(results$label_track) # mostly similar proportions (between .1 and .16), except tram

# misclassification in xgb
nrow(results[xgb_correct == FALSE]) # 94 misclassifications in xgb
nrow(results[xgb_correct == FALSE])/nrow(results) # 0.089 (inverse accuracy)
table(results[xgb_correct == FALSE]$label_track) # by label
table(results[xgb_correct == FALSE]$label_track)/table(results$label_track) # much lower for bike & car (<0.07), higher for metro and bus (>0.2)

# predicted wrongly in both
nrow(results[correct_neither == TRUE]) # 76 misclassified in both
nrow(results[correct_neither == TRUE])/nrow(results) # 0.072 (inverse accuracy)
table(results[correct_neither == TRUE]$label_track) # by label
table(results[correct_neither == TRUE]$label_track)/table(results$label_track) # walk, metro and train > 0.1

# wrong in rf but correct in xgb
nrow(results[correct_xgb_not_rf == TRUE]) # 57 misclassifications in rf (but not in xgb)
nrow(results[correct_xgb_not_rf == TRUE])/nrow(results) # 0.054 of testdata
nrow(results[correct_xgb_not_rf == TRUE])/nrow(results[rf_correct == FALSE]) # 0.429 of rf misclassified
table(results[correct_xgb_not_rf == TRUE]$label_track) # by label
table(results[correct_xgb_not_rf == TRUE]$label_track)/table(results$label_track)
table(results[correct_xgb_not_rf == TRUE]$label_track)/table(results[rf_correct == FALSE]$label_track)

# wrong in xgb but correct in rf
nrow(results[xgb_correct == FALSE])
nrow(results[xgb_correct == FALSE]) / nrow(results)
nrow(results[correct_rf_not_xgb == TRUE]) # 18 misclassifications in xgb (but not in rf)
nrow(results[correct_rf_not_xgb == TRUE])/nrow(results) # 0.017 of testdata
nrow(results[correct_rf_not_xgb == TRUE])/nrow(results[xgb_correct == FALSE]) # 0.191 of xgb misclassified
table(results[correct_rf_not_xgb == TRUE]$label_track) # by label
table(results[correct_rf_not_xgb == TRUE]$label_track)/table(results$label_track)
table(results[correct_rf_not_xgb == TRUE]$label_track)/table(results[xgb_correct == FALSE]$label_track)

table(results[correct_rf_not_xgb == TRUE]$label_track)
results[correct_rf_not_xgb == TRUE & label_track == car]$

  
### misclassification by n geolocations and mean accuracy ----------------------
cut(results$N, breaks = quantile(results$N, probs = seq(0, 1, 0.1)))

misclassification_n <- results %>%
  mutate(binned_N = cut(results$N, breaks = quantile(results$N, probs = seq(0, 1, 0.1))),
         sum_incorrect_rf = nrow(results[rf_correct == FALSE]),
         sum_incorrect_xgb = nrow(results[xgb_correct == FALSE])) %>%
  group_by(binned_N) %>%
  summarise(n_incorrect_rf = sum(!rf_correct),
            prop_incorrect_rf = sum(!rf_correct) / mean(sum_incorrect_rf),
            n_incorrect_xgb = sum(!xgb_correct),
            prop_incorrect_xgb = sum(!xgb_correct) / mean(sum_incorrect_xgb))
misclassification_n

misclassification_acc <- results %>%
  mutate(binned_avg_accuracy = cut(results$avg_accuracy, breaks = quantile(results$avg_accuracy, probs = seq(0, 1, 0.1))),
         sum_incorrect_rf = nrow(results[rf_correct == FALSE]),
         sum_incorrect_xgb = nrow(results[xgb_correct == FALSE])) %>%
  group_by(binned_avg_accuracy) %>%
  summarise(n_incorrect_rf = sum(!rf_correct),
            prop_incorrect_rf = sum(!rf_correct) / mean(sum_incorrect_rf),
            n_incorrect_xgb = sum(!xgb_correct),
            prop_incorrect_xgb = sum(!xgb_correct) / mean(sum_incorrect_xgb))
misclassification_acc

