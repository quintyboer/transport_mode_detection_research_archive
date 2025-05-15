
require(stringr)

### scatterplot of 1st vs second highest predicted probability (per label) ---------------------------------------------------

# random forest: first vs second probability (by class)
plot_rf_1v2 <- ggplot(results, aes(x = rf_first_prob, y = rf_second_prob,
                      color = factor(rf_correct, levels = c(TRUE, FALSE)),
                      alpha = factor(rf_correct, levels = c(TRUE, FALSE)))) +
  geom_point(size = 1) +
  facet_wrap(facets = ~factor(results$label_track, labels = str_to_title(levels(results$label_track)))) +
  xlim(0, 1) +
  ylim(0, 0.5) +
  scale_color_manual(labels = c("Correct", "Incorrect"), values = c("#4477AA", "#AA3377")) +
  scale_alpha_manual(values = c(0.4, 1), guide = "none") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  labs(x = "Pseudo-probability of the predicted class",
       y = "Pseudo-probability of the second class",
       color = "Random forest prediction")

# save plot
save_plot(plot_rf_1v2, "output/fig8_rf_probabilities.png")

# xgboost: first vs second probability (by class)
plot_xgb_1v2 <- ggplot(results, aes(x = xgb_first_prob, y = xgb_second_prob,
                    color = factor(xgb_correct, levels = c(TRUE, FALSE)),
                    alpha = factor(xgb_correct, levels = c(TRUE, FALSE)))) +
  geom_point(size = 1) +
  facet_wrap(facets = ~factor(results$label_track, labels = str_to_title(levels(results$label_track)))) +
  xlim(0, 1) +
  ylim(0, 0.5) +
  scale_color_manual(labels = c("Correct", "Incorrect"), values = c("#4477AA", "#AA3377")) +
  scale_alpha_manual(values = c(0.4, 1), guide = "none") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        strip.text.x = element_text(size = 10)) +
  labs(x = "Pseudo-probability of the predicted class",
       y = "Pseudo-probability of the second class",
       color = "XGBoost prediction")

save_plot(plot_xgb_1v2, "output/fig9_xgb_probabilities.png")


### Check misclassifications with correct 2nd best ---------------------------------------------------------------------------

nrow(results[rf_correct == FALSE]) # 133 incorrect
nrow(results[rf_correct == FALSE & rf_second_correct == TRUE]) # 75 correct with second
nrow(results[rf_correct == FALSE & rf_second_correct == TRUE])/nrow(results[rf_correct == FALSE]) # 0.564
table(results[rf_correct == FALSE & rf_second_correct == TRUE]$label_track)
table(results[rf_correct == FALSE & rf_second_correct == TRUE]$label_track) / table(results[rf_correct == FALSE]$label_track) 

# misclassification in xgb
nrow(results[xgb_correct == FALSE]) # 94 incorrect
nrow(results[xgb_correct == FALSE & xgb_second_correct == TRUE]) # 56 correct with second
nrow(results[xgb_correct == FALSE & xgb_second_correct == TRUE])/nrow(results[xgb_correct == FALSE]) # 0.596
table(results[xgb_correct == FALSE & xgb_second_correct == TRUE]$label_track)
table(results[xgb_correct == FALSE & xgb_second_correct == TRUE]$label_track) / table(results[xgb_correct == FALSE]$label_track) 


### metrics when 'switching' to 2nd best prediction --------------------------------------------------------------------------

# based on percentages of trips (quantiles) with lowest first prediction probability
rf_prob_breaks <- quantile(results$rf_first_prob, seq(0, 1, 0.1))[2:10] # get predicted probabilties for 10% up until 90%
xgb_prob_breaks <- quantile(results$xgb_first_prob, seq(0, 1, 0.1))[2:10]

# create 'adjusted' prediction based on switching to second best if first is incorrect below predicted probability quantile
for(percentage in c(seq(1, 9))) {
  results[, paste0("rf_predict_adjust_", percentage, 0) := fifelse(rf_correct == FALSE & rf_first_prob < rf_prob_breaks[[percentage]],
                                                                   rf_second_choice, rf_predicted)]
  results[, paste0("xgb_predict_adjust_", percentage, 0) := fifelse(xgb_correct == FALSE & xgb_first_prob < xgb_prob_breaks[[percentage]],
                                                                    xgb_second_choice, xgb_predicted)]
}

# switch all misclassified to second best prediction (100%)
results[, `:=`(
  rf_predict_adjust_100 = fifelse(rf_correct == FALSE, rf_second_choice, rf_predicted),
  xgb_predict_adjust_100 = fifelse(xgb_correct == FALSE, xgb_second_choice, xgb_predicted)
)]

# store evaluation metrics (accuracy & macro f1) for original prediction
metrics_adjusted <- data.table(cbind(metrics_test_rf_all$accuracy, metrics_test_rf_all$macro_f1, metrics_test_xgb_all$accuracy, metrics_test_xgb_all$macro_f1))
colnames(metrics_adjusted) <- c("rf_accuracy", "rf_f1", "xgb_accuracy", "xgb_f1")

# add metrics for adjusted predictions
for(percentage in c(seq(1,10))){
  metrics_perc <- cbind(get_metrics(results[, get(paste0("rf_predict_adjust_", percentage, 0))], results$label_track)$accuracy,
                        get_metrics(results[, get(paste0("rf_predict_adjust_", percentage, 0))], results$label_track)$macro_f1,
                        get_metrics(results[, get(paste0("xgb_predict_adjust_", percentage, 0))], results$label_track)$accuracy,
                        get_metrics(results[, get(paste0("xgb_predict_adjust_", percentage, 0))], results$label_track)$macro_f1)
  
  metrics_adjusted <- rbind(metrics_adjusted, metrics_perc, use.names = FALSE)
}

# add % adjusted as column
metrics_adjusted$perc_adjusted <- seq(0, 100, 10)
metrics_adjusted


### plot 'adjusted' metrics --------------------------------------------------------------------------------------------------

# change to long format
metrics_adjusted_long <- melt(metrics_adjusted,
                              id.vars = "perc_adjusted",
                              variable.name = "model_metric",
                              value.name = "value")
metrics_adjusted_long[, c("model", "metric") := tstrsplit(model_metric, "_", fixed = TRUE)]

# create plot
plot_adjusted_metrics <- ggplot(metrics_adjusted_long, aes(x = perc_adjusted, y = value, color = model, linetype = metric)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  theme_bw() +
  scale_color_manual(name = "Model", labels = c("Random Forest", "XGBoost"), values = color_scheme_bright) +
  scale_linetype_manual(name = "Evaluation metric", labels = c("Accuracy", "Macro-averaged F1-score"), values = c("accuracy"= "solid", "f1" = "dashed")) +
  labs(x = "% Adjusted",
       y = "Metric value (accuracy and macro-averaged F1-score)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

# save plot
save_plot(plot_adjusted_metrics, "output/fig10_adjusted_metrics.png")

