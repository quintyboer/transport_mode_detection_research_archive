
### load libraries -------------------------------------------------------------

require(viridis)


### random forest cv for more hyperparameters ----------------------------------
# to visualise more hyperparameter values for the rf model with all features

# specify trainControl for 10-fold cross-validation rf models
train_control <- trainControl(
  method = "cv",
  number = 10,
  index = cv_folds,
  verboseIter = TRUE,
  summaryFunction = multiClassSummary, # Mean recall will be evaluation metric
  classProbs = TRUE
)

# hyperparameter grid for rf cv models
tune_grid <- expand.grid(
  mtry = c(10, 20, 30, 40, 50), # n features to possibly split at in each node
  splitrule = c("gini"),
  min.node.size = c(0, 100, 200, 300, 400) # minimum n obs in terminal nodes
)

# cv for random forest with traindata and all features (200 trees)
set.seed(4845242)
rf_all_param <- rf_train(traindata_all,
                         num.trees = 200,
                         trControl = train_control,
                         tuneGrid = tune_grid)


### rf hyperparameter plot extended --------------------------------------------

# mean recall as evaluation metric
plot_rf_param_extended_recall <- ggplot(rf_all_param$results, aes(x = mtry, y = Mean_Recall, color = factor(min.node.size), group = factor(min.node.size))) +
  geom_point() +
  geom_line()+
  ylim(0.65, 0.85) +
  scale_color_manual(values = color_scheme_bright) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(x = "Randomly selected features",
       y = "Mean recall (cross-validation)",
       color = "Minimum node size")

# save plot
save_plot(plot_rf_param_extended_recall, "output/fig4_rf_hyperparameters_extended.png")


### XGB hyperparameter plot (eta + nround) -------------------------------------

# retrieve full evaluation log for the xgb cv model trained with eta = 0.05
# store mean mlogloss at nth iteration for train and validation, and best iteration 
eta05 <- data.table(
  nround = seq_along(xgb_all_cv_3[[4]][[1]]$evaluation_log$train_mlogloss_mean),
  Train = xgb_all_cv_3[[4]][[1]]$evaluation_log$train_mlogloss_mean,
  Validation = xgb_all_cv_3[[4]][[1]]$evaluation_log$test_mlogloss_mean,
  eta = 0.05,
  best_iter = xgb_all_cv_3[[4]][[1]]$best_iteration
)

# retrieve full evaluation log for the xgb cv model trained with eta = 0.1
eta1 <- data.table(
  nround = seq_along(xgb_all_cv_3[[4]][[2]]$evaluation_log$train_mlogloss_mean),
  Train = xgb_all_cv_3[[4]][[2]]$evaluation_log$train_mlogloss_mean,
  Validation = xgb_all_cv_3[[4]][[2]]$evaluation_log$test_mlogloss_mean,
  eta = 0.1,
  best_iter = xgb_all_cv_3[[4]][[2]]$best_iteration
)

# retrieve full evaluation log for the xgb cv model trained with eta = 0.2
eta2 <- data.table(
  nround = seq_along(xgb_all_cv_3[[4]][[3]]$evaluation_log$train_mlogloss_mean),
  Train = xgb_all_cv_3[[4]][[3]]$evaluation_log$train_mlogloss_mean,
  Validation = xgb_all_cv_3[[4]][[3]]$evaluation_log$test_mlogloss_mean,
  eta = 0.2,
  best_iter = xgb_all_cv_3[[4]][[3]]$best_iteration
)

# retrieve full evaluation log for the xgb cv model trained with eta = 0.3
eta3 <- data.table(
  nround = seq_along(xgb_all_cv_3[[4]][[4]]$evaluation_log$train_mlogloss_mean),
  Train = xgb_all_cv_3[[4]][[4]]$evaluation_log$train_mlogloss_mean,
  Validation = xgb_all_cv_3[[4]][[4]]$evaluation_log$test_mlogloss_mean,
  eta = 0.3,
  best_iter = xgb_all_cv_3[[4]][[4]]$best_iteration
)

# combine dt's with results for different eta and convert to long format 
eta <- rbindlist(list(eta05, eta1, eta2, eta3))
eta_long <- melt(
  eta, 
  id.vars = c("nround", "eta", "best_iter"),
  variable.name = "dataset",
  value.name = "mlogloss"
)

# plot mean mlogloss at each iteration for validation data (for each eta)
xgb_param_validation <- ggplot(eta_long[dataset == "Validation"], aes(x = nround, y = mlogloss, color = factor(eta), group = factor(eta))) +
  geom_line() +
  ylim(0.5, 2) +
  scale_color_manual(values = color_scheme_bright) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(x = "Boosting iteration",
       y = "Mean multiple-class log loss (cross-validation)",
       color = "Learning rate (eta)")

# save plot
save_plot(xgb_param_validation, "output/fig5_xgb_eta.png")

