
### load libraries -----------------------------------------------------------------------------------------------------------

require(xgboost)


### create xgb train and test matrices ---------------------------------------------------------------------------------------

# xgb train/test matrix with gps features (with inverse class weights)
trainmatrix_gps <- xgb.DMatrix(data = as.matrix(traindata_gps[,!"label_track"]),
                                  label = as.matrix(as.numeric(traindata_gps$label_track) -1),
                                  weight = weights)
testmatrix_gps <- xgb.DMatrix(data = as.matrix(testdata_gps[, !"label_track"]),
                                 label = as.matrix(as.numeric(testdata_gps$label_track) -1))

# xgb train/test matrix with gps + osm features
trainmatrix_osm <- xgb.DMatrix(data = as.matrix(traindata_osm[,!"label_track"]),
                               label = as.matrix(as.numeric(traindata_osm$label_track) -1),
                               weight = weights)
testmatrix_osm <- xgb.DMatrix(data = as.matrix(testdata_osm[, !"label_track"]),
                              label = as.matrix(as.numeric(testdata_osm$label_track) -1))

# xgb train/test matrix with gps + temporal features
trainmatrix_temp <- xgb.DMatrix(data = as.matrix(traindata_temp[,!"label_track"]),
                               label = as.matrix(as.numeric(traindata_temp$label_track) -1),
                               weight = weights)
testmatrix_temp <- xgb.DMatrix(data = as.matrix(testdata_temp[, !"label_track"]),
                              label = as.matrix(as.numeric(testdata_temp$label_track) -1))

# xgb train/test matrix with all features
trainmatrix_all <- xgb.DMatrix(data = as.matrix(traindata_all[,!"label_track"]),
                                   label = as.matrix(as.numeric(traindata_all$label_track) -1),
                                   weight = weights)
testmatrix_all <- xgb.DMatrix(data = as.matrix(testdata_all[, !"label_track"]),
                                  label = as.matrix(as.numeric(testdata_all$label_track) -1))


### function for xgb parameter grid search -----------------------------------------------------------------------------------

# perform xgb hyperparameter grid search with 10-fold cv
# parameters not included in grid will be set to the xgb default (see documentation)
# early stopping rounds is set as a tenth of the total number of iterations
# output: matrix with results of all hyperparameter combinations, best model, and best parameter combination
xgb_param_search <- function(grid, # hyperparameter grid (created with expand.grix)
                             trainmatrix, # training data/matrix (xgb.DMatrix)
                             nrounds, # number of max. iterations
                             all = FALSE) { # whether to store full evaluation logs of all models (ex: for hyperparameter plots)
  
  # number of hyperparameter combinations
  n_param <- nrow(grid)
  
  # xgb hyperparameter grid search
  xgb_cv <- apply(grid, 1, function(params) {
    xgb.cv(
      params = as.list(params), # specified in function call (grid)
      data = trainmatrix, # specified in function call
      folds = xgb_folds, # previously set cv folds for 10-fold cv
      nrounds = nrounds, # specified in function call
      early_stopping_rounds = nrounds/10, # tenth of max iterations
      verbose = 0,
      nthread = 6,
      objective = "multi:softprob",
      eval_metric = "mlogloss",
      num_class = n_classes
    )
  })
  
  # create matrix for storing cv results
  xgb_cv_results <- matrix(c(seq(1, n_param), rep(0, n_param)), ncol = 2)
  
  # add best iteration (lowest mlogloss) of each param combination to results matrix
  for(i in 1:n_param){
    xgb_cv_results[i, 2] <- min(xgb_cv[[i]]$evaluation_log$test_mlogloss_mean)
  }
  
  # select model with best cv results
  selected <- xgb_cv_results[which.min(xgb_cv_results[, 2]), ][1]
  
  # return results matrix, best model, best parameters (and full evaluation logs if all is true)
  if(all) {
    return(list(
      results_matrix = xgb_cv_results,
      best_model = xgb_cv[[selected]],
      best_param = xgb_cv[[selected]]$params,
      full_logs = xgb_cv
    ))
  } else {
  return(list(
    results_matrix = xgb_cv_results,
    best_model = xgb_cv[[selected]],
    best_param = xgb_cv[[selected]]$params
  ))
  }
}


### first parameter search: gamma, max tree depth & min node weight ----------------------------------------------------------

# define hyperparameter grid
xgb_grid <- expand.grid(
  gamma = c(1, 2, 5),
  max_depth = c(3, 4, 5),
  min_child_weight = c(10, 20, 30)
)

# seed for reproducibility
set.seed(4845242)

# xgb cv with gps features (100 iterations)
xgb_gps_cv_1 <- xgb_param_search(xgb_grid, trainmatrix_gps, nrounds = 100)

# xgb cv with gps + osm features (100 iterations)
xgb_osm_cv_1 <- xgb_param_search(xgb_grid, trainmatrix_osm, nrounds = 100)

# xgb cv with gps + temporal features (100 iterations)
xgb_temp_cv_1 <- xgb_param_search(xgb_grid, trainmatrix_temp, nrounds = 100)

# xgb cv with all features (100 iterations)
xgb_all_cv_1 <- xgb_param_search(xgb_grid, trainmatrix_all, nrounds = 100)


### second hyperparameter search: (column) subsample -------------------------------------------------------------------------

# update hyperparameter grid for xgb with gps features
xgb_gps_cv_1[3] # gamma = 2, max_depth 3, min_child_weight 20

xgb_grid_2_gps <- expand.grid(
  gamma = 2,
  max_depth = 3,
  min_child_weight = 20,
  subsample = c(0.5, 0.7, 0.9),
  colsample_bytree = c(0.5, 0.7, 0.9))

# update hyperparameter grid for xgb with gps + osm features
xgb_osm_cv_1[3] # gamma = 1, max_depth 3, min_child_weight 20

xgb_grid_2_osm <- expand.grid(
  gamma = 1,
  max_depth = 3,
  min_child_weight = 20,
  subsample = c(0.5, 0.7, 0.9),
  colsample_bytree = c(0.5, 0.7, 0.9))

# update hyperparameter grid for xgb with gps + temporal features
xgb_temp_cv_1[3] # gamma = 5, max_depth 3, min_child_weight 30

xgb_grid_2_temp <- expand.grid(
  gamma = 5,
  max_depth = 3,
  min_child_weight = 30,
  subsample = c(0.5, 0.7, 0.9),
  colsample_bytree = c(0.5, 0.7, 0.9))

# update hyperparameter grid for xgb with gps + temporal features
xgb_all_cv_1[3] # gamma = 1, max_depth 3, min_child_weight 10

xgb_grid_2_all <- expand.grid(
  gamma = 1,
  max_depth = 3,
  min_child_weight = 10,
  subsample = c(0.5, 0.7, 0.9),
  colsample_bytree = c(0.5, 0.7, 0.9))

# seed for reproducibility
set.seed(4845242)

# xgb cv with gps features (100 iterations)
xgb_gps_cv_2 <- xgb_param_search(xgb_grid_2_gps, trainmatrix_gps, nrounds = 100)

# xgb cv with gps + osm features (100 iterations)
xgb_osm_cv_2 <- xgb_param_search(xgb_grid_2_osm, trainmatrix_osm, nrounds = 100)

# xgb cv with gps + temporal features (100 iterations)
xgb_temp_cv_2 <- xgb_param_search(xgb_grid_2_temp, trainmatrix_temp, nrounds = 100)

# xgb cv with all features (100 iterations)
xgb_all_cv_2 <- xgb_param_search(xgb_grid_2_all, trainmatrix_all, nrounds = 100)


### third parameter search: eta ----------------------------------------------------------------------------------------------

# update hyperparameter grid for xgb with gps features
xgb_gps_cv_2[3] # subsample = 0.9, colsample_bytree = 0.7

xgb_grid_3_gps <- expand.grid(
  gamma = 2,
  max_depth = 3,
  min_child_weight = 20,
  subsample = 0.9,
  colsample_bytree = 0.7,
  eta = c(0.05, 0.1, 0.2, 0.3))

# update hyperparameter grid for xgb with gps + osm features
xgb_osm_cv_2[3] # subsample = 0.9, colsample_btree = 0.7

xgb_grid_3_osm <- expand.grid(
  gamma = 1,
  max_depth = 3,
  min_child_weight = 20,
  subsample = 0.9,
  colsample_bytree = 0.7,
  eta = c(0.05, 0.1, 0.2, 0.3))

# update hyperparameter grid for xgb with gps + temporal features
xgb_temp_cv_2[3] # subsample = 0.9, colsample_bytree = 0.7

xgb_grid_3_temp <- expand.grid(
  gamma = 5,
  max_depth = 3,
  min_child_weight = 30,
  subsample = 0.9,
  colsample_bytree = 0.7,
  eta = c(0.05, 0.1, 0.2, 0.3))

# update hyperparameter grid for xgb with gps + temporal features
xgb_all_cv_2[3] # subsample = 0.9, colsample_bytree = 0.9

xgb_grid_3_all <- expand.grid(
  gamma = 1,
  max_depth = 3,
  min_child_weight = 10,
  subsample = 0.9,
  colsample_bytree = 0.9,
  eta = c(0.05, 0.1, 0.2, 0.3))

# seed for reproducibility
set.seed(4845242)

# xgb cv with gps features (200 iterations)
xgb_gps_cv_3 <- xgb_param_search(xgb_grid_3_gps, trainmatrix_gps, nrounds = 200, all = TRUE)

# xgb cv with gps + osm features (200 iterations)
xgb_osm_cv_3 <- xgb_param_search(xgb_grid_3_osm, trainmatrix_osm, nrounds = 200, all = TRUE)

# xgb cv with gps + temporal features (200 iterations)
xgb_temp_cv_3 <- xgb_param_search(xgb_grid_3_temp, trainmatrix_temp, nrounds = 200, all = TRUE)

# xgb cv with all features (200 iterations)
xgb_all_cv_3 <- xgb_param_search(xgb_grid_3_all, trainmatrix_all, nrounds = 200, all = TRUE)

# check number of iterations for each model
xgb_gps_cv_3[2] # nrounds = 42
xgb_osm_cv_3[2] # nrounds = 54
xgb_temp_cv_3[2] # nrounds = 14
xgb_all_cv_3[2] # nrounds = 23

# check eta value for each model
xgb_gps_cv_3[3] # eta = 0.1, 
xgb_osm_cv_3[3] # eta = 0.1
xgb_temp_cv_3[3] # eta = 0.3
xgb_all_cv_3[3] # eta = 0.2


### train final xgb models ---------------------------------------------------------------------------------------------------

# seed for reproducibility
set.seed(4845242)

# xgb with gps features
xgb_gps_final <- xgb.train(
  gamma = 2,
  max_depth = 3,
  min_child_weight = 20,
  subsample = 0.9,
  colsample_bytree = 0.7,
  eta = 0.1,
  nrounds = 42,
  data = trainmatrix_gps,
  verbose = 0,
  nthread = 6,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = n_classes
)

# xgb with gps + osm features
xgb_osm_final <- xgb.train(
  gamma = 1,
  max_depth = 3,
  min_child_weight = 20,
  subsample = 0.9,
  colsample_bytree = 0.7,
  eta = 0.1,
  nrounds = 54,
  data = trainmatrix_osm,
  verbose = 0,
  nthread = 6,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = n_classes
)

# xgb with gps + temp features
xgb_temp_final <- xgb.train(
  gamma = 5,
  max_depth = 3,
  min_child_weight = 30,
  subsample = 0.9,
  colsample_bytree = 0.7,
  eta = 0.3,
  nrounds = 14,
  data = trainmatrix_temp,
  verbose = 0,
  nthread = 6,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = n_classes
)

# xgb with all features
xgb_all_final <- xgb.train(
  gamma = 1,
  max_depth = 3,
  min_child_weight = 10,
  subsample = 0.9,
  colsample_bytree = 0.9,
  eta = 0.2,
  nrounds = 23,
  data = trainmatrix_all,
  verbose = 0,
  nthread = 6,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = n_classes
)

