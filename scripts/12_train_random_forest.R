
### load libraries

require(ranger) # random forest implementation


### function for training RF model in caret ----------------------------------------------------------------------------------

# Function for training the ranger model in caret,
# using a pre-specified trControl object (with or without 10-fold cross-validation)
# and tuneGrid object (multiple parameter values for cv, or only one for final model).
# NA's are internally handled by ranger default (na.action = na.learn):
# missng values are ignored for initial split criterion value, then missings are tried in both child nodes,
# direction with best split criterion value is selected as default direction (see ranger documentation).
# Using inverse class weights such that underrepresented classes will be selected with higher probability
# in the bootstrap samples for the trees (case.weights)
# function input: training data, number of rf trees, trainControl and hyperparameter grid
# output: trained rf model
rf_train <- function(data, # train data
                     num.trees, # number of rf trees (here, 200 for cv or 2000 for final model)
                     trControl, # created with caret's trainControl() function
                     tuneGrid) { # created with expand.grid() function, includes model hyperparameters

  rf_model <- train(
    label_track ~.,
    data = data, # specified in function call
    method = "ranger", # random forest implementation
    num.trees = num.trees, # specificied in function call
    trControl = trControl, # specified in function call
    tuneGrid = tuneGrid, # specified in function call
    importance = "permutation", # permutation method for variable importance (instead of gini)
    metric = "Mean_Recall", # mean recall as evaluation/model selection metric
    weights = weights, # use inverse class weights (the same for each train dataset)
    na.action = na.pass # defaults to na.action = na.learn in ranger
  )
  
  return(rf_model)
}


### generate seeds -----------------------------------------------------------------------------------------------------------

# seeds for cross-validation with different hyperparameter combinations
set.seed(4845242)
k_folds <- 10 # 10-fold cross-validation 
n_param <- 3*3 # grid search with 3 mtry and 3 min node size values
seeds <- vector(mode = "list", length = k_folds + 1)
for(i in 1:k_folds) {
  seeds[[i]] <- sample.int(100000, n_param)
}
seeds[[k_folds + 1]] <- sample.int(100000, 1)


### specify trainControl and tuning grid for random forest cv-----------------------------------------------------------------

# specify trainControl for 10-fold cross-validation rf models
train_control <- trainControl(
  method = "cv",
  number = 10,
  index = cv_folds,
  seeds = seeds,
  verboseIter = TRUE,
  summaryFunction = multiClassSummary, # Mean recall will be evaluation metric
  classProbs = TRUE
)

# hyperparameter grid for rf cv models
tune_grid <- expand.grid(
  mtry = c(20, 30, 40), # n features to possibly split at in each node
  splitrule = c("gini"),
  min.node.size = c(200, 300, 400) # minimum n obs in terminal nodes
)


### train random forest models with cv grid search ---------------------------------------------------------------------------

# cv for random forest with gps traindata (200 trees)
rf_gps_cv <- rf_train(traindata_gps,
                      num.trees = 200,
                      trControl = train_control,
                      tuneGrid = tune_grid)

# cv for random forest with gps + osm traindata (200 trees)
rf_osm_cv <- rf_train(traindata_osm,
                      num.trees = 200,
                      trControl = train_control,
                      tuneGrid = tune_grid)

# cv for random forest with gps + temp traindata (200 trees)
rf_temp_cv <- rf_train(traindata_temp,
                       num.trees = 200,
                       trControl = train_control,
                       tuneGrid = tune_grid)

# cv for random forest with gps + temp traindata (200 trees)
rf_all_cv <- rf_train(traindata_all,
                      num.trees = 200,
                      trControl = train_control,
                      tuneGrid = tune_grid)

# see select hyperparameters for each model
rf_gps_cv # mtry = 20, min.node.size = 400
rf_osm_cv # mtry = 30, min.node.size = 300
rf_temp_cv # mtry = 30, min.node.size = 300
rf_all_cv # mtry = 30, min.node.size = 300


### respecify traincontrol and hyperparameter grids for final models ---------------------------------------------------------

# respecify trainControl without cv for final rf models
train_control_final <- trainControl(
  method = "none",
  verboseIter = TRUE,
  summaryFunction = multiClassSummary,
  classProbs = TRUE,
  seeds = seeds
)

# hyperparameter grid for rf with gps features (mtry = 20, min.node.size = 400)
tune_grid_gps <- expand.grid(
  mtry = c(20),
  splitrule = c("gini"),
  min.node.size = c(400)
)

# hyperparameter grid for other rf models (mtry = 30, min.node.size = 300)
tune_grid_all <- expand.grid(
  mtry = c(30),
  splitrule = c("gini"),
  min.node.size = c(300)
)


### rerun final rf models with fixed hyperparameters -------------------------------------------------------------------------

# final random forest with gps traindata (2000 trees)
rf_gps_final <- rf_train(traindata_gps,
                      num.trees = 2000,
                      trControl = train_control_final,
                      tuneGrid = tune_grid_gps)

# final random forest with gps + osm traindata (2000 trees)
rf_osm_final <- rf_train(traindata_osm,
                      num.trees = 2000,
                      trControl = train_control_final,
                      tuneGrid = tune_grid_all)

# final random forest with gps + temp traindata (2000 trees)
rf_temp_final <- rf_train(traindata_temp,
                       num.trees = 2000,
                       trControl = train_control_final,
                       tuneGrid = tune_grid_all)

# final random forest with gps + temp traindata (2000 trees)
rf_all_final <- rf_train(traindata_all,
                      num.trees = 2000,
                      trControl = train_control_final,
                      tuneGrid = tune_grid_all)

# final rf models
rf_gps_final
rf_osm_final
rf_temp_final
rf_all_final

