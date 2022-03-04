## Compare Outputs
library(glmnet)
library(caret)
library(here)
library(dplyr)
library(keras)
library(gbm)
library(purrr)
library(gridExtra)
library(ggplot2)
library(cowplot)

#library(ModelMetrics)
options(scipen=0)

# Load full data
load(here("data", "berlin", "b_metric_pic_abs_temp.Rda"))
data <- metric_pic_abs_temp
rm(metric_pic_abs_temp)
# Train data
x_tr <- model.matrix(price ~ ., train)[,-1] # x_tr stands for training
y_tr <- train$price
# Validation data
x_tst<-  model.matrix(price ~ ., test)[,-1] # x_tst stands for validation
y_tst<- test$price
# Full data
x <-  model.matrix(price ~ ., data)[,-1] # combined data set of train and valid.
y <- data$price


# Load models 
load(here("model", "validation performance", "ols_lasso.Rda"))
load(here("model", "validation performance", "boost_val.Rda"))
load(here("model", "validation performance", "rf_val.Rda"))


# Merge Lists
models <- c(ols_lasso_train[1:3], boost_models_val[1:2], rf_models_val[1:2], ols_lasso_train[4], boost_models_val[3], rf_models_val[3])




#--------------- Model Predictions
# Run the model to predict coefficients on the validation set. 
predictions <- list()
for (i in 1:length(models)){
  p <- models[[i]] %>% predict(x_tst)
  predictions[[length(predictions) + 1]] <- p
}
names(predictions) <- names(models)

predictions_log <- lapply(predictions[1:7], log)
predictions_log <- c(predictions_log, predictions[8:10])



#------------------- Performance
# Fit analysis in terms of RMSE, Rsquared and MAE
performance <- data.frame(matrix(1,3))
for (i in 1:length(predictions_log)){
  p <- predictions_log[[i]] %>% postResample(log(y_tst))
  performance[,i] <-  p
  rownames(performance) <- names(p)
  #performance[[length(performance) + 1]] <- p
}
names(performance) <- names(models)

#-------------------------- Variable importance
models_vip <- c(ols_lasso_train, boost_models_val)
names(models_vip) <-  c("OLS",
                        "Lasso",
                        "Lasso N",
                        "Lasso N + log(price)",
                        "GBM",
                        "GBM N",
                        "GBM N + log (price)")

var_importance <- list()
for (i in 1:length(models_vip)){     # Random Forest do not have variable importance with Caret
  vi <- models_vip[[i]] %>% varImp()
  var_importance[[length(var_importance) + 1]] <- vi
}
names(var_importance) <- names(models_vip)
var_importance

# Plot Variable Importance
vip <- lapply(var_importance, plot, top = 20)
names(vip) <- names(models_vip)

vip_lasso <- plot_grid(vip[[2]], vip[[3]], vip[[4]], ncol=1, nrow=3, labels = names(models_vip)[2:4])
vip_lasso

vip_gbm <- plot_grid(vip[[5]], vip[[6]], vip[[7]], ncol=1, nrow=3, labels = names(models_vip)[5:7])
vip_gbm






#----------------- Summaries of Outputs. 
summaries <- lapply(models, summary)

# Best Hyperparameters
best_hp <- list()
for (i in 1:length(models)) {
  bhp <- models[[i]]$bestTune
  best_hp[[length(best_hp) + 1]] <- bhp
}
best_hp


save(predictions,
     predictions_log,
     performance,
     best_hp,
     summaries,
     vip,
     vip_lasso,
     vip_gbm,
     var_importance,
     file = here("model", "validation performance", "val_results.Rda"))


# #---------- Load Results
# load(here("model", "hyperparameter tuning performance", "hp_results.Rda"))


# # DNN
# 
# 
# # Load dnn
# dnn <- load_model_tf(here("model", "dnn", "nn_abs_new"))
# 
# 
# # Predict dnn
# predictions_test <- dnn %>% predict(x = test_data)
# predictions_train <- dnn %>% predict(x = train_data)
# 
# # Calculate RMSE
# rmse(test_targets, predictions_test)
# rmse(train_targets, predictions_train)


