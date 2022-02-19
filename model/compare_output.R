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
load(here("model", "ols_lasso.Rda"))
load(here("model", "boost.Rda"))
load(here("model", "rf_mtry21.Rda"))

# Merge Lists
models <- c(ols_lasso, boost_models, rf_models_mtry21)


#--------------- Model Predictions
# Run the model to predict coefficients on the validation set. 
predictions <- list()
for (i in 1:length(models)){
  p <- models[[i]] %>% predict(x_tst)
  predictions[[length(predictions) + 1]] <- p
}
names(predictions) <- names(models)


#------------------- Performance
# Fit analysis in terms of RMSE, Rsquared and MAE
performance <- data.frame(matrix(1,3))
for (i in 1:length(predictions)){
  p <- predictions[[i]] %>% postResample(y_tst)
  performance[,i] <-  p
  rownames(performance) <- names(p)
  #performance[[length(performance) + 1]] <- p
}
names(performance) <- names(models)

#-------------------------- Variable importance
var_importance <- list()
for (i in 1:4){     # Random Forest do not have variable importance with Caret
  vi <- models[[i]] %>% varImp()
  var_importance[[length(var_importance) + 1]] <- vi
}
names(var_importance) <- names(models)[1:4]
var_importance

# Plot Variable Importance
vip <- lapply(var_importance, plot, top = 20)
names(vip) <- names(models)[1:4]

vip_1 <- plot_grid(vip[[1]], vip[[2]], vip[[3]], vip[[4]], ncol=2, nrow=2, labels = names(models)[1:4])
vip_1


#----------------- Summaries of Outputs. 
summaries <- lapply(models, summary)

# Best Hyperparameters
best_hp <- list()
for (i in 1:length(models)) {
  bhp <- models[[i]]$bestTune
  best_hp[[length(best_hp) + 1]] <- bhp
}




# DNN


# Load dnn
dnn <- load_model_tf(here("model", "dnn", "nn_abs_new"))


# Predict dnn
predictions_test <- dnn %>% predict(x = test_data)
predictions_train <- dnn %>% predict(x = train_data)

# Calculate RMSE
rmse(test_targets, predictions_test)
rmse(train_targets, predictions_train)


