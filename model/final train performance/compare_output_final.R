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
load(here("data", "munich", "m_metric_pic_abs_temp.Rda"))
data <- metric_pic_abs_temp
rm(metric_pic_abs_temp)

# Full data
x <-  model.matrix(price ~ ., data)[,-1] # combined data set of train and valid.
y <- data$price


# Load models 
load(here("model", "final train performance", "ols_lasso_best.Rda"))
load(here("model", "final train performance", "boost_best.Rda"))
load(here("model", "final train performance", "rf_best.Rda"))

# Merge Lists
models <- c(ols_lasso_best[1:3], boost_models_best[1:2], rf_models_best[1:2], ols_lasso_best[4], boost_models_best[3],rf_models_best[3])
names(models) <- c("OLS",
                   "Lasso",
                   "Lasso N",
                   "Boost",
                   "Boost N",
                   "RF",
                   "RF N",
                   "Lasso N-log(price)",
                   "Boost N-log(price)",
                   "RF N-log(price)"
                   )


#--------------- Model Predictions
# Run the model to predict coefficients on the validation set. 
predictions <- list()
for (i in 1:length(models)){
  p <- models[[i]] %>% predict(x)
  predictions[[length(predictions) + 1]] <- p
}
names(predictions) <- names(models)

predictions_log <- lapply(predictions[1:7], log)
predictions_log <- c(predictions_log, predictions[8:10])



#------------------- Performance
# Fit analysis in terms of RMSE, Rsquared and MAE
performance <- data.frame(matrix(1,3))
for (i in 1:length(predictions_log)){
  p <- predictions_log[[i]] %>% postResample(log(y))
  performance[,i] <-  p
  rownames(performance) <- names(p)
  #performance[[length(performance) + 1]] <- p
}
names(performance) <- names(models)


#-------------------------- Variable importance
vip_models <- c(models[1:5], models[8:9])


var_importance <- list()
for (i in 1:length(vip_models)){     # Random Forest do not have variable importance with Caret
  vi <- vip_models[[i]] %>% varImp()
  var_importance[[length(var_importance) + 1]] <- vi
}
names(var_importance) <- names(vip_models)[1:7]
var_importance

# Plot Variable Importance
vip <- lapply(var_importance, plot, top = 20)
names(vip) <- names(vip_models)

vip_lasso <- plot_grid(vip[[1]], vip[[2]], vip[[3]], vip[[6]], ncol=2, nrow=2, labels = names(vip_models)[c(1:3,6)])
vip_lasso

vip_boost <- plot_grid(vip[[4]], vip[[5]], vip[[7]], ncol=2, nrow=2, labels = names(vip_models)[c(4,5,7)])
vip_boost



#----------------- Summaries of Outputs. 
summaries <- lapply(models, summary)

#------------------- Best Hyperparameters
best_hp <- list()
for (i in 1:length(models)) {
  bhp <- models[[i]]$bestTune
  best_hp[[length(best_hp) + 1]] <- bhp
}




#--------- Expor
save(predictions,
     predictions_log,
     performance,
     best_hp,
     summaries,
     vip,
     vip_lasso,
     vip_boost,
     var_importance,
     file = here("model", "final train performance", "final_results.Rda"))


# 
# # Base RMSE
# sqrt(mean((mean(y_tr) - y_tst)^2))
# 
# # Log RMSE
# sqrt(mean((mean(log(y_tr)) - log(y_tst))^2))


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
