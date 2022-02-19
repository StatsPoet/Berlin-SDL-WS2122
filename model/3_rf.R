### Random Forests
# install.packages("e1071")
# install.packages("ranger")
library(caret)
library(here)
library(dplyr)
library(e1071)
library(ranger)
library(here)
options(scipen=999)

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

#------------------- Random Forests with ranger
# Random Forest
# 
# method = 'ranger'
# 
# Type: Classification, Regression
# 
# Tuning parameters:
#   
#   mtry (#Randomly Selected Predictors)
#     splitrule (Splitting Rule)
#     min.node.size (Minimal Node Size)
#     
#     Required packages: e1071, ranger, dplyr
#     
#     A model-specific variable importance metric is available.

# For Regression usually mtry = p/3

p <- dim(data)[2]
p_3 <- round(p/3)

set.seed(69)
rf_grid <- expand.grid(
  .mtry = c(-7 + p_3, p_3,  7 + p_3), # only three options 
  .splitrule = "variance",
  .min.node.size = c(5)
)

ctrl_cv <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

#Only these three are supported by caret and not the number of trees. 
#In train you can specify num.trees and importance:

set.seed(69)
rf <- train(x_tr,
            y_tr,
            method = "ranger",
            trControl = ctrl_cv,
            tuneGrid = rf_grid, 
            metric = "RMSE" )

set.seed(69)
rf_centered <- train(x_tr,
                     y_tr,
                     trControl = ctrl_cv,
                     method = "ranger",
                     tuneGrid = rf_grid,
                     metric = "RMSE",
                     preProc = c("center", "scale"))


#------------------- Export models 
rf_models <- list(rf, rf_centered)
names(rf_models) <- c("RF","RF Centered")
save(rf_models,  file = here("model", "rf.Rda"))


# Best Hyperparameters
best_hp <- list()
for (i in 1:length(rf_models)) {
  bhp <- rf_models[[i]]$bestTune
  best_hp[[length(best_hp) + 1]] <- bhp
}
best_hp

## Final Train

ctrl_ff <- trainControl(method = "none")

ff_grid <- expand.grid(
  .mtry = 21, # only three options 
  .splitrule = "variance",
  .min.node.size = c(5)
)



f_rf <- train(x,
            y,
            method = "ranger",
            trControl = ctrl_ff,
            tuneGrid = ff_grid, 
            metric = "RMSE" )

f_rf_s <- train(x,
              y,
              trControl = ctrl_ff,
              method = "ranger",
              tuneGrid = ff_grid,
              metric = "RMSE",
              preProc = c("center", "scale"))


f_rf_slog <- train(x,
                  log(y),
                  trControl = ctrl_ff,
                  method = "ranger",
                  tuneGrid = ff_grid,
                  metric = "RMSE",
                  preProc = c("center", "scale"))


rf_models_best <- list(f_rf, f_rf_s, f_rf_slog)
names(rf_models_best) <- c("RF-f","RF Standatd-f", "RF Standard Log(price)-f")
save(rf_models_best,  file = here("model", "rf_best.Rda"))
