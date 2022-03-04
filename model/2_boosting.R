#------ Boosting with Caret nach ISLR
library(caret)
library(here)
library(dplyr)
library(gbm)
library(ranger)
options(scipen=999)

# Load full data
load(here("data", "berlin", "b_metric_pic_abs_temp.Rda"))
data <- metric_pic_abs_temp
rm(metric_pic_abs_temp)


x_tr <- model.matrix(price ~ ., train)[,-1] # x_tr stands for training
y_tr <- train$price
x_tst<-  model.matrix(price ~ ., test)[,-1] # x_tst stands for validation
y_tst<- test$price
x <-  model.matrix(price ~ ., data)[,-1] # combined data set of train and valid.
y <- data$price

#### -------------------- Boosting Trees for regression nach ISLR with Caret
# Stochastic Gradient Boosting
# 
# method = 'gbm'
# 
# Type: Regression, Classification
# 
# Tuning parameters:
#   
#   n.trees (# Boosting Iterations)
#     interaction.depth (Max Tree Depth)
#     shrinkage (Shrinkage)
#     n.minobsinnode (Min. Terminal Node Size)
#     
#     Required packages: gbm, plyr
#     
#     A model-specific variable importance metric is available.


#  Cross Validation for controlling:

ctrl_cv <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Set the grid of parameters to be optimized
set.seed(69)
gbm_grid <- expand.grid(interaction.depth = 1,
                        shrinkage = seq(0.001, 0.202, 0.04), # 3 Parameters only. 
                        n.trees = c(5000),
                        n.minobsinnode = 10)

# Boost
set.seed(69)
boost <- train(x_tr,
               y_tr,
               trControl = ctrl_cv,
               method = "gbm",
               tuneGrid = gbm_grid,
               metric = "RMSE",
               #preProc = c("center", "scale"),
               distribution = "gaussian")

# Boost with preprocesing. 
set.seed(69)
boost_centered <- train(x_tr,
                        y_tr,
                        trControl = ctrl_cv,
                        method = "gbm",
                        tuneGrid = gbm_grid,
                        metric = "RMSE", 
                        preProc = c("center", "scale"),
                        distribution = "gaussian")


# Best Hyperparameters
boost$bestTune

# Save results of hyperparameter Tuning

boost_models <- list(boost)
#, boost_centered)
names(boost_models) <- c("Boost")
#,"Boost Centered")
save(boost_models, boost,  file = here("model", "validation performance", "hpt", "boost.Rda"))


#------------------- Validation
load(here("model", "validation performance", "hpt", "boost.Rda"))

# Set control and methods grids
ctrl_ff <- trainControl(method = "none")
fgbm_grid <- expand.grid(interaction.depth = 1,
                         shrinkage = seq(0.001),
                         n.trees = c(5000),
                         n.minobsinnode = 10)

boost <- train(x_tr,
                 y_tr,
                 trControl = ctrl_ff,
                 method = "gbm",
                 tuneGrid = fgbm_grid,
                 metric = "RMSE",
                 #preProc = c("center", "scale"),
                 distribution = "gaussian")

boost_s <- train(x_tr,
                   y_tr,
                   trControl = ctrl_ff,
                   method = "gbm",
                   tuneGrid = fgbm_grid,
                   metric = "RMSE",
                   preProc = c("center", "scale"),
                   distribution = "gaussian")

boost_slog <- train(x_tr,
                      log(y_tr),
                      trControl = ctrl_ff,
                      method = "gbm",
                      tuneGrid = fgbm_grid,
                      metric = "RMSE",
                      preProc = c("center", "scale"),
                      distribution = "gaussian")

boost_models_val <- list(boost, boost_s, boost_slog)
#, boost_centered)
names(boost_models_val) <- c("Boost-f", "Boost S-f", "Boost S log(price)-f")
#,"Boost Centered")
save(boost_models_val,  file = here("model", "validation performance", "boost_val.Rda"))





# --------------------------------------- Test
load(here("model", "boost.Rda"))

# Best Hyperparameters
boost$bestTune

# Set control and methods grids
ctrl_ff <- trainControl(method = "none")
fgbm_grid <- expand.grid(interaction.depth = 1,
                        shrinkage = seq(0.001),
                        n.trees = c(5000),
                        n.minobsinnode = 10)

f_boost <- train(x,
               y,
               trControl = ctrl_ff,
               method = "gbm",
               tuneGrid = fgbm_grid,
               metric = "RMSE",
               #preProc = c("center", "scale"),
               distribution = "gaussian")

f_boost_s <- train(x,
               y,
               trControl = ctrl_ff,
               method = "gbm",
               tuneGrid = fgbm_grid,
               metric = "RMSE",
               preProc = c("center", "scale"),
               distribution = "gaussian")

f_boost_slog <- train(x,
               log(y),
               trControl = ctrl_ff,
               method = "gbm",
               tuneGrid = fgbm_grid,
               metric = "RMSE",
               preProc = c("center", "scale"),
               distribution = "gaussian")

boost_models_best <- list(f_boost, f_boost_s, f_boost_slog)
#, boost_centered)
names(boost_models_best) <- c("Boost-f", "Boost S-f", "Boost S log(price)-f")
#,"Boost Centered")
save(boost_models_best,  file = here("model", "boost_best.Rda"))


