#------ Boosting with Caret nach ISLR
library(caret)
library(here)
library(dplyr)
library(gbm)
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
                        shrinkage = seq(0.001, 0.202, 0.04),
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


#------------------- Export models 
boost_models <- list(boost)
                     #, boost_centered)
names(boost_models) <- c("Boost")
                         #,"Boost Centered")
save(boost_models, boost,  file = here("model", "boost.Rda"))
