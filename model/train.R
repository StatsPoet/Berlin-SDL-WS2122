#------ Lasso with Caret 
# Dependencies 
# install.packages("glmnet")
# install.packages("ISLR2")
install.packages("e1071")
install.packages("ranger")


library(glmnet)
library(caret)
library(here)
library(dplyr)
library(e1071)
library(ranger)
#library(ModelMetrics)

options(scipen=999)

# Load full data
load(here("data", "berlin", "b_metric_pic_abs_temp.Rda"))
data <- metric_pic_abs_temp
rm(metric_pic_abs_temp)

# # Partition data
# set.seed (69)
# id <- createDataPartition(work$price,
#                           p = .9,
#                           list = FALSE,
#                           times = 1)
# train <- work[id, ]
# valid <- work[-id, ]

# glmnet asks for a matrix input and a y vector

# Train data
x_tr <- model.matrix(price ~ ., train)[,-1] # x_tr stands for training
y_tr <- train$price
# Validation data
x_tst<-  model.matrix(price ~ ., test)[,-1] # x_tst stands for validation
y_tst<- test$price
# Full data
x <-  model.matrix(price ~ ., data)[,-1] # combined data set of train and valid.
y <- data$price

# Check for rank: abs: 72 i.e dim(data)-1 to full ranked
qr(x)$rank == ncol(data)-1
qr(x_tr)$rank == ncol(data)-1
qr(x_tst)$rank == ncol(data)-1


# Train Models
#---------------------------------------



# OLS for reference
set.seed(69)
ols <- train(
  x_tr,
  y_tr,
  method = "lm",
)

#--------------------------- Lasso
# glmnet
# 
# method = 'glmnet'
# 
# Type: Regression, Classification
# 
# Tuning parameters:
#   
#   alpha (Mixing Percentage)
# lambda (Regularization Parameter)
# 
# Required packages: glmnet, Matrix
# 
# A model-specific variable importance metric is available.


##  Create grid of possible lambda values. 
## glmnet wouldn't ask for this for CV but caret does. 
grid_lasso <- 10^seq(10, -2, length = 100)

# lasso CV: Fit a lasso with 10 fold -cross validation
## Set the control grid for setting the CV
ctrl_cv <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Model Fit:
set.seed(69)
lasso <- train(
  x_tr,
  y_tr,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid_lasso),
  trControl = ctrl_cv
  
)

# Centered 
set.seed(69)
lasso_centered <- train(
  x_tr,
  y_tr,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid),
  preProc = c("center", "scale"),
  trControl = ctrl_cv
  
)


#-------------------- Boosting Trees for regression nach ISLR with Caret
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



# Set the grid of parameters to be optimized
grid_gbm <- expand.grid(interaction.depth = 1:2,
                       shrinkage = c(0.001, 0.1, 0.2),
                       n.trees = c(100, 5000),
                       #n.minobsinnode = 10)
                       )
                       

# Set the control grid for setting for Out of Bag observations and no method for reference. 
ctrl_obb <- trainControl(method = "oob")



set.seed(69)
boost <- train(x_tr,
               y_tr,
               trControl = ctrl_cv,
               method = "gbm",
               tuneGrid = rf_grid,
               metric = "RMSE", 
               #preProc = c("center", "scale"),
               distribution = "gaussian")
               

set.seed(69)
boost_centered <- train(x_tr,
                        y_tr,
                        trControl = ctrl_cv,
                        method = "gbm",
                        tuneGrid = rf_grid,
                        metric = "RMSE", 
                        preProc = c("center", "scale"),
                        distribution = "gaussian")




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
  .mtry = seq(p_3-5, p_3 + 5),
  .splitrule = "variance",
  .min.node.size = c(5,10,15)
)

#Only these three are supported by caret and not the number of trees. 
#In train you can specify num.trees and importance:
set.seed(69)
rf_nometric <- train(x_tr,
            y_tr,
            method = "ranger",
            trControl = ctrl_cv,
            tuneGrid = rf_grid 
            #metric = "RMSE"
            )

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

#----------------------------- Export Results 
models <- list(l_bs,
               l_bs_final,
               l_cv,
               l_cv_final,
               ols,
               ols_r)
names(models) <- c("Lasso Bootstrap", 
                   "Lasso BS final",
                   "Lasso 10 CV",
                   "Lasso 10 CV final",
                   "OLS",
                   "OLS with R"
                   )

save(models,
     valid,
     x,
     x_tr,
     x_tst,
     y,
     y_tr,
     y_v,  file = here("model", "models.Rda"))

