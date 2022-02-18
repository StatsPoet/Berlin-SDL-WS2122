#------ Boosting with Caret nach ISLR
library(glmnet)
library(caret)
library(here)
library(dplyr)
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


#------------------- Export models 
boost_models <- list(boost, boost_centered)
names(boost_models) <- c("Boost", "Boost Centered")
save(boost_models,  file = here("model", "boost.Rda"))
