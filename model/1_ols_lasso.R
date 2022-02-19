#------ Lasso with Caret 
# Dependencies 
# install.packages("glmnet")
# install.packages("ISLR2")
# install.packages("e1071")
# install.packages("ranger")


library(glmnet)
library(caret)
library(here)
library(dplyr)

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
  tuneGrid = expand.grid(alpha = 1, lambda = grid_lasso),
  preProc = c("center", "scale"),
  trControl = ctrl_cv
)

# Centered with log price
set.seed(69)
lasso_c_log <- train(
  x_tr,
  log(y_tr),
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid_lasso),
  preProc = c("center", "scale"),
  trControl = ctrl_cv
)




#------------------- Export models 
ols_lasso <- list(ols, lasso, lasso_centered, lasso_c_log)
names(ols_lasso) <- c("OLS", 
                   "Lasso",
                   "Lasso Standard", 
                   "Lasso S + log(price)")
save(ols_lasso,  file = here("model", "ols_lasso.Rda"))

#-------------Last fit with best parameters. 

# Best Hyperparameters
best_hp <- list()
for (i in 1:length(ols_lasso)) {
  bhp <- ols_lasso[[i]]$bestTune
  best_hp[[length(best_hp) + 1]] <- bhp
}
best_hp

#----------------------------------------------------------------------- Final Train
ctrl_ff <- trainControl(method = "none")


# Model Fit:

# OLS for reference
f_ols <- train(
  x,
  y,
  method = "lm",
)

f_lasso <- train(
  x,
  y,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = ols_lasso$Lasso$bestTune$lambda),
  trControl = ctrl_ff
)

# Centered 
f_lasso_c <- train(
  x,
  y,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = ols_lasso$`Lasso Standard`$bestTune$lambda),
  preProc = c("center", "scale"),
  trControl = ctrl_ff
)

# Centered with log price
f_lasso_c_log <- train(
  x,
  log(y),
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = ols_lasso$`Lasso S + log(price)`$bestTune$lambda),
  preProc = c("center", "scale"),
  trControl = ctrl_ff
)


ols_lasso_best <- list(f_ols, f_lasso, f_lasso_c, f_lasso_c_log)
names(ols_lasso_best) <- c("OLS-f", 
                            "Lasso-f",
                            "Lasso S-f", 
                            "Lasso S_log(price)-f")

save(ols_lasso_best,  file = here("model", "ols_lasso_best.Rda"))
