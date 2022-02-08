#------ Lasso with Caret 
# Dependencies 
# install.packages("glmnet")
# install.packages("ISLR2")
library(glmnet)
library(caret)
library(here)
library(dplyr)
#library(ModelMetrics)

options(scipen=999)

# Load full data
load(here("data", "abs_model_data.Rda"))

# Partition data
set.seed (69)
id <- createDataPartition(work$price,
                          p = .9,
                          list = FALSE,
                          times = 1)
train <- work[id, ]
valid <- work[-id, ]

# glmnet asks for a matrix input and a y vector
# I still keep it for caret. 

# Train data
x_t <- model.matrix(price ~ ., train)[,-1] # x_t stands for training
y_t <- train$price

# Validation data
x_v <-  model.matrix(price ~ ., valid)[,-1] # x_v stands for validation
y_v <- valid$price

# Full data
x <-  model.matrix(price ~ ., work)[,-1] # combined data set of train and valid.
y <- work$price

# Check for rank: abs: 72 i.e dim(work)-1 to full ranked
qr(x)$rank == ncol(work)-1
qr(x_t)$rank == ncol(work)-1
qr(x_v)$rank == ncol(work)-1

#---- OLS for reference.
set.seed(69)
ols <- train(
  x_t,
  y_t,
  method = "lm",
)

# ols as we learned for testing package equivalence. 
ols_r <- lm(price ~ ., train)
out_ols_r <- summary(model)


#---- Lasso
# Create grid of possible lambda values. 
# glmnet wouldn't ask for this for CV but caret does. 
grid <- 10^seq(10, -2, length = 100)


#---- Fit lasso with bootstrap: 
# Caret does bootstrap sampling of the data as a validation method. 
# 25 boostrap samples are drawn and the model is thus fitted 25 times. 
# This method is the default lasso fitting method in caret.
set.seed(69)
l_bs <- train(
  x_t,
  y_t,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid)
)

#---- Fit a lasso with 10 fold -cross validation
set.seed(69)

# Set the parameter grid 
ctrl_l_cv <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# Run lasso cv
l_cv <- train(
  x_t,
  y_t,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid),
  trControl = ctrl_l_cv
  
)

# Run the model to predict coefficients on the validation set. 
pred_l_bs <- l_bs %>% predict(x_v)
pred_l_cv <- l_cv %>% predict(x_v)
pred_ols <- ols %>% predict(x_v)
pred_ols2 <- ols_r %>% predict(valid)

#----- Diagnostics 
# Tell me the best Lambdas for the lassos.
print(paste0('Lasso Bootstrap best lambda parameters: ' , l_bs$finalModel$lambdaOpt))
print(paste0('Lasso CV best lambda parameters: ' , l_cv$finalModel$lambdaOpt))


# Fit: 
## R2
c_determination <- data.frame(
  Lasso_R2 = R2(pred_l_bs , y_v),
  Lasso_CV_R2 = R2(pred_l_cv , y_v),
  OLS_caret_R2 = R2(pred_ols, y_v), 
  OLS_r_R2 = out$r.squared
)

print(c_determination)


## RMSE
error <- data.frame(
  Lasso_BS_RMSE = RMSE(pred_l_bs , y_v),
  Lasso_CV_RMSE = RMSE(pred_l_cv , y_v),
  OLS_R2 = RMSE(pred_ols, y_v),
  OLS_r_RMSE = RMSE(pred_ols2, valid$price)
  
)
print(error)

# R2 is diffrent but RMSE is the same, why?

# Give me the betas 
betas <- data.frame(
  l_nocv = as.data.frame.matrix(coef(l_bs$finalModel, l_nocv$finalModel$lambdaOpt)), 
  l_cv = as.data.frame.matrix(coef(l_cv$finalModel, l_cv$finalModel$lambdaOpt))
  # linear = (linear$finalModel$coefficients)
) %>%   rename(l_nocv = s1, l_cv = s1.1)

betas

# Save r objetcs

