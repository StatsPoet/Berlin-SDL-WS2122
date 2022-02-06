# Lasso with Caret 


# Dependencies 
# install.packages("glmnet")
# install.packages("ISLR2")
library(glmnet)
library(caret)
library(here)
library(ModelMetrics)

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

# Train data
x_t <- model.matrix(price ~ ., train)[,-1] # x_t stands for training
y_t <- train$price

# Validation data
x_v <-  model.matrix(price ~ ., valid)[,-1] # x_v stands for validation
y_v <- valid$price

# Full data
x <-  model.matrix(price ~ ., work)[,-1] # combined data set of train and valid.
y <- work$price

# Check for rank: abs: 73 to full ranked
qr(x)$rank
qr(x_t)$rank
qr(x_v)$rank 


# Create grid
grid <- 10^seq(10, -2, length = 100)


# Fit model 
lasso.mod <- glmnet(x_t, y_t, alpha = 1, lambda = grid) # alpha = 1 is lasso. alpha = 0 is ridge. 


# Check dimensions: 338 variables for 100 lamda values. 
dim(coef(lasso.mod))

# Plot: Interpretation:
plot(lasso.mod, label = TRUE)
plot(lasso.mod,xvar="lambda",label=TRUE)

## Analysis: 

# Based on the plot variable 317 has some importance: 
colnames(train[3])
# Bedrooms

colnames(train[2])
# Accomodates

## CV Lasso
set.seed (69)
cv.out <- cv.glmnet(x_t, y_t, alpha = 1, lambda = grid) 

# if i don't imput a grid of lambda values glmnet does it by itself. 
# Caret ask me to imput a grid. There's no auto generating mechanism. Results are the same. 


plot(cv.out)
title("x = log(\lamda), y = MSE", line = 2.5)

# ID best lambda value
bestlam <- cv.out$lambda.min
log(bestlam) # log lambda = -0.381616 for plot interpretation
bestlam  # 0.6827572


# Calculate Test RMSE for best Lambda: MSE: , RMSE: 50.10559, slight improvement over collinear model.
lasso.pred <- predict(lasso.mod , s = bestlam , newx = x_v)
sqrt(mean (( lasso.pred - y_v)^2))
# Comparison: lambda of a model fitted with an intercept: , RSME: 66.79612
sqrt(mean (( mean(y_t) - y_v)^2))



# Fit model with full data:
out <- glmnet(x, y, alpha = 1)#, lambda  = grid)
lasso.coef <- predict(out , type = "coefficients", s = bestlam)#[1:20, ]

# See coefficients that weren't set to 0. 
apply(lasso.coef,2, sort, decreasing = TRUE)

# That's more like it. 



############################ Lasso with Caret
set.seed(69)

l_car <- train(
  x_t,
  y_t,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid)
  #,
  #trControl = ctrl
  
)
l_car$bestTune

#



# Lasso with Caret and cross validation
set.seed(69)

ctrl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


lcv_car <- train(
  x_t,
  y_t,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid),
  trControl = ctrl
  
)
lcv_car$bestTune

