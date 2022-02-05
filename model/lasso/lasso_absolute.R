# Lasso Full model 

## Setting: 
# We choose a grid of λ values, and compute the cross-validation error for each value of λ.
# The unregularized case is λ = 0.01
# We expect the coefficient estimates to be much smaller, in terms of ℓ2 norm, when a large value of λ is used. 
# then select the tuning parameter value for which the cross-validation error is smallest. 
# Finally, the model is re-fit using all of the available observations and the selected value of the tuning parameter.


# Dependencies 
# install.packages("glmnet")
# install.packages("ISLR2")
library(glmnet)
library(here)
library(ISLR2)
options(scipen=999)

# Load full data
load(here("data", "abs_model_data.Rda"))


# glmnet asks for a matrix input and a y vector
x_t <- model.matrix(price ~ ., train)[, -1] # x_t stands for training
y_t <- train$price

x_v <- model.matrix(price ~ ., valid)[, -1] # x_v stands for validation
y_v <- valid$price

x <- model.matrix(price ~ ., full)[, -1] # combined data set of train and valid. 
y <- full$price

# Check for rank: everything is full ranked. 
qr(x)$rank # 336
qr(x_t)$rank # 336
qr(x_v)$rank # 323


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
cv.out <- cv.glmnet(x_t, y_t, alpha = 1)
plot(cv.out)
title("x = log(\lamda), y = MSE", line = 2.5)

# ID best lambda value
bestlam <- cv.out$lambda.min
log(bestlam) # log lambda = 0.1308595 for plot interpretation
bestlam  # 1.139808


# Calculate Test RMSE for best Lambda: MSE: 3232.358, RSM: 56.85383, slight improvement over collinear model.
lasso.pred <- predict(lasso.mod , s = bestlam , newx = x_v)
sqrt(mean (( lasso.pred - y_v)^2))
# Comparison: lambda of a model fitted with an intercept: MSE: 5025.173, RSME: 70.88845
sqrt(mean (( mean(y_t) - y_v)^2))


# Fit model with full data:
out <- glmnet(x, y, alpha = 1, lambda  = grid)
lasso.coef <- predict(out , type = "coefficients", s = bestlam)#[1:20, ]

# See coefficients that weren't set to 0. 
apply(lasso.coef,2, sort, decreasing = TRUE)

# That's more like it. 






