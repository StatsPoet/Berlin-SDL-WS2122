# Lasso Full model 

## Lasso: Linere regression mit Penalty für die Große der Betas: L2 norms:
# zu diese SS, die ich minimiere, l2 die grosserer Betas betraft. 
# \lambda ist mein Tunning parametern und bestimmt, wie stark mein Betas bestraft werden. 

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
load(here("data", "model_data.Rda"))


# glmnet asks for a matrix input and a y vector
x_t <- model.matrix(price ~ ., train)[, -1] # x_t stands for training
y_t <- train$price

x_v <- model.matrix(price ~ ., valid)[, -1] # x_v stands for validation
y_v <- valid$price

x <- model.matrix(price ~ ., full)[, -1] # combined data set of train and valid. 
y <- full$price


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
colnames(train[317])
# Drawer_pic_10 wtf?

colnames(train[293])
# Drawer_pic_9 wtf?

## CV Lasso
set.seed (69)
cv.out <- cv.glmnet(x_t, y_t, alpha = 1)
plot(cv.out)
title("x = log(\lamda), y = MSE", line = 2.5)

# ID best lambda value
bestlam <- cv.out$lambda.min
log(bestlam) # log lambda = 1.247264 for plot interpretation
bestlam  # 3.480808 


# Calculate Test RMSE for best Lambda: MSE: 3314.171, RSME: 57.56884
lasso.pred <- predict(lasso.mod , s = bestlam , newx = x_v)
sqrt(mean (( lasso.pred - y_v)^2))
# Comparison: lambda of a model fitted with an intercept: MSE: 5025.173, RSME: 70.88845
sqrt(mean (( mean(y_t) - y_v)^2))


# Fit model with full data:
out <- glmnet(x, y, alpha = 1, lambda  = grid)
lasso.coef <- predict(out , type = "coefficients", s = bestlam)#[1:20, ]

# See coefficients that weren't set to 0. 
apply(lasso.coef,2, sort, decreasing = TRUE)

# It makes no sense that a fireplace in the tenth picture brings such an improvement in the model. Something is wrong. 
# Coefficient for drawer_pic_10
lasso.coef[317]
# i.e the variable shown to have the highest performance in the first plot now performs badly. 

## To Do: Explanatory power isn't given. Variable selection makes no sense. MSE does improves from baseline. 

# problems: rank of matrix
qr(x)$rank # 336
qr(x_t)$rank # 336
qr(x_v)$rank # 323

## That is a lot of linear dependence. I don't know why the validation test has considerable lower rank as the first one. 




# 2. Training and validation set:
valid_id <- sample (1:nrow(full), floor(0.9 * nrow(full)))
train <- full[valid_id,]
valid <- full[-valid_id,]
