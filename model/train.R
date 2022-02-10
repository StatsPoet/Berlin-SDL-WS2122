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
load(here("data", "metric_pic_abs.Rda"))

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

# Check for rank: abs: 72 i.e dim(work)-1 to full ranked
qr(x)$rank == ncol(work)-1
qr(x_t)$rank == ncol(work)-1
qr(x_v)$rank == ncol(work)-1


# Train Models
#---------------------------------------

# OLS for reference
set.seed(69)
ols <- train(
  x_t,
  y_t,
  method = "lm",
)
out_ols <- summary(ols)

# OLS with R for testing package equivalence. 
ols_r <- lm(price ~ ., train)
out_ols_r <- summary(ols_r)


# Lasso
##  Create grid of possible lambda values. 
## glmnet wouldn't ask for this for CV but caret does. 
grid <- 10^seq(10, -2, length = 100)


# Fit lasso with bootstrap: 
##  Caret does bootstrap sampling of the data as a validation method. 
## 25 boostrap samples are drawn and the model is thus fitted 25 times. 
##  This method is the default lasso fitting method in caret.
set.seed(69)
l_bs <- train(
  x_t,
  y_t,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid)
)

# Fit a lasso with 10 fold -cross validation
set.seed(69)

# Set the control grid for setting the CV
ctrl_l_cv <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

# lasso CV
l_cv <- train(
  x_t,
  y_t,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid),
  trControl = ctrl_l_cv
  
)

# Final Models i.e models fitted in the whole dataset. 
# --------------------
# Lasso BS
set.seed(69)
fit_final <- trainControl(method = "none")
l_bs_final <- train(
  x,
  y,
  method = "glmnet",
  tuneGrid = data.frame(alpha = 1,
                        lambda = l_bs$finalModel$lambdaOpt),
  trControl = fit_final
  
)


# Lasso CV
set.seed(69)
fit_final <- trainControl(method = "none")
l_cv_final <- train(
  x,
  y,
  method = "glmnet",
  tuneGrid = data.frame(alpha = 1,
                        lambda = l_cv$finalModel$lambdaOpt),
  trControl = fit_final
  
)


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
     x_t,
     x_v,
     y,
     y_t,
     y_v,  file = here("model", "models.Rda"))

