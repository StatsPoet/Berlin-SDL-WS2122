## Unused models and code 



performance <- data.frame(
  Lasso_BS = postResample(pred_l_bs , y_v),
  Lasso_CV = postResample(pred_l_cv , y_v),
  OLS = postResample(pred_ols, y_v),
  OLS_R = postResample(pred_ols2, y_v)
  
)

# Fit lasso with bootstrap: 
##  Caret does bootstrap sampling of the data as a validation method. 
## 25 boostrap samples are drawn and the model is thus fitted 25 times. 
##  This method is the default lasso fitting method in caret.
set.seed(69)
l_bs <- train(
  x_tr,
  y_tr,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid)
)


# OLS with R for testing package equivalence. 
ols_r <- lm(price ~ ., train)
out_ols_r <- summary(ols_r)

