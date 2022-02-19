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


# I still need to write this loop!!



coefficients <- data.frame(matrix())


perf = as.data.frame.matrix(coef(models[[i]]$finalModel, models[[i]]$finalModel$lambdaOpt))
data_3 <- cbind(data, new_col = vec)

# Export coefficients of final model trained on the whole data. 
betas <- data.frame(
  l_bs = as.data.frame.matrix(coef(l_bs$finalModel, l_bs$finalModel$lambdaOpt)), 
  l_cv = as.data.frame.matrix(coef(l_cv$finalModel, l_cv$finalModel$lambdaOpt)),
  ols = ols$finalModel$coefficients,
  ols_r = ols_r$coefficients
) #%>%   rename(l_nocv = s1, l_cv = s1.1)

betas



