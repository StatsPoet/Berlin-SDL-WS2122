## Unused models and code 



performance <- data.frame(
  Lasso_BS = postResample(pred_l_bs , y_v),
  Lasso_CV = postResample(pred_l_cv , y_v),
  OLS = postResample(pred_ols, y_v),
  OLS_R = postResample(pred_ols2, y_v)
  
)