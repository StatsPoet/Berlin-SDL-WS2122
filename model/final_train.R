# Final Train:
# #----------------------------------------
# Main Question: 
## Setting: 
# - Some models are fitted. 
# - The best ones are identified. 
# -> Should I train the model again with all available data to calculate the coefficients 
# as it is done in ISLR?

## I can't fit Lasso with best lambda

set.seed(69)
fit_l_bs <- trainControl(method = "none")
l_bs_best <- train(
  x,
  y,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = grid),
  trControl = ctrl_l_cv
  
)


Lasso_bs_best <- as.data.frame.matrix(coef(l_bs_best$finalModel, l_bs_best$finalModel$lambdaOpt))
betas <- cbind(Lasso_bs_best, betas)
colnames(betas) <- c("Lasso BS best", "Lasso BS", "Lasso CV", "OLS", "OLS_R")

pred_l_bs_best <- l_bs_best %>% predict(x)
performance$Lasso_BS_Best <- postResample(pred_l_bs_best , y)

betas
performance


lbs_final <- as.data.frame.matrix(coef(l_bs_final$finalModel,
                                       l_bs_final$finalModel$lambdaOpt))
betas <- cbind(lbs_final, betas)

pred_l_bs_final <- l_bs_final %>% predict(x)
performance$l_bs_final <- postResample(pred_l_final , y)

betas
performance




lcv_final <- as.data.frame.matrix(coef(l_cv_final$finalModel,
                                       l_cv_final$finalModel$lambdaOpt))
betas <- cbind(lcv_final, betas)
colnames(betas) <- c("lcv final", 
                     "lbs final",
                     "lbs",
                     "lcv",
                     "ols",
                     "ols r")

pred_l_cv_final <- l_cv_final %>% predict(x)
performance$l_bs_final <- postResample(pred_l_cv_final , y)

betas
performance


