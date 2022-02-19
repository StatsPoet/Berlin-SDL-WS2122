## Compare Outputs
library(glmnet)
library(caret)
library(here)
library(dplyr)
#library(ModelMetrics)
options(scipen=0)

# Load dnn
model <- load_model_tf("nn_abs_new")


# Predict dnn
predictions_test <- model %>% predict(x = test_data)
predictions_train <- model %>% predict(x = train_data)

# Calculate RMSE
rmse(test_targets, predictions_test)
rmse(train_targets, predictions_train)



# Load full data
load(here("model", "ols_lasso.Rda"))
load(here("model", "boost.Rda"))
load(here("model", "rf.Rda"))
load(here("data", "berlin", "b_metric_pic_abs_temp.Rda"))
data <- metric_pic_abs_temp
rm(metric_pic_abs_temp)

# Train data
x_tr <- model.matrix(price ~ ., train)[,-1] # x_tr stands for training
y_tr <- train$price

# Validation data
x_tst<-  model.matrix(price ~ ., test)[,-1] # x_tst stands for validation
y_tst<- test$price

# Full data
x <-  model.matrix(price ~ ., data)[,-1] # combined data set of train and valid.
y <- data$price


# Model Predictions
#-----------------------------------------------

# Run the model to predict coefficients on the validation set. 
predictions <- list()
for (i in 1:5){
  p <- models[[i]] %>% predict(x_v)
  predictions[[length(predictions) + 1]] <- p
}
# For OLS with R the input needs to be different 
predictions[[length(predictions) + 1]] <- models$`OLS with R` %>% predict(valid)

names(predictions) <- names(models)


# Model Diagnostics 
#------------------------------------
# Tell me the best Lambdas for the lassos.
print(paste0('Lasso Bootstrap best lambda parameters: ' , 
             models$`Lasso Bootstrap`$finalModel$lambdaOpt))
print(paste0('Lasso CV best lambda parameters: ' , 
             models$`Lasso 10 CV`$finalModel$lambdaOpt))


# Fit analysis in terms of RMSE, Rsquared and MAE
performance <- data.frame(matrix(1,3))
for (i in 1:length(predictions)){
  p <- predictions[[i]] %>% postResample(y_v)
  performance[,i] <-  p
  rownames(performance) <- names(p)
  #performance[[length(performance) + 1]] <- p
}
names(performance) <- names(models)


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


# # Variable importance
# par(mfrow = c(1,2))
# 
# varImpPlot(l_cv, n.var = 5)
# varImpPlot(l_cv$finalModel, n.var = 5)





# collect Resamples
# ------------------------


trained_models <- list(lBS = l_bs,
                       LCV = l_cv
                       ,
                       LBS_final = l_bs_final,
                       LV_final = l_cv_final,
                       OLS = ols,
                       OLS_R = ols_r
                       )




results <- resamples(list(l_bs, ols))

results2 <- resamples(list(a = l_cv, b = l_bs))


# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)


options(scipen=0)




