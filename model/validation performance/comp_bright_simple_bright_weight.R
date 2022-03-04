library(glmnet)
library(caret)
library(here)
library(dplyr)
library(keras)
library(gbm)
library(purrr)
library(gridExtra)
library(ggplot2)
library(cowplot)

# load data
load(here("data","berlin", "b_metric_pic_abs_temp_simple_bright.Rda"))

bright_simple <- subset(x = metric_pic_abs_temp, select = c(brightness_mean))
prices <- subset(x = metric_pic_abs_temp, select = c(price))

load(here("data","berlin", "b_metric_pic_abs_temp.Rda"))
bright_weight <- subset(x = metric_pic_abs_temp, select = c(brightness_mean))

df <- data.frame(c(prices, bright_weight, bright_simple))

plot(cor(df))


## Compare variable importance
load(here("model", "final train performance", "ols_lasso_best.Rda"))
vi_1 <- ols_lasso_best[[4]] %>% varImp()
vi_1

load(here("model", "final train performance", "lasso_SB_best.Rda"))

vi_2 <- lasso_SB_best[[1]] %>% varImp()
vi_2
