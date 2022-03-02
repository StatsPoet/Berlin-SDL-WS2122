# load data
load(here("data","berlin", "b_metric_pic_abs_temp_simple_bright.Rda"))

bright_simple <- subset(x = metric_pic_abs_temp, select = c(brightness_mean))
prices <- subset(x = metric_pic_abs_temp, select = c(price))

load(here("data","berlin", "b_metric_pic_abs_temp.Rda"))
bright_weight <- subset(x = metric_pic_abs_temp, select = c(brightness_mean))

df <- data.frame(c(prices, bright_weight, bright_simple))

plot(cor(df))


