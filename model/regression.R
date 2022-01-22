##### (Simple Linear) Regression

mvars <- read_csv(here("data","1_mvars.csv"), col_types = cols(...1 = col_skip()))
names(mvars)


model <- lm(price ~ ., mvars)
output <- summary(model)
output