### Global Data Partitioning
## Description: Creating of a training, validation and test sets. 
## The Test Set will be used only a week before report submission. To keep us from milk the data to much.
## Thus, as k-fold CV will be used, the validation set will act as a factual set for all purposes. 


#### !!!!!!!!!! Uncommnent the files you want to update !!!!!!!!!!!!!!!!!1


# Dependencies
library(here)

# Load full data
# load(here("data","clean_data","4_fulldata.Rda"))
# data <- image_an_mvars_df

# Load full data with dummies.
# load(here("data","clean_data","5_fulldata_dum.Rda"))
# data <- image_an_mvars_df_ind_pic_only

# # Load full data with absolute frequencies.
# load(here("data","clean_data","6_fulldata_abs.Rda"))
# data <- image_an_mvars_df_sums_only


# Proportion 80:10:10
set.seed (420)

# 1. Separate test set for final evaluation:
test_id <- sample (1:nrow(data), floor(0.9 * nrow(data)))
full <- data[test_id,]
test <- data[-test_id,]

# 2. Training and validation set:
valid_id <- sample (1:nrow(full), floor(0.9 * nrow(full)))
train <- full[valid_id,]
valid <- full[-valid_id,]


# Inspection
nrow(train) + nrow(valid) + nrow(test) == nrow(data)

# View(train[0:10,])
# View(valid[0:10,])
# View(test[0:10,])

# # Save full
# save(train, valid , full, file = here("data", "model_data.Rda"))
# save(test, file = here("data", "test_data_USE_LAST_USE_ONCE", "model_test.Rda"))

# # Save dummies data
# save(train, valid, full , file = here("data", "dum_model_data.Rda"))
# save(test, file = here("data", "test_data_USE_LAST_USE_ONCE", "dum_model_test.Rda"))

# # Save absolute frequencies data
# save(train, valid , file = here("data", "abs_model_data.Rda"))
# save(test, file = here("data", "test_data_USE_LAST_USE_ONCE", "abs_model_test.Rda"))
