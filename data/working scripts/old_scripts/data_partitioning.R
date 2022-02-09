## Global Data Partitioning
# Description: Create an initial data partition to leave a final test set out, that's only going 
# to be used for the last fitting moment.

# Note: initially, the partition process  took place here. 
# Caret offers a more efficient way to streamline the process. 
# Thus, train/validation partition is relegated to every single model for consistency, 

# Dependencies
library(here)
library(caret)

#--------------- Full Data

# Load full data
load(here("data","clean_data","4_fulldata.Rda"))
data <- image_an_mvars_df

# Proportion 90:10
set.seed (69)
id <- createDataPartition(data$price, p = .9, 
                          list = FALSE, 
                          times = 1)
work <- data[id,]
test <- data[-id,]
# Inspection
nrow(work) + nrow(test)  == nrow(data)

# View(work[0:10,])
# View(test[0:10,])

# Save full
save(work, file = here("data", "model_data.Rda"))
save(test, file = here("data", "test_data_USE_LAST_USE_ONCE", "model_test.Rda"))

#------------------------- Dummies
rm(data, work, test, id)
# Load full data with dummies.
load(here("data","clean_data","5_fulldata_dum.Rda"))
data <- image_an_mvars_df_ind_pic_only

# Proportion 90:10
set.seed (69)
id <- createDataPartition(data$price, p = .9, 
                          list = FALSE, 
                          times = 1)
work <- data[id,]
test <- data[-id,]

# Inspection
nrow(work) + nrow(test)  == nrow(data)


# Save full
save(work, file = here("data", "dum_model_data.Rda"))
save(test, file = here("data", "test_data_USE_LAST_USE_ONCE", "dum_model_test.Rda"))

#------------------------- Absolute frequencies
rm(data, work, test, id)
# Load full data with absolute frequencies.
load(here("data","clean_data","6_fulldata_abs.Rda"))
data <- image_an_mvars_df_sums_only

# Proportion 90:10
set.seed (69)
id <- createDataPartition(data$price, p = .9, 
                                  list = FALSE, 
                                  times = 1)
work <- data[id,]
test <- data[-id,]
# Inspection
nrow(work) + nrow(test)  == nrow(data)

# View(work[0:10,])
# View(test[0:10,])


# Save absolute frequencies data
save(work,  file = here("data", "abs_model_data.Rda"))
save(test, file = here("data", "test_data_USE_LAST_USE_ONCE", "abs_model_test.Rda"))

