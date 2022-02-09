### Data Cleaning 
# Sebastian
library(dplyr)
library(readr)
library(naniar)
library(magrittr)
library(anytime)
library(lubridate)
library(readr)
library(here)
library(caret)
# Tobias
library(reticulate)

options(scipen = 999)


#----------------------- Data Import:
# To Do:

grand_listings <- read_csv(here("data","raw_data","listings.csv.gz"))

# Grand Listings clean. The .gz one. "w" stands for work.  
data <- grand_listings

# Save a list with variable names.
names <- as.list(names(data))  

# Sort variables to identify ratio>interval>ordinal>nominal to ease navigation
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]

#----------------- Missing. Goal: Keep as much info as possible. 
# To Do:
# Somehow the dates require different handling. Separate them. 
dates <- data[,45:49]
data <- data[, -c(45:49)]

# Remove different variations of NA. 
# Rerun code for different NA variations if encountered. 
for (name in names(data)){
  data[, c(name)] <- na_if(data[, c(name)], "N/A")
}

## Or for specific variables use the following code. 
# for (name in c("host_response_rate", "review_scores_rating", "last_review")){
#   data_s[, c(name)] <- na_if(data_s[, c(name)], "N/A")
# }


# Calculate the percentage of missings per variable. This is may indicator for dropping 
data <- bind_cols(data, dates) # Regroup dates into data
sort(colMeans(is.na.data.frame(data)))

#------------ Remove Variables 
# To Do: 

## Work mini dataset
# data_s <- data[1:300,]
# dates_s <- dates[1:300,]

# Host liststings count is the same
all.equal(data$host_listings_count, data$host_total_listings_count)
data[c("host_total_listings_count")] <- NULL

# bathrooms and calendar_updated and licence: virtually absent. Drop them 
data[c("bathrooms",
       "license",
       "calendar_updated")] <- NULL

# Neighbourhood variables: Dropt if neighbourhood dataset is more complete.
# nbh <- read_csv("data/neighbourhoods.csv")
# sort(colMeans(is.na.data.frame(nbh)))

# Missing rate under 1% in separate dataset. 
# Variables are not the same though. 
# nbh has a cleansed version. 
# nhb location could be used to rank kiezes somehow. Neural nets. 2much work.
# Host nbh can be replaced with host location. Drop them. 
data[c("neighbourhood",
       "neighborhood_overview",
       "host_neighbourhood")] <- NULL

# Trustfullness variables:
# higher trustfulness should lead to price increase but not so much. 
# Create a dataset with them in it. 
# Host_about, host_response_rate, host_acceptance_rate, host_response_time  NA rate > 0.5.
data[c("host_about",
       "host_response_rate",
       "host_acceptance_rate",
       "host_response_time")] <- NULL

## scrape_id is the same number for all observations. drop it. 
data$scrape_id <- NULL


## Notes about variables
# Review scores have a missing rate of 21% i.e. ~3800 obs. 
# Further, they seem relevant. Don't drop them yet.
# All missing rate are < 21.3%. 

# Test dataset without missings. # 12822 or 29.88% info loss. 
data <- na.omit(data)
sort(colMeans(is.na.data.frame(data)))
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]

#---------------------  Variables Re-coding:
# Consider overlap with caret preprocessing and the effects of `modelmatrix()` 
# To Do:
# - Language of name (EN vs DE).
# - review dates into seasonal interval. 
# - roomtype: dummys per class (lot of work consider if idoneous)
# - property type: dummys per class (lot of work consider if idoneous)
# - host location: binary,inland Ausland. 
# - host response rate: text to percentage
# - Host aceptance rate: text to percentage


## price: text to numerical
data$price <- as.numeric(gsub(",", "", substring(data$price, 2)))
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]


## Separate dataset between facts, text and dates (again)

time <- data[,42:46]
text <- data[,47:62]
mvars <- data[, -c(42:62)] # for metric_variables

# ## Export datasets.
# write.csv(x=mvars, file="data/1_mvars.csv")
# write.csv(x=text, file="data/2_text.csv")
# write.csv(x=time, file="data/3_time.csv")

# save(mvars, file="data/1_mvars.Rda")
# save(text, file="data/2_text.Rda")
# save(time, file="data/3_time.Rda")


#------------------------------------- Add images:
pd <- import("pandas")
det_bright_mean_df <- pd$read_pickle("det_bright_mean_df")
#pic_id == id

## Load dataset
# load(here("data", "clean_data", "1_mvars.Rda"))
# dataset_mod <- mvars

## ??
det_bright_mean_df <- subset(x = det_bright_mean_df, select = -c(pic_paths,detect_obj))

## transform logical values to binary 
mvars[(length(mvars)-4):length(mvars)] <- lapply(X = mvars[(length(mvars)-4):length(mvars)], FUN = as.numeric)
#View(mvars[(length(mvars)-4):length(mvars)])

## ??
mvars <- subset(x = mvars, select = -c(host_id))
# View(mvars)

##---------------- Merge Dataframe: 
metric_pic <- merge(mvars, det_bright_mean_df, by.x = "id", by.y = "pic_id")

# # leave out id etc

metric_pic <- metric_pic[,3:length(metric_pic)]


## leave out longitude and latitude
metric_pic <- subset(x = metric_pic, select = -c(latitude, longitude))
#View(mvars)


## Take only individual data from the first 10 pictures. 
# The rest of the data gives not enough information to train the model. 
# Training with the according columns only results in Nan
metric_pic <-
  metric_pic[, -c(
    which(colnames(metric_pic) == "Alarm_clock_pic_10"):which(colnames(metric_pic) == "Washing_machine_pic_17")
  )]


## ?
metric_pic <-
  metric_pic[, -c(which(colnames(metric_pic) == "brightness_pic_10"):which(colnames(metric_pic) == "brightness_pic_17"))]


## Make sure that there are no lists inside the df
for (i in 1:dim(metric_pic)[2]) {
  metric_pic[, i] <-  unlist(metric_pic[, i])
}

## check if every column is not a list
truth_list <- list()
for (i in 1:dim(metric_pic)[2]) {
  truth_list[i] <-  is.list(metric_pic[, i])
}
sum(unlist(truth_list)) == 0

## check if every value is numeric
truth_num <- list()
for (i in 1:dim(metric_pic)[2]) {
  truth_num[i] <-  which(is.numeric(metric_pic[, i]))
}
sum(unlist(truth_num)) == dim(metric_pic)[2]

## Create a data set with excluding dummies and including absolute frequencies variables. 
# Remove all dummy variables with "Bathroom_cabinet_pic_0" 
# being the first dummy and "Washing_machine_pic_9" the last. 
metric_pic_abs  <-
  metric_pic[, -c(
    which(colnames(metric_pic) == "Bathroom_cabinet_pic_0"):which(colnames(metric_pic) == "Washing_machine_pic_9")
  )]

## Remove "obj_sum"
metric_pic_abs  <-
  metric_pic_abs[, -c(which(colnames(metric_pic_abs) == "obj_sum"))]

## Create a data set including dummies and excluding absolute frequencies variables
metric_pic_dum  <-
  metric_pic[, -c(which(colnames(metric_pic) == "obj_sum"):which(colnames(metric_pic) == "Washing_machine_sum"))]
any(metric_pic_dum[, "brightness_mean"] == 0)
all(is.numeric(metric_pic_dum[, "brightness_mean"]))

## Visually inspect datasets
# View(metric_pic_abs[1:10,])
# View(metric_pic[1:10,])
# View(metric_pic_dum[1:10,])

#------------------------------------------ Partitioning

## Global Data Partitioning
# Description: Create an initial data partition to leave a final test set out, that's only going 
# to be used for the last fitting moment.

# Note: initially, the partition process  took place here. 
# Caret offers a more efficient way to streamline the process. 
# Thus, train/validation partition is relegated to every single model for consistency, 


#--------------- Full Data

# Proportion 90:10
rm(data, work, test, id)
set.seed (69)
id <- createDataPartition(metric_pic$price, p = .9, 
                          list = FALSE, 
                          times = 1)
work <- metric_pic[id,]
test <- metric_pic[-id,]
# Inspection
nrow(work) + nrow(test)  == nrow(metric_pic)

# Save full
save(work, file = here("data", "metric_pic.Rda"))
save(test, file = here("data", "test_data_USE_LAST_USE_ONCE", "metric_pic_test.Rda"))

#------------------------- Dummies
rm(data, work, test, id)

# Proportion 90:10
set.seed (69)
id <- createDataPartition(metric_pic_dum$price, p = .9, 
                          list = FALSE, 
                          times = 1)
work <- metric_pic_dum[id,]
test <- metric_pic_dum[-id,]

# Inspection
nrow(work) + nrow(test)  == nrow(metric_pic_dum)


# Save full
save(work, file = here("data", "metric_pic_dum.Rda"))
save(test, file = here("data", "test_data_USE_LAST_USE_ONCE", "metric_pic_dum_test.Rda"))

#------------------------- Absolute frequencies
rm(data, work, test, id)

# Proportion 90:10
set.seed (69)
id <- createDataPartition(metric_pic_abs$price, p = .9, 
                          list = FALSE, 
                          times = 1)
work <- metric_pic_abs[id,]
test <- metric_pic_abs[-id,]
# Inspection
nrow(work) + nrow(test)  == nrow(metric_pic_abs)


# Save absolute frequencies data
save(work,  file = here("data", "metric_pic_abs.Rda"))
save(test, file = here("data", "test_data_USE_LAST_USE_ONCE", "metric_pic_abs_test.Rda"))

