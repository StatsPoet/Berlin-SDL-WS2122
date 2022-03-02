### Data Cleaning Berlin
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

# Data import and Cleaning 
#----------------------- 
# To Do:

grand_listings <- read_csv(here("data","berlin", "raw_data","listings.csv.gz"))

# Grand Listings clean. The .gz one.  
data <- grand_listings

# Save a list with variable names.
names <- as.list(names(data))  

# Sort variables to identify ratio>interval>ordinal>nominal to ease navigation
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]

#----------------- Missing. Goal: Keep as much info as possible. 
# To Do:
# Somehow the dates require different handling. Separate them. 
dates_id <- as.vector(which(sapply(data, is.Date)))
dates <- data[,dates_id]
data <- data[, -dates_id]

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
#View(sort(colMeans(is.na.data.frame(data))))
#------------ Remove Variables 
# To Do: 

## Work mini dataset
# data_s <- data[1:300,]
# dates_s <- dates[1:300,]

# Host listings count is the same
all.equal(data$host_listings_count, data$host_total_listings_count)
data[c("host_total_listings_count")] <- NULL

# bathrooms and calendar_updated and licence: virtually absent. Drop them 
data[c("bathrooms",
       "license",
       "calendar_updated"
)] <- NULL


# nbh has a cleansed version. 
# NA rate of other variables > 0.5
data[c("neighbourhood",
       "neighborhood_overview",
       "host_neighbourhood")] <- NULL

# Trustfulness variables:
# higher trustfulness should lead to price increase but not so much. 
# Create a data set with them in it. 
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
dates_id <- as.vector(which(sapply(data, is.Date)))
text_id <- as.vector(which(sapply(data, is.character)))

dates <- data[,dates_id]
text <- data[, text_id]
mvars <- data[, -c(dates_id, text_id)] # the metric variables. before M_vars

#Add images:
#------------------------------------- 
pd <- import("pandas") 
bilder <- pd$read_pickle(here("data", "berlin", "raw_data", "1_bilder_b"))
#pic_id == id

## Load dataset
# load(here("data", "clean_data", "1_mvars.Rda"))
# dataset_mod <- mvars

## delete pic_paths and detected objects
bilder <- subset(x = bilder, select = -c(pic_paths,detect_obj))

## transform logical values to binary 
mvars[(length(mvars)-4):length(mvars)] <- lapply(X = mvars[(length(mvars)-4):length(mvars)], FUN = as.numeric)
#View(mvars[(length(mvars)-4):length(mvars)])

## delete host_id
mvars <- subset(x = mvars, select = -c(host_id))
# View(mvars)

##---------------- Merge Dataframe: 
metric_pic <- merge(mvars, bilder, by.x = "id", by.y = "pic_id")


#change brightness_mean to brightness_mean without weights
#remove original brightness mean
metric_pic <- subset(x = metric_pic, select = -c(brightness_mean))
pd <- import("pandas") 

#add data off brightness without weights
bright_simple <- pd$read_pickle(here("data", "berlin", "raw_data", "brightness_simple"))
bright_simple_test <- subset(x = bright_simple, select = c(ids,brightness_mean))

metric_pic <- merge(metric_pic, bright_simple_test, by.x = "id", by.y = "ids")

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


## remove brightness for pictures 10 - 17
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

metric_pic_abs  <-
  metric_pic_abs[, -c(
    which(colnames(metric_pic_abs) == "brightness_pic_0"):which(colnames(metric_pic_abs) == "brightness_pic_9")
  )]

## Remove "obj_sum"
metric_pic_abs  <-
  metric_pic_abs[, -c(which(colnames(metric_pic_abs) == "obj_sum"))]

## Create a data set including dummies and excluding absolute frequencies variables
metric_pic_dum  <-
  metric_pic[, -c(which(colnames(metric_pic) == "obj_sum"):which(colnames(metric_pic) == "Washing_machine_sum"))]

metric_pic_dum <- subset(x = metric_pic_dum, select = -c(brightness_mean))

#any(metric_pic_dum[, "brightness_mean"] == 0)
#all(is.numeric(metric_pic_dum[, "brightness_mean"]))

## Visually inspect datasets
# View(metric_pic_abs[1:10,])
# View(metric_pic[1:10,])
# View(metric_pic_dum[1:10,])

# Add Temperature:
#------------

temperature <- pd$read_pickle(here("data", "berlin", "raw_data", "2_cct_b"))

#there are some values (707) which are very large. They need to be removed. 
temperature <- temperature[-c(which(temperature$cct_mean > 100000)), ]

## check if every column is not a list
truth_list <- list()
for (i in 1:dim(temperature)[2]) {
  truth_list[i] <-  is.list(temperature[, i])
}
sum(unlist(truth_list)) == 0

## check if every value is numeric
truth_num <- list()
for (i in 1:dim(temperature)[2]) {
  truth_num[i] <-  which(is.numeric(temperature[, i]))
}
sum(unlist(truth_num)) == dim(temperature)[2]

# make one df with only the mean
temp_mean <- temperature[c("pic_id", "cct_mean")]

# make one df with only the values for each picture
temp_pics <- subset(x = temperature, select = -c(cct_mean))

metric_pic_temp <- merge(metric_pic, temperature, by.x = "id", by.y = "pic_id")

metric_pic_dum_temp <- merge(metric_pic_dum, temp_pics, by.x = "id", by.y = "pic_id")

metric_pic_abs_temp <- merge(metric_pic_abs, temp_mean, by.x = "id", by.y = "pic_id")

# # leave out id etc

metric_pic_temp  <- metric_pic_temp[, -c(which(colnames(metric_pic_temp) == "id"))]
metric_pic_dum_temp  <- metric_pic_dum_temp[, -c(which(colnames(metric_pic_dum_temp) == "id"))]
metric_pic_abs_temp  <- metric_pic_abs_temp[, -c(which(colnames(metric_pic_abs_temp) == "id"))]

metric_pic  <- metric_pic[, -c(which(colnames(metric_pic) == "id"))]



#------------------------- Absolute frequencies
rm(data, train, test, id)

# Proportion 90:10
set.seed (69)
id <- createDataPartition(metric_pic_abs_temp$price, p = .9,
                          list = FALSE,
                          times = 1)
train <- metric_pic_abs_temp[id,]
test <- metric_pic_abs_temp[-id,]
# Inspection
nrow(train) + nrow(test)  == nrow(metric_pic_abs_temp)


# Save absolute frequencies data
save(metric_pic_abs_temp, train, test,  file = here("data","berlin",  "b_metric_pic_abs_temp_simple_bright.Rda"))
#save(test, file = here("data","berlin",  "test_data_USE_LAST_USE_ONCE", "metric_pic_abs_temp_test.Rda"))
