### Data Cleaning 
# Packages
library(dplyr)
library(readr)
# install.packages("naniar")
library(naniar)
options(scipen = 999)
library(magrittr)
library(anytime)
library(lubridate)
library(readr)
library(here)

## To Do:
# - Consider uniting all datasets in one by variable time i.e. metric, text, time. 
# - Text seems omnipresent. We need to use it. 
# - Crossvalidation: training and testing dataset. 

## Ideas for variable coding of chr into num:
# - Language of name (EN vs DE).
# - review dates into seasonal interval. 


# Import Dataset
grand_listings <- read_csv(here("data","raw_data","listings.csv.gz"))

# Grand Listings clean. The .gz one. "w" stands for work.  
data <- grand_listings

# Save a list with variable names.
names <- as.list(names(data))  

# Sort variables to identify ratio>interval>ordinal>nominal to ease navigation
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]

#### Missing. Goal: Keep as much info as possible. 
# Somehow the dates require different handling. Separate them. 
dates <- data[,45:49]
data <- data[, -c(45:49)]

# Remove different variations of NA. Rerun code for different NA variations if encountered. 
for (name in names(data)){
  data[, c(name)] <- na_if(data[, c(name)], "N/A")
}

## Or for specific variables use the following code. 
# for (name in c("host_response_rate", "review_scores_rating", "last_review")){
#   data_s[, c(name)] <- na_if(data_s[, c(name)], "N/A")
# }


# Calculate the percenntage of missings per variable. This is may indicator for dropping 
data <- bind_cols(data, dates) # Regroup dates into data
sort(colMeans(is.na.data.frame(data)))


## Work minidataset
# data_s <- data[1:300,]
# dates_s <- dates[1:300,]

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

## Repeated variables:

## scrape_id is the same number for all observations. drop it. 
data$scrape_id <- NULL


## Notes about variables
# Review scores have a missing rate of 21% i.e. ~3800 obs. Further, they seem relevant. Don't drop them yet.
# All missing rate are < 21.3%. 

# Test dataset without missings. # 12822 or 29.88% info loss. 
data <- na.omit(data)
sort(colMeans(is.na.data.frame(data)))
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]

## Variable recoding. 

# price: text to numerical
data$price <- as.numeric(gsub(",", "", substring(data$price, 2)))
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]

## host response rate: text to percentage
#data_comp$price <- as.numeric(gsub(",", "", substring(data_comp$price, 2)))
## Host aceptance rate: text to percentage

## To Do:
# roomtype: dummys per class (lot of work consider if idoneous)
# property type: dummys per class (lot of work consider if idoneous)
# host location: binary,inland Ausland. Pricing is affected by the market knowledge. Foreigners have lessofit

## Separate dataset between facts, text and dates (again)

time <- data[,43:47]
text <- data[,48:63]
mvars <- data[, -c(43:63)] # for model_variables

# ## Export datasets.
# write.csv(x=mvars, file="data/1_mvars.csv")
# write.csv(x=text, file="data/2_text.csv")
# write.csv(x=time, file="data/3_time.csv")

save(x=mvars, file="data/1_mvars.Rda")
save(x=text, file="data/2_text.Rda")
save(x=time, file="data/3_time.Rda")



# ## Normalization and centralization
# 
# 
# for (name in names(data)){
#   print(name)
#   curr <- as.matrix(data[,name])
#   print(class(curr))
#   mu <- mean(curr)
#   sigma <- sd(curr)
#   maxx <- max(curr)
#   minn <- min(curr)
#   
#   data[,name] <- (data[,name] - mu) / sigma # for standard score
#   #data[,name] <- (data[,name] - min) / (maxx-min) # For min-max feature scaling. 
#   
# }





