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

# Import Dataset
grand_listings <- read_csv("data/listings.csv.gz")

# Grand Listings clean. The .gz one. "w" stands for work.  
data <- grand_listings

# Save a list with variable names. Too much of them to visualize. o stands for "original"
names <- as.list(names(data))  

# Sort variables to identify ratio>interval>ordinal>nominal to ease navigation
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]


#### Missings. Goal: Keep as much info as possible. 
# Somehow the dates require different handling. Separate them. 
dates <- data[,45:49]
data <- data[, -c(45:49)]

# Remove different variations of NA. #Rerun code for different NA variations if encountered. 
for (name in names(data)){
  data[, c(name)] <- na_if(data[, c(name)], "N/A")
}
## Or for specific variables. 
# for (name in c("host_response_rate", "review_scores_rating", "last_review")){
#   data_s[, c(name)] <- na_if(data_s[, c(name)], "N/A")
# }


# Calculate the percenntage of missings per variable. 
data <- bind_cols(data, dates) # Regroup dates into data
sort(colMeans(is.na.data.frame(data)))


# # Test dataset
# data_s <- data[1:300,]
# dates_s <- dates[1:300,]

# bathrooms and calendar_updated and licence are virtually absent. Drop them 
data[c("bathrooms","license", "calendar_updated")] <- NULL

# Consider droping neighbourhood variables, if neighbourhood dataset is more complete. 
# nbh <- read_csv("data/neighbourhoods.csv")
# sort(colMeans(is.na.data.frame(nbh)))

### Missing rate under 1% in separate dataset. 
### Variables are not the same though. 
### nbh has a cleansed version. 
### nhb location could be used to rank kiezes somehow. Neural nets. 2much work.
### Host nbh can be replaced with host location. Drop them. 
data[c("neighbourhood","neighborhood_overview", "host_neighbourhood")] <- NULL

##### Trustfullness variables:
## higher trustfulness should lead to price increase but not so much. 
## Create a dataset with them in it. 
## Host_about, host_response_rate, host_acceptance_rate, host_response_time  NA rate > 0.5.
data[c("host_about",
       "host_response_rate",
       "host_acceptance_rate",
       "host_response_time")] <- NULL


##### Notes about variables
# Review scores have a missing rate of 21% i.e. ~3800 obs. Further, they seem relevant. Don't drop them yet.
# All missing rate are < 21.3%. 

# Test dataset without missings. # 12822 or 29.88% info loss. 
data <- na.omit(data)
sort(colMeans(is.na.data.frame(data)))
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]

##### Variable recoding. 

## price: text to numerical
data$price <- as.numeric(gsub(",", "", substring(data$price, 2)))
data <- data[,names(sort(unlist(lapply(data, class)), decreasing = T))]

## host response rate: text to percentage
#data_comp$price <- as.numeric(gsub(",", "", substring(data_comp$price, 2)))
## Host aceptance rate: text to percentage

# roomtype: dummys per class (lot of work consider if idoneous)

# property type: dummys per class (lot of work consider if idoneous)

# host location: binary,inland Ausland. Pricing is affected by the market knowledge. Foreigners have lessofit

##### Separate dataset between facts, text and dates (again)

dates <- data[,44:45]
text <- data[,46:59]
data <- data[, -c(44:59)]


######## Numerical Variables:
##### Normalization


##### Centralization







## Consider correlation between variables
data$scrape_id <- NULL
C <- cor(data)
symnum(C, cutpoints = c(0.3, 0.7, 0.8, 0.9, 0.95), corr = T)

heatmap(C)


for (name in names(data)){
  print(name)
  curr <- as.matrix(data[,name])
  print(class(curr))
  mu <- mean(curr)
  sigma <- sd(curr)
  maxx <- max(curr)
  minn <- min(curr)
  
  data[,name] <- (data[,name] - mu) / sigma
  #data[,name] <- (data[,name] - min) / (maxx-min)
  
}
data$id
data[,"id"]

sapply(data, function(x) mean(x))