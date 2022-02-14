# File inorder to transform data from rda to a file that can be loaded into python. 
# The first approach is to use "feather"

library(here)
library(feather)


load(here("data", "munich", "initial_cleaned_data.Rda"))
picture_urls <- text["listing_url"]
picture_urls["id"] <- data["id"]

path <- here("data", "munich", "picture_urls.feather")

write_feather(picture_urls, path)
