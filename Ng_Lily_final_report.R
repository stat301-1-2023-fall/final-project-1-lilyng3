# load data and packages
library(tidyverse)
library(janitor)
library(naniar)

cps <- read_rds("data/cps_data.rds")


# data overview and quality -----------------------------------------------
length(select_if(cps,is.numeric))
length(select_if(cps,is.character))

gg_miss_upset(cps)

# Find rows with missing values
row_with_missing <- which(apply(cps, 1, function(row) any(is.na(row))))

# Print the row index with missing values
print(row_with_missing)


