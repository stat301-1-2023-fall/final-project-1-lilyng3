# load data and packages
library(tidyverse)
library(janitor)
library(naniar)

# for reading sheets
library(readxl)
library(writexl)

# original data -----------------------------------------------------------

ela_scores <- read_excel("data/raw/iar-parcc_2015to2023_schoollevel.xlsx", sheet = "IAR-PARCC ELA Results", skip = 1) |> 
  clean_names()

math_scores <- read_excel("data/raw/iar-parcc_2015to2023_schoollevel.xlsx", sheet = "IAR-PARCC Math Results", skip = 1) |> 
  clean_names()

sy1718 <- read.csv("data/raw/Chicago_Public_Schools_-_School_Profile_Information_SY1718.csv") |> 
  clean_names()

sy1819 <- read.csv("data/raw/Chicago_Public_Schools_-_School_Profile_Information_SY1819.csv") |> 
  clean_names()

sy2122 <- read.csv("data/raw/Chicago_Public_Schools_-_School_Profile_Information_SY2122.csv") |> 
  clean_names()

sy2223 <- read.csv("data/raw/Chicago_Public_Schools_-_School_Profile_Information_SY2223.csv") |> 
  clean_names()

# cleaned data ------------------------------------------------------------

keep <- c("school_id", "school_name", "year", "number_students_tested", "percent_did_not_meet", "percent_partially_met", "percent_approached_9", "percent_met", "percent_exceeded", "percent_met_or_exceeded_12")

ela_scores_clean <- ela_scores |> 
  filter(year == 2023 | year == 2022 |year == 2019 | year == 2018,
         test_name == "Combined ELA Grades 3-8") |> 
  select(keep) |> 
  rename(percent_approached = percent_approached_9,
         percent_met_or_exceeded = percent_met_or_exceeded_12)

math_scores_clean <- math_scores |> 
  filter(year == 2023 | year == 2022 |year == 2019 | year == 2018,
         test_name == "Combined Math Grades 3-8") |> 
  select(keep) |> 
  rename(percent_approached = percent_approached_9,
         percent_met_or_exceeded = percent_met_or_exceeded_12)

profile_keep <- c("school_id", "short_name", "long_name", "primary_category", "student_count_total", "student_count_low_income", "student_count_black", "student_count_hispanic", "student_count_white", "student_count_asian", "student_count_native_american", "student_count_other_ethnicity", "student_count_asian_pacific_islander", "student_count_multi", "student_count_hawaiian_pacific_islander", "student_count_ethnicity_not_available")

data_frames <- list(sy1718 = sy1718, sy1819 = sy1819, sy2122 = sy2122, sy2223 = sy2223)

for (name in names(data_frames)) {
  year_suffix <- substr(name, 3, 4)
  year <- as.integer(paste0("20", year_suffix)) + 1
  
  cleaned_data <- data_frames[[name]] |> 
    select(all_of(profile_keep)) |> 
    rename(school_name = short_name) |> 
    mutate(year = year,
           .after = school_name)
  
  new_var_name <- paste0(name, "_clean")
  
  assign(new_var_name, cleaned_data)
}

sy_clean <- rbind(sy1718_clean, sy1819_clean, sy2122_clean, sy2223_clean)

scores <- full_join(ela_scores_clean, math_scores_clean, by = c("school_id", "school_name", "year"), suffix = c("_ela", "_math"))

cps_data <- inner_join(sy_clean, scores, by = c("school_id", "school_name", "year"))

# save cleaned data -------------------------------------------------------
write.csv(cps_data, file = "data/cps_data.csv", row.names = FALSE)
