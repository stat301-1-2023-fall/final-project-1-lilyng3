# load data and packages
library(tidyverse)
library(janitor)
library(naniar)
# for reading sheets
library(readxl)
library(writexl)

# original data -----------------------------------------------------------

# ela and math scores [excel file]
ela_scores <- read_excel("data/raw/iar-parcc_2015to2023_schoollevel.xlsx", sheet = "IAR-PARCC ELA Results", skip = 1) |> 
  clean_names()

math_scores <- read_excel("data/raw/iar-parcc_2015to2023_schoollevel.xlsx", sheet = "IAR-PARCC Math Results", skip = 1) |> 
  clean_names()

# school profile data [csv]
sy1718 <- read.csv("data/raw/Chicago_Public_Schools_-_School_Profile_Information_SY1718.csv") |> 
  clean_names()

sy1819 <- read.csv("data/raw/Chicago_Public_Schools_-_School_Profile_Information_SY1819.csv") |> 
  clean_names()

sy2122 <- read.csv("data/raw/Chicago_Public_Schools_-_School_Profile_Information_SY2122.csv") |> 
  clean_names()

sy2223 <- read.csv("data/raw/Chicago_Public_Schools_-_School_Profile_Information_SY2223.csv") |> 
  clean_names()

# cleaned data ------------------------------------------------------------

# variables to keep, only selecting the general testing results and not the subscores
keep <- c("school_id", "school_name", "year", "number_students_tested", "percent_did_not_meet", "percent_partially_met", "percent_approached_9", "percent_met", "percent_exceeded", "percent_met_or_exceeded_12")

# clean dataset 
# [years selected, cumulative averages across grades only, renaming variables from import issues]
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

# variables to keep, selecting ones that will help with joining as well as demographic information
profile_keep <- c("school_id", "short_name", "long_name", "primary_category", "student_count_total", "student_count_low_income", "student_count_black", "student_count_hispanic", "student_count_white", "student_count_asian", "student_count_native_american", "student_count_other_ethnicity", "student_count_asian_pacific_islander", "student_count_multi", "student_count_hawaiian_pacific_islander", "student_count_ethnicity_not_available")

# clean dataset 
# [using loop as there are four data sets]

# have to put name = name in order to assist with renaming the new datasets, something that was the biggest issue in this whole process. not exactly sure if i understand why this worked but i tried many solutions and this was the only adjustment that corrected my issue
# data sets i want to make clean
data <- list(sy1718 = sy1718, sy1819 = sy1819, sy2122 = sy2122, sy2223 = sy2223)

# selecting the part of the file name i want to keep, assigning a variable to create the year value to match the ela and math data
for (name in names(data)) {
  year_suffix <- substr(name, 3, 4)
  year <- as.integer(paste0("20", year_suffix)) + 1
  
  # keep vars, rename variable to match ela and math data, create the year variable and move it to the same position as the ela and math data for neatness
  cleaned_data <- data[[name]] |> 
    select(all_of(profile_keep)) |> 
    rename(school_name = short_name) |> 
    mutate(year = year,
           .after = school_name)
  
  # rename the new files
  new_var_name <- paste0(name, "_clean")
  
  # match the new name and data together
  assign(new_var_name, cleaned_data)
}

# stack the new clean datasets together into one
sy_clean <- rbind(sy1718_clean, sy1819_clean, sy2122_clean, sy2223_clean)

# full join the ela and math scores by overlapping variables, assigning a suffix so we know which variable is for which test
scores <- full_join(ela_scores_clean, math_scores_clean, by = c("school_id", "school_name", "year"), suffix = c("_ela", "_math"))

# create the full dataset! inner join the school profile data and the ela and math scores with an inner join to only keep schools that have test data (this testing occurs in 3rd - 8th grade, so high schools are cut out unless they offer 7th or 8th grade)
cps_data <- inner_join(sy_clean, scores, by = c("school_id", "school_name", "year"))

# save cleaned data -------------------------------------------------------
write_rds(cps_data, file = "data/cps_data.rds")
write_csv(cps_data, file = "data/cps_data.csv")

# data overview and quality -----------------------------------------------
length(select_if(cps,is.numeric))
length(select_if(cps,is.character))

gg_miss_upset(cps)

# Find rows with missing values
row_with_missing <- which(apply(cps, 1, function(row) any(is.na(row))))

# Print the row index with missing values
print(row_with_missing)

# codebook ----------------------------------------------------------------
codebook <- tibble(
  variable = c(
    "school_id",
    "school_name",
    "year",
    "long_name",
    "primary_category",
    "student_count_total",
    "student_count_low_income",
    "student_count_black",
    "student_count_hispanic",
    "student_count_white",
    "student_count_asian",
    "student_count_native_american",
    "student_count_other_ethnicity",
    "student_count_asian_pacific_islander",
    "student_count_multi",
    "student_count_hawaiian_pacific_islander",
    "student_count_ethnicity_not_available",
    "number_students_tested_ela",
    "percent_did_not_meet_ela",
    "percent_partially_met_ela",
    "percent_approached_ela",
    "percent_met_ela",
    "percent_exceeded_ela",
    "percent_met_or_exceeded_ela",
    "number_students_tested_math",
    "percent_did_not_meet_math",
    "percent_partially_met_math",
    "percent_approached_math",
    "percent_met_math",
    "percent_exceeded_math",
    "percent_met_or_exceeded_math"
  ),
  description = c(
    "unique identifier for each school",
    "shorthand name of school",
    "year",
    "full name of school",
    "level of school (elementary, middle, or high)",
    "number of students",
    "number of low income schools",
    "number of Black students",
    "number of Hispanic students",
    "number of White students",
    "number of Asian students",
    "number of Native American students",
    "number of other ethnicity students",
    "number of Asian Pacific Islander students",
    "number of students with multiple racial or ethnic categorizations",
    "number of Hawaiian Pacific Islander students",
    "number of students with no ethnicity available",
    "number of students tested for ELA",
    "percent of students that did not meet ELA standards",
    "percent of students that partially met ELA standards",
    "percent of students that approached ELA standards",
    "percent of students that met ELA standards",
    "percent of students that exceeded ELA standards",
    "percent of students that met or exceeded ELA standards",
    "number of students tested for math",
    "percent of students that did not meet math standards",
    "percent of students that partially met math standards",
    "percent of students that approached math standards",
    "percent of students that met math standards",
    "percent of students that exceeded math standards",
    "percent of students that met or exceeded math standards"
  )
)

write_csv(codebook, file = "data/cps_data_codebook.csv")
