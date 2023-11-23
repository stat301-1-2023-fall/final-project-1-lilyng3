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

# eda ---------------------------------------------------------------------
# reading in the cleaned data
cps <- read_rds("data/cps_data.rds")

# find racial counts and percentages at each school
racial_count <- cps |>
  group_by(school_name) |>
  summarize(
    avg_black = round(mean(student_count_black), 1),
    avg_hispanic = round(mean(student_count_hispanic), 1),
    avg_white = round(mean(student_count_white), 1),
    avg_asian = round(mean(student_count_asian), 1),
    avg_native_american = round(mean(student_count_native_american), 1),
    avg_other_ethnicity = round(mean(student_count_other_ethnicity), 1),
    avg_asian_pacific_islander = round(mean(student_count_asian_pacific_islander), 1),
    avg_multiracial = round(mean(student_count_multi), 1),
    avg_hawaiian_pacific_islander = round(mean(student_count_hawaiian_pacific_islander), 1),
    avg_not_available = round(mean(student_count_ethnicity_not_available), 1),
    
    avg_black_pct = round(mean((student_count_black/student_count_total) * 100), 1),
    avg_hispanic_pct = round(mean((student_count_hispanic/student_count_total) * 100), 1),
    avg_white_pct = round(mean((student_count_white/student_count_total) * 100), 1),
    avg_asian_pct = round(mean((student_count_asian/student_count_total) * 100), 1),
    avg_native_american = round(mean((student_count_native_american/student_count_total) * 100), 1),
    avg_other_ethnicity = round(mean((student_count_other_ethnicity/student_count_total) * 100), 1),
    avg_asian_pacific_islander = round(mean((student_count_asian_pacific_islander/student_count_total) * 100), 1),
    avg_multiracial = round(mean((student_count_multi/student_count_total) * 100), 1),
    avg_hawaiian_pacific_islander = round(mean((student_count_hawaiian_pacific_islander/student_count_total) * 100), 1),
    avg_not_available = round(mean((student_count_ethnicity_not_available/student_count_total) * 100), 1)
  )

racial_count |> 
  DT::datatable()

# pre and post pandemic scores
pandemic_scores <- cps |> 
  group_by(year) |> 
  summarise(
    avg_met_ela = mean(percent_met_ela, na.rm = TRUE),
    avg_met_math = mean(percent_met_math, na.rm = TRUE),
    avg_did_not_meet_ela = mean(percent_did_not_meet_ela, na.rm = TRUE),
    avg_did_not_meet_math = mean(percent_did_not_meet_math, na.rm = TRUE)
  )

# ela
ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_met_ela)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#66c2a5") +
  labs(title = "Percentage of Students Meeting ELA Levels Over Years",
       x = "Year",
       y = "Average % Met Math Levels") +
  theme_minimal()

# focusing on meeting levels as a standard
# ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_did_not_meet_ela)) +
#   geom_bar(stat = "identity", position = "dodge", fill = "darkmagenta") +
#   labs(title = "Percentage of Students Not Meeting ELA Levels Over Years",
#        x = "Year",
#        y = "% Did Not Meet ELA Levels") +
#   theme_minimal()

ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_met_math)) +
geom_bar(stat = "identity", position = "dodge", fill = "#8da0cb") +
  labs(title = "Percentage of Students Meeting Math Levels Over Years",
       x = "Year",
       y = "Average % Met Math Levels") +
  theme_minimal()

# ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_did_not_meet_math)) +
#   geom_bar(stat = "identity", position = "dodge", fill = "darkmagenta") +
#   labs(title = "Percentage of Students Not Meeting Math Levels Over Years",
#        x = "Year",
#        y = "% Did Not Meet Math Levels") +
#   theme_minimal()


# by race

racial_groups <- c("student_count_black", "student_count_hispanic", "student_count_white", "student_count_asian", "student_count_native_american", "student_count_other_ethnicity","student_count_asian_pacific_islander", "student_count_multi", "student_count_hawaiian_pacific_islander", "student_count_ethnicity_not_available")

# i could not figure out how to get a loop that would compare each racial group to the next one and then pick out the highest value, so i copied a solution i found online
cps_race <- cps |> 
  mutate(
    primary_race = apply(cps[, racial_groups], 1, function(row) {
    names(row)[which.max(row)]})) |> 
  mutate(primary_race = sub("student_count_", "", primary_race))

cps_race |> 
  group_by(year, primary_race) |> 
  summarise(count = n()) |> 
  gt::gt()

pandemic_scores_race <- cps_race |> 
  group_by(year, primary_race) |> 
  summarise(
    avg_met_ela = mean(percent_met_ela, na.rm = TRUE),
    avg_met_math = mean(percent_met_math, na.rm = TRUE),
    avg_did_not_meet_ela = mean(percent_did_not_meet_ela, na.rm = TRUE),
    avg_did_not_meet_math = mean(percent_did_not_meet_math, na.rm = TRUE)
  )

# ela
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_ela, fill = primary_race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Students Meeting ELA Levels By Race Over Time",
       x = "Year",
       y = "Average % Met ELA Levels") +
  theme_minimal() +
  facet_wrap(~ primary_race, 
             labeller = labeller(primary_race = c("black" = "Black", "white" = "White", "asian" = "Asian", "hispanic" = "Hispanic"))) +
  scale_fill_brewer(palette = "Set2") + 
  theme(legend.position = "none")

# math
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_math, fill = primary_race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Students Meeting Math Levels By Race Over Time",
       x = "Year",
       y = "Average % Met Math Levels") +
  theme_minimal() +
  facet_wrap(~ primary_race, 
             labeller = labeller(primary_race = c("black" = "Black", "white" = "White", "asian" = "Asian", "hispanic" = "Hispanic"))) +
  scale_fill_brewer(palette = "Set2") + 
  theme(legend.position = "none")

# work section ------------------------------------------------------------

# table with racial demographic data
# cps |>
#   mutate(
#     black = student_count_black,
#     hispanic = student_count_hispanic,
#     white = student_count_white,
#     asian = student_count_asian,
#     native_american = student_count_native_american,
#     other_ethnicity = student_count_other_ethnicity,
#     asian_pacific_islander = student_count_asian_pacific_islander,
#     multiracial = student_count_multi,
#     hawaiian_pacific_islander = student_count_hawaiian_pacific_islander,
#     not_available = student_count_ethnicity_not_available
#   ) |>
#   select(black, hispanic, white, asian, native_american, other_ethnicity, asian_pacific_islander, multiracial, hawaiian_pacific_islander, not_available) |>
#   DT::datatable()

# racial_count <- cps |>
#   group_by(school_name) |>
#   summarize(
#     avg_black = mean(student_count_black),
#     avg_hispanic = mean(student_count_hispanic),
#     avg_white = mean(student_count_white),
#     avg_asian = mean(student_count_asian),
#     avg_native_american = mean(student_count_native_american),
#     avg_other_ethnicity = mean(student_count_other_ethnicity),
#     avg_asian_pacific_islander = mean(student_count_asian_pacific_islander),
#     avg_multiracial = mean(student_count_multi),
#     avg_hawaiian_pacific_islander = mean(student_count_hawaiian_pacific_islander),
#     avg_not_available = mean(student_count_ethnicity_not_available),
# 
#     avg_black_pct = mean((student_count_black/student_count_total) * 100),
#     avg_hispanic_pct = mean((student_count_hispanic/student_count_total) * 100),
#     avg_white_pct = mean((student_count_white/student_count_total) * 100),
#     avg_asian_pct = mean((student_count_asian/student_count_total) * 100),
#     avg_native_american = mean((student_count_native_american/student_count_total) * 100),
#     avg_other_ethnicity = mean((student_count_other_ethnicity/student_count_total) * 100),
#     avg_asian_pacific_islander = mean((student_count_asian_pacific_islander/student_count_total) * 100),
#     avg_multiracial = mean((student_count_multi/student_count_total) * 100),
#     avg_hawaiian_pacific_islander = mean((student_count_hawaiian_pacific_islander/student_count_total) * 100),
#     avg_not_available = mean((student_count_ethnicity_not_available/student_count_total) * 100)
#   )

# side by side bars, not as good visually as another solution
# ggplot(cps_race, aes(x = as.factor(year), y = percent_did_not_meet_math, fill = primary_race)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Percentage of Students Not Meeting Math Levels Over Years By Race",
#        x = "Year",
#        y = "% Did Not Meet Math Levels") +
#   theme_minimal()

# graph does not make sense, have to take avgs
# math
# ggplot(cps, aes(x = as.factor(year), y = percent_met_math)) +
#   geom_bar(stat = "identity", position = "dodge", fill = "midnightblue") +
#   labs(title = "Percentage of Students Meeting Math Levels Over Years",
#        x = "Year",
#        y = "% Met Math Levels") +
#   theme_minimal()
