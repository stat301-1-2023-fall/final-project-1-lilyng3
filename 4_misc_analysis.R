# load data and packages
library(tidyverse)
library(janitor)
library(naniar)

cps <- read_rds("data/cps_data.rds")

# how many schools in dataset --------------------------------------------------------
num_unique_schools <- cps_sorted |> 
  distinct(school_name) |> 
  count()
# 500 schools

# level of school ----------------------------------------------------------
# "level" is elementary, middle, or high school
cps |> 
  mutate(primary_category = factor(primary_category, levels = c("ES", "MS", "HS"))) |> 
  ggplot(aes(x = primary_category)) +
  geom_bar(fill = "#CCEBC5") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, position = position_stack(vjust = 0.5)) +
  labs(
    title = "Distribution of Levels of Schools",
    x = "School Level",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("Elementary", "Middle School", "High School")) +
  facet_wrap(~ year)

# not using ---------------------------------------------------------------
# cps_sorted |>
#   ggplot(aes(x = primary_ela, fill = primary_ela)) +
#   geom_bar() +
#   geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
#   scale_fill_brewer(palette = "Set2") +
#   labs(
#     title = "Distribution of Primary ELA Scores",
#     x = "Primary ELA Score",
#     y = "Count"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   facet_wrap(~ year)
# +
#   scale_x_discrete(labels = c("Asian", "Black", "Hispanic", "White"))

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

# side by side bars, not as good visually as another solution
# ggplot(cps_race, aes(x = as.factor(year), y = percent_did_not_meet_math, fill = primary_race)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Percentage of Students Not Meeting Math Levels Over Years by Race",
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

# ggplot(pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_ela, color = primary_race)) +
#   geom_point() +
#   geom_line(aes(group = primary_race)) +
#   labs(title = "Percentage of Students Meeting ELA Levels by Race Over Time",
#        x = "Year",
#        y = "Average % Met ELA Levels",
#        color = "Primary Race") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Dark2", labels = c("Asian", "Black", "Hispanic", "White")) + 
#   theme(legend.position = "bottom")
# 
# # low-income math line graph
# ggplot(pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_math, color = primary_race)) +
#   geom_point() +
#   geom_line(aes(group = primary_race)) +
#   labs(title = "Percentage of Students Meeting Math Levels by Race Over Time",
#        x = "Year",
#        y = "Average % Met Math Levels",
#        color = "Primary Race") +
#   theme_minimal() +
#   scale_color_brewer(palette = "Dark2", labels = c("Asian", "Black", "Hispanic", "White")) + 
#   theme(legend.position = "bottom")

# focusing on meeting levels as a standard
# ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_did_not_meet_ela)) +
#   geom_bar(stat = "identity", position = "dodge", fill = "darkmagenta") +
#   labs(title = "Percentage of Students Not Meeting ELA Levels Over Years",
#        x = "Year",
#        y = "% Did Not Meet ELA Levels") +
#   theme_minimal()

# ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_did_not_meet_math)) +
#   geom_bar(stat = "identity", position = "dodge", fill = "darkmagenta") +
#   labs(title = "Percentage of Students Not Meeting Math Levels Over Years",
#        x = "Year",
#        y = "% Did Not Meet Math Levels") +
#   theme_minimal()

