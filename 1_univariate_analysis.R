# load data and packages
library(tidyverse)
library(janitor)
library(naniar)

cps <- read_rds("data/cps_data.rds")

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


