# load data and packages
library(tidyverse)
library(janitor)
library(naniar)

cps <- read_rds("data/cps_data.rds")

# data manipulation ------------------------------------------------------------
# by race
racial_groups <- c("student_count_black", "student_count_hispanic", "student_count_white", "student_count_asian", "student_count_native_american", "student_count_other_ethnicity","student_count_asian_pacific_islander", "student_count_multi", "student_count_hawaiian_pacific_islander", "student_count_ethnicity_not_available")

# by ela score
ela_score <- c("percent_did_not_meet_ela", "percent_partially_met_ela", "percent_approached_ela", "percent_met_ela", "percent_exceeded_ela", "percent_met_or_exceeded_ela")

# by math score
math_score <- c("percent_did_not_meet_math", "percent_partially_met_math", "percent_approached_math", "percent_met_math", "percent_exceeded_math", "percent_met_or_exceeded_math")

# manipulate data, add primary variables, calculate low income/title 1
cps_sorted <- cps |> 
  mutate(
    primary_race = apply(cps[, racial_groups], 1, function(row) {
      names(row)[which.max(row)]}),
    primary_ela = names(cps[, ela_score])[max.col(cps[, ela_score], "last")],
    primary_math = names(cps[, math_score])[max.col(cps[, math_score], "last")]
  ) |> 
  mutate(primary_race = sub("student_count_", "", primary_race),
         percent_low_income = round((student_count_low_income / student_count_total) * 100, 1),
         title_one = percent_low_income >= 40) |>
  # remove only school with partial missing observations for continuity
  filter(school_name != "U OF C - WOODLAWN HS") |> 
  # make factors to move graph levels around
  mutate(
    primary_race = factor(primary_race),
    primary_ela = factor(primary_ela),
    primary_math = factor(primary_math)
  )

# ela scores --------------------------------------------------------------

# graph to look at ELA scores by benchmark categories
cps_sorted |> 
  ggplot(aes(x = factor(primary_ela, levels = c("percent_did_not_meet_ela", "percent_partially_met_ela", "percent_approached_ela", "percent_met_or_exceeded_ela")))) + 
  geom_bar(fill = "#F7B4AD") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, position = position_stack(vjust = 0.5)) +
  labs(
    title = "Primary ELA Scores",
    x = "Primary ELA Score",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_x_discrete(labels = c("% Did Not Meet", "% Partially Met", "% Approached", "% Met or Exceeded")) +
  facet_wrap(~ year)

# math scores -------------------------------------------------------------

# graph to look at math scores by benchmark categories
cps_sorted |> 
  ggplot(aes(x = factor(primary_math, levels = c("percent_did_not_meet_math", "percent_partially_met_math", "percent_approached_math", "percent_met_or_exceeded_math")))) + 
  geom_bar(fill = "#B4CCE3") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, position = position_stack(vjust = 0.5)) +
  labs(
    title = "Primary Math Scores",
    x = "Primary Math Score",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  scale_x_discrete(labels = c("% Did Not Meet", "% Partially Met", "% Approached", "% Met or Exceeded")) +
  facet_wrap(~ year)

# met standards -----------------------------------------------------------
# pre and post pandemic scores for met standards (met and exceeded)
# looking at this since anyone past 'met' has reached the benchmark

# average out scores into a new dataset for cleanliness/graphability
pandemic_scores <- cps |> 
  group_by(year) |> 
  summarise(
    avg_met_exceeded_ela = mean(percent_met_or_exceeded_ela, na.rm = TRUE),
    avg_met_exceeded_math = mean(percent_met_or_exceeded_math, na.rm = TRUE),
  )

# ela scores for only students who met the standards
ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_met_exceeded_ela)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#F7B4AD") +
  labs(title = "Percentage of Students Meeting ELA Levels",
       x = "Year",
       y = "Average % Met ELA Levels") +
  theme_minimal()

# math scores for only students who met the standards
ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_met_exceeded_math)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#B4CCE3") +
  labs(title = "Percentage of Students Meeting Math Levels",
       x = "Year",
       y = "Average % Met Math Levels") +
  theme_minimal()