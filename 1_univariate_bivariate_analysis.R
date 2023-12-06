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
  filter(school_name != "U OF C - WOODLAWN HS") |> 
  mutate(
    primary_race = factor(primary_race),
    primary_ela = factor(primary_ela),
    primary_math = factor(primary_math)
  )

# pre and post pandemic scores for met standards (met and exceeded)
pandemic_scores <- cps |> 
  group_by(year) |> 
  summarise(
    avg_met_exceeded_ela = mean(percent_met_or_exceeded_ela, na.rm = TRUE),
    avg_met_exceeded_math = mean(percent_met_or_exceeded_math, na.rm = TRUE),
  )

# ela
ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_met_exceeded_ela)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#F7B4AD") +
  labs(title = "Percentage of Students Meeting ELA Levels Over Years",
       x = "Year",
       y = "Average % Met Math Levels") +
  theme_minimal()

ggplot(pandemic_scores, aes(x = as.factor(year), y = avg_met_exceeded_math)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#B4CCE3") +
  labs(title = "Percentage of Students Meeting Math Levels Over Years",
       x = "Year",
       y = "Average % Met Math Levels") +
  theme_minimal()

# how many schools --------------------------------------------------------
num_unique_schools <- cps_sorted |> 
  distinct(school_name) |> 
  count()
# 500 schools

# level of school ----------------------------------------------------------
# issue is this is across all data points, which is across four years. therefore, it will represent schools multiple times

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


# low-income --------------------------------------------------------------

cps_sorted |> 
  ggplot(aes(x = percent_low_income)) +
  geom_histogram(bins = 80, fill = "#E0CBE4") +
  theme_minimal() +
  labs(
    title = "Distribution of Low-Income Students",
    x = "% Low-Income Students Out of Whole Student Body",
    y = "Count"
  )

cps_sorted |> 
  filter(title_one == TRUE) |> 
  ggplot(aes(x = primary_race, fill = primary_race)) +
  geom_bar() +
  labs(
    title = "Title 1 Schools By Primary Race",
    x = "Primary Race",
    y = "Count",
    fill = "Primary Race"
  ) +
  scale_fill_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) +
  theme_minimal()

# primary race --------------------------------------------------------------------
# bar chart version
# cps_sorted |> 
#   ggplot(aes(x = primary_race, fill = primary_race)) + 
#   geom_bar() + 
#   geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
#   scale_fill_brewer(palette = "Set2") +
#   labs(
#     title = "Distribution of Primary Race",
#     x = "Primary Race",
#     y = "Count"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   scale_x_discrete(labels = c("Asian", "Black", "Hispanic", "White")) +
#   facet_wrap(~ year)

cps_sorted |> 
  ggplot(aes(x = "", fill = primary_race)) +
  geom_bar(width = 1) +
  geom_text(stat = 'count', 
            aes(label = ..count..), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) +
  labs(
    title = "Distribution of Primary Race",
    x = NULL,
    y = NULL,
    fill = "Primary Race"
  ) +
  facet_wrap(~ year) +
  coord_polar(theta = "y") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_blank(), 
        axis.text = element_blank(),   
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# ela scores --------------------------------------------------------------
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

cps_sorted |> 
  ggplot(aes(x = factor(primary_ela, levels = c("percent_did_not_meet_ela", "percent_partially_met_ela", "percent_approached_ela", "percent_met_or_exceeded_ela")))) + 
  geom_bar(fill = "#F7B4AD") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(
    title = "Distribution of Primary ELA Scores",
    x = "Primary ELA Score",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("% Did Not Meet", "% Partially Met", "% Approached", "% Met or Exceeded")) +
  facet_wrap(~ year)

# math scores -------------------------------------------------------------
cps_sorted |> 
  ggplot(aes(x = factor(primary_math, levels = c("percent_did_not_meet_math", "percent_partially_met_math", "percent_approached_math", "percent_met_or_exceeded_math")))) + 
  geom_bar(fill = "#B4CCE3") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(
    title = "Distribution of Primary Math Scores",
    x = "Primary Math Score",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("% Did Not Meet", "% Partially Met", "% Approached", "% Met or Exceeded")) +
  facet_wrap(~ year)

# extra -------------------------------------------------------------------
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