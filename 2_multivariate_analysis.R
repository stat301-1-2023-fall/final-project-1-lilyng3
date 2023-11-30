# load data and packages
library(tidyverse)
library(janitor)
library(naniar)

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

# scores by race

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
