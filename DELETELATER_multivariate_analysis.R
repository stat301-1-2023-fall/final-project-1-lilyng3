# load data and packages
library(tidyverse)
library(janitor)
library(naniar)

cps <- read_rds("data/cps_data.rds")

# ela barplot
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_ela, fill = primary_race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Students Meeting ELA Levels By Primary Race of School Over Time",
       x = "Year",
       y = "Average % Met ELA Levels") +
  theme_minimal() +
  facet_wrap(~ primary_race, 
             labeller = labeller(primary_race = c("black" = "Black", "white" = "White", "asian" = "Asian", "hispanic" = "Hispanic"))) +
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = "none")

# ela line graph
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_ela, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Students Meeting ELA Levels By Primary Race of School Over Time",
       x = "Year",
       y = "Average % Met ELA Levels",
       color = "Primary Race") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) + 
  theme(legend.position = "bottom")

# math barplot
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_math, fill = primary_race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Students Meeting Math Levels By Primary Race of School Over Time",
       x = "Year",
       y = "Average % Met Math Levels") +
  theme_minimal() +
  facet_wrap(~ primary_race, 
             labeller = labeller(primary_race = c("black" = "Black", "white" = "White", "asian" = "Asian", "hispanic" = "Hispanic"))) +
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = "none")

# math line graph
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_math, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Students Meeting Math Levels By Primary Race of School Over Time",
       x = "Year",
       y = "Average % Met Math Levels",
       color = "Primary Race") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) + 
  theme(legend.position = "bottom")


# low-income by race by score ------------------------------------------------------

pandemic_scores_race_lowincome <- cps_sorted |>
  filter(title_one == TRUE) |>
  group_by(year, primary_race) |>
  summarise(
    avg_met_exceeded_ela = mean(percent_met_or_exceeded_ela, na.rm = TRUE),
    avg_met_exceeded_math = mean(percent_met_or_exceeded_math, na.rm = TRUE)
  )

# low-income ela line graph
ggplot(pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_exceeded_ela, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Title 1 Students Meeting ELA Levels by Primary Race of School over Time",
       x = "Year",
       y = "Average % Met ELA Levels",
       color = "Primary Race") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) +
  theme(legend.position = "bottom")

# low-income math line graph
ggplot(pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_exceeded_math, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Title 1 Students Meeting Math Levels by Primary Race of School over Time",
       x = "Year",
       y = "Average % Met Math Levels",
       color = "Primary Race") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) +
  theme(legend.position = "bottom")

# comparison --------------------------------------------------------------
# ela
avg_ela <- ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_ela, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Students Meeting ELA Levels by Primary Race and Primary Income of School over Time",
       x = "Year",
       y = "Average % Met ELA Levels",
       color = "Primary Race\n(Title 1 = dashed)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) + 
  theme(legend.position = "bottom")

# layer average and low-income ela data
avg_ela + 
  geom_point(data = pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_exceeded_ela, color = primary_race), shape = 1) +
  geom_line(data = pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_exceeded_ela, group = primary_race), linetype = "dashed")

# math
avg_math <- ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_math, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Students Meeting Math Levels by Primary Race and Primary Income of School over Time",
       x = "Year",
       y = "Average % Met Math Levels",
       color = "Primary Race\n(Title 1 = dashed)") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) + 
  theme(legend.position = "bottom")

# layer average and low-income math data
avg_math + 
  geom_point(data = pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_exceeded_math, color = primary_race), shape = 1) +
  geom_line(data = pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_exceeded_math, group = primary_race), linetype = "dashed")

# unused graphics ------------------------------------------------------------

# scores by race by year
cps_sorted |> 
  group_by(year, primary_race) |> 
  summarise(count = n()) |> 
  gt::gt()

pandemic_scores_race <- cps_sorted |> 
  group_by(year, primary_race) |> 
  summarise(
    avg_met_exceeded_ela = mean(percent_met_or_exceeded_ela, na.rm = TRUE),
    avg_met_exceeded_math = mean(percent_met_or_exceeded_math, na.rm = TRUE)
  )

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

# ggplot(pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_ela, color = primary_race)) +
#   geom_point() +
#   geom_line(aes(group = primary_race)) +
#   labs(title = "Percentage of Students Meeting ELA Levels By Race Over Time",
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
#   labs(title = "Percentage of Students Meeting Math Levels By Race Over Time",
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
