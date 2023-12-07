# load data and packages
library(tidyverse)
library(janitor)
library(naniar)

cps <- read_rds("data/cps_data.rds")

# low-income --------------------------------------------------------------
# distribution of low income students
cps_sorted |> 
  ggplot(aes(x = percent_low_income)) +
  geom_histogram(bins = 80, fill = "#E0CBE4") +
  theme_minimal() +
  labs(
    title = "Distribution of Low-Income Students",
    x = "% Low-Income Students Out of Whole Student Body",
    y = "Count"
  )

# looking at which schools quality as title 1 (40% free or reduced lunch or more) and examining their primary racial makeup
# low-income by race
cps_sorted |> 
  filter(title_one == TRUE) |> 
  ggplot(aes(x = primary_race, fill = primary_race)) +
  geom_bar() +
  labs(
    title = "Title 1 Schools by Primary Race",
    x = "Primary Race",
    y = "Count",
    fill = "Primary Race"
  ) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_discrete(labels = c("Asian", "Black", "Hispanic", "White")) +
  theme_minimal()

# met standards low income -----------------------------------------------------------
# pre and post pandemic scores for met standards (met and exceeded)
# looking at this since anyone past 'met' has reached the benchmark

# average out scores into a new dataset for cleanliness/graphability
pandemic_scores_lowincome <- cps_sorted |>
  filter(title_one == TRUE) |>
  group_by(year) |>
  summarise(
    avg_met_exceeded_ela = mean(percent_met_or_exceeded_ela, na.rm = TRUE),
    avg_met_exceeded_math = mean(percent_met_or_exceeded_math, na.rm = TRUE)
  )

# ela scores for only students who met the standards
# combine the data for avg and low-income
combined_data <-
  merge(pandemic_scores, pandemic_scores_lowincome, by = c("year")) |>
  pivot_longer(
    cols = c("avg_met_exceeded_ela.x", "avg_met_exceeded_ela.y"),
    names_to = "group_ela",
    values_to = "avg_met_exceeded_ela"
  ) |>
  pivot_longer(
    cols = c("avg_met_exceeded_math.x", "avg_met_exceeded_math.y"),
    names_to = "group_math",
    values_to = "avg_met_exceeded_math"
  )

# plot with combined data ela
ggplot(combined_data,
       aes(x = as.factor(year), y = avg_met_exceeded_ela, fill = group_ela)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Students Meeting ELA Levels by Income",
       x = "Year",
       y = "Average % Met ELA Levels") +
  scale_fill_manual(values = c("#F7B4AD", "#E0CBE4"), name = "Group", labels = c("Average", "Title 1")) +
  theme_minimal()

# math scores for only students who met the standards
# plot with combined data math
ggplot(combined_data,
       aes(x = as.factor(year), y = avg_met_exceeded_math, fill = group_math)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Students Meeting Math Levels by Income",
       x = "Year",
       y = "Average % Met Math Levels") +
  scale_fill_manual(values = c("#B4CCE3", "#E0CBE4"), name = "Group", labels = c("Average", "Title 1")) +
  theme_minimal()

# low-income by race by score ------------------------------------------------------
# put in additonal section
# average out scores outside of graph for neatness
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
  geom_line(aes(group = primary_race), linetype = "dashed") +
  labs(title = "Percentage of Title 1 Students Meeting ELA Levels by Primary Race of School",
       x = "Year",
       y = "Average % Met ELA Levels",
       color = "Primary Race") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) +
  theme(legend.position = "bottom") 

# low-income math line graph
ggplot(pandemic_scores_race_lowincome, aes(x = as.factor(year), y = avg_met_exceeded_math, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race), linetype = "dashed") +
  labs(title = "Percentage of Title 1 Students Meeting Math Levels by Primary Race of School",
       x = "Year",
       y = "Average % Met Math Levels",
       color = "Primary Race") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) +
  theme(legend.position = "bottom")

# comparison --------------------------------------------------------------
# average ela scores with low-income ela scores
avg_ela <- ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_ela, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Students Meeting ELA Levels by Primary Race and Income of School",
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

# average math scores with low-income math scores
avg_math <- ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_math, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Students Meeting Math Levels by Primary Race and Income of School",
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
