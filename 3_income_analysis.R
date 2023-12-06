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
    title = "Title 1 Schools By Primary Race",
    x = "Primary Race",
    y = "Count",
    fill = "Primary Race"
  ) +
  scale_fill_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) +
  theme_minimal()

# low-income by race by score ------------------------------------------------------
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
# average ela scores with low-income ela scores
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

# average math scores with low-income math scores
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
