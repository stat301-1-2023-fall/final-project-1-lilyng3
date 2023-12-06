# load data and packages
library(tidyverse)
library(janitor)
library(naniar)
library(gt)

cps <- read_rds("data/cps_data.rds")

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

# pie chart to display ratio of primary school race in dataset
cps_sorted |> 
  ggplot(aes(x = "", fill = primary_race)) +
  geom_bar(width = 1) +
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

# scores by race by year (table form)
cps_sorted |> 
  group_by(year, primary_race) |>
  summarise(count = n()) |>
  group_by(year) |>
  mutate(percentage = round(count / sum(count) * 100, 1)) |>
  mutate(primary_race = str_to_title(primary_race)) |>
  gt() |>
  tab_header(
    title = "Primary Race of School by Year",
  ) |>
  cols_label(
    year = "Year",
    primary_race = "Primary Race",
    count = "Count",
    percentage = "Percentage (%)"
  )

# ela and math by race ----------------------------------------------------

# average out scores outside of graph for neatness
pandemic_scores_race <- cps_sorted |> 
  group_by(year, primary_race) |> 
  summarise(
    avg_met_exceeded_ela = mean(percent_met_or_exceeded_ela, na.rm = TRUE),
    avg_met_exceeded_math = mean(percent_met_or_exceeded_math, na.rm = TRUE)
  )

# ela barplot faceted by race, can see scores by primary race of school
# benefit is seeing each racial group's data on its own
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_ela, fill = primary_race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Students Meeting ELA Levels by Primary Race of School by Year",
       x = "Year",
       y = "Average % Met ELA Levels") +
  theme_minimal() +
  facet_wrap(~ primary_race, 
             labeller = labeller(primary_race = c("black" = "Black", "white" = "White", "asian" = "Asian", "hispanic" = "Hispanic"))) +
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = "none")

# ela line graph faceted by race, can see scores by primary race of school
# benefit is comparing the racial groups together
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_ela, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Students Meeting ELA Levels by Primary Race of School by Year",
       x = "Year",
       y = "Average % Met ELA Levels",
       color = "Primary Race") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) + 
  theme(legend.position = "bottom")

# math barplot faceted by race, can see scores by primary race of school
# benefit is seeing each racial group's data on its own
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_math, fill = primary_race)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Students Meeting Math Levels by Primary Race of School by Year",
       x = "Year",
       y = "Average % Met Math Levels") +
  theme_minimal() +
  facet_wrap(~ primary_race, 
             labeller = labeller(primary_race = c("black" = "Black", "white" = "White", "asian" = "Asian", "hispanic" = "Hispanic"))) +
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = "none")

# math line graph faceted by race, can see scores by primary race of school
# benefit is comparing the racial groups together
ggplot(pandemic_scores_race, aes(x = as.factor(year), y = avg_met_exceeded_math, color = primary_race)) +
  geom_point() +
  geom_line(aes(group = primary_race)) +
  labs(title = "Percentage of Students Meeting Math Levels by Primary Race of School by Year",
       x = "Year",
       y = "Average % Met Math Levels",
       color = "Primary Race") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", labels = c("Asian", "Black", "Hispanic", "White")) + 
  theme(legend.position = "bottom")

# not using -------------------------------------------------------------------

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