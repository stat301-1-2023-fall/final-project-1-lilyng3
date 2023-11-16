# load data and packages
library(tidyverse)
library(janitor)
library(naniar)

# original data -----------------------------------------------------------
# sy 17-18
sy1718_progress_report <- read.csv("data/Chicago_Public_Schools_-_School_Progress_Reports_SY1718.csv") |> 
  clean_names()

sy1718_school_profile <- read.csv("data/Chicago_Public_Schools_-_School_Profile_Information_SY1718.csv") |> 
  clean_names()

# sy 18-19
sy1819_progress_report <- read.csv("data/Chicago_Public_Schools_-_School_Progress_Reports_SY1819.csv") |> 
  clean_names()

sy1819_school_profile <- read.csv("data/Chicago_Public_Schools_-_School_Profile_Information_SY1819.csv") |> 
  clean_names()

# sy 21-22
sy2122_progress_report <- read.csv("data/Chicago_Public_Schools_-_School_Progress_Reports_SY2122.csv") |> 
  clean_names()

sy2122_school_profile <- read.csv("data/Chicago_Public_Schools_-_School_Profile_Information_SY2122.csv") |> 
  clean_names()

# sy 22-23
sy2223_progress_report <- read.csv("data/Chicago_Public_Schools_-_School_Progress_Reports_SY2223.csv") |> 
  clean_names()

sy2223_school_profile <- read.csv("data/Chicago_Public_Schools_-_School_Profile_Information_SY2223.csv") |> 
  clean_names()

# cleaned + joined data ------------------------------------------------------------
# sy 17-18 ----------------------------------------------------------------
# progress report
progress_report_remove_columns <- c("phone", "fax", "cps_school_profile", "website", "blue_ribbon_award_year", "excelerate_award_gold_year", "spot_light_award_year", "improvement_award_year", "excellence_award_year", "state_school_report_card_url", "other_metrics_year_1", "other_metrics_year_2")

sy1718_clean_progress_report <- sy1718_progress_report |> 
  select(-one_of(progress_report_remove_columns))

# school profile
school_profile_remove_columns <- c("legacy_unit_id", "finance_id", "summary", "administrator_title", "administrator", "secondary_contact_title", "secondary_contact", "phone", "fax", "cps_school_profile", "website", "facebook", "twitter", "youtube", "pinterest", "freshman_start_end_time", "after_school_hours", "earliest_drop_off_time", "transportation_bus", "transportation_el", "transportation_metra", "third_contact_title", "third_contact_name", "fourth_contact_title", "fourth_contact_name", "fifth_contact_title", "fifth_contact_name", "sixth_contact_title", "sixth_contact_name", "seventh_contact_title", "seventh_contact_name", "is_go_cps_elementary", "is_go_cps_participant", "is_go_cps_pre_k", "is_go_cps_high_school", "open_for_enrollment_date", "closed_for_enrollment_date")

sy1718_clean_school_profile <- sy1718_school_profile |> 
  select(-one_of(school_profile_remove_columns))

# joined data
sy1718_full <- full_join(sy1718_clean_school_profile, sy1718_clean_progress_report, by = c("school_id", "short_name", "long_name", "primary_category", "location"))


# sy 18-19 ----------------------------------------------------------------
# progress report
progress_report_remove_columns <- c("phone", "fax", "cps_school_profile", "website", "blue_ribbon_award_year", "excelerate_award_gold_year", "spot_light_award_year", "improvement_award_year", "excellence_award_year", "state_school_report_card_url", "other_metrics_year_1", "other_metrics_year_2")

sy1819_clean_progress_report <- sy1819_progress_report |> 
  select(-one_of(progress_report_remove_columns))

# school profile
school_profile_remove_columns <- c("legacy_unit_id", "finance_id", "summary", "administrator_title", "administrator", "secondary_contact_title", "secondary_contact", "phone", "fax", "cps_school_profile", "website", "facebook", "twitter", "youtube", "pinterest", "freshman_start_end_time", "after_school_hours", "earliest_drop_off_time", "transportation_bus", "transportation_el", "transportation_metra", "third_contact_title", "third_contact_name", "fourth_contact_title", "fourth_contact_name", "fifth_contact_title", "fifth_contact_name", "sixth_contact_title", "sixth_contact_name", "seventh_contact_title", "seventh_contact_name", "is_go_cps_elementary", "is_go_cps_participant", "is_go_cps_pre_k", "is_go_cps_high_school", "open_for_enrollment_date", "closed_for_enrollment_date")

sy1819_clean_school_profile <- sy1819_school_profile |> 
  select(-one_of(school_profile_remove_columns))

# joined data
sy1819_full <- full_join(sy1819_clean_progress_report, sy1819_clean_school_profile, by = c("school_id", "short_name", "long_name", "primary_category", "address", "city", "state", "zip"))

# sy 21-22 ----------------------------------------------------------------
# progress report
progress_report_remove_columns <- c("phone", "fax", "cps_school_profile", "website", "blue_ribbon_award_year", "excelerate_award_gold_year", "spot_light_award_year", "improvement_award_year", "excellence_award_year", "state_school_report_card_url", "other_metrics_year_1", "other_metrics_year_2")

sy2122_clean_progress_report <- sy2122_progress_report |> 
  select(-one_of(progress_report_remove_columns))

# school profile
school_profile_remove_columns <- c("legacy_unit_id", "finance_id", "summary", "administrator_title", "administrator", "secondary_contact_title", "secondary_contact", "phone", "fax", "cps_school_profile", "website", "facebook", "twitter", "youtube", "pinterest", "freshman_start_end_time", "after_school_hours", "earliest_drop_off_time", "transportation_bus", "transportation_el", "transportation_metra", "third_contact_title", "third_contact_name", "fourth_contact_title", "fourth_contact_name", "fifth_contact_title", "fifth_contact_name", "sixth_contact_title", "sixth_contact_name", "seventh_contact_title", "seventh_contact_name", "is_go_cps_elementary", "is_go_cps_participant", "is_go_cps_pre_k", "is_go_cps_high_school", "open_for_enrollment_date", "closed_for_enrollment_date")

sy2122_clean_school_profile <- sy2122_school_profile |> 
  select(-one_of(school_profile_remove_columns))

# joined data
sy2122_full <- full_join(sy2122_clean_progress_report, sy2122_clean_school_profile, by = c("school_id", "short_name", "long_name", "primary_category", "address", "city", "state", "zip"))

# sy 22-23 ----------------------------------------------------------------
progress_report_remove_columns <- c("phone", "fax", "cps_school_profile", "website", "blue_ribbon_award_year", "excelerate_award_gold_year", "spot_light_award_year", "improvement_award_year", "excellence_award_year", "state_school_report_card_url", "other_metrics_year_1", "other_metrics_year_2")

sy2223_clean_progress_report <- sy2223_progress_report |> 
  select(-one_of(progress_report_remove_columns))

# school profile
school_profile_remove_columns <- c("legacy_unit_id", "finance_id", "summary", "administrator_title", "administrator", "secondary_contact_title", "secondary_contact", "phone", "fax", "cps_school_profile", "website", "facebook", "twitter", "youtube", "pinterest", "freshman_start_end_time", "after_school_hours", "earliest_drop_off_time", "transportation_bus", "transportation_el", "transportation_metra", "third_contact_title", "third_contact_name", "fourth_contact_title", "fourth_contact_name", "fifth_contact_title", "fifth_contact_name", "sixth_contact_title", "sixth_contact_name", "seventh_contact_title", "seventh_contact_name", "is_go_cps_elementary", "is_go_cps_participant", "is_go_cps_pre_k", "is_go_cps_high_school", "open_for_enrollment_date", "closed_for_enrollment_date")

sy2223_clean_school_profile <- sy2223_school_profile |> 
  select(-one_of(school_profile_remove_columns))

# joined data
sy2223_full <- full_join(sy2223_clean_progress_report, sy2223_clean_school_profile, by = c("school_id", "short_name", "long_name", "primary_category", "address", "city", "state", "zip"))
