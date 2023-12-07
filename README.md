## The Effect of Race, Income, and The Pandemic on Chicago Public School's Learning: An EDA

This repository is for an analysis of data regarding the ELA and math scores in Chicago Public Schools. 

## In this repository:

### Directories
- `data/`: cleaned dataset, `cps_data.rds`, as well as the original datasets used to create the cleaned one in the `raw/` directory.
- `progress memos/`: contains Progress Memo 1 & 2, which were checkpoints on the way to completing the final report

### R Scripts
- `0_data_collection_cleaning.R`: contains the work done on joining the raw data together, as well as a brief overview of the data and quality checks.
- `1_ela_math_analysis.R`: contains the work done regarding an introductory look at the distrobution of ELA and math scores, as well as looking at the scores across time.
- `2_race_analysis.R`: contains the work done regarding the primary racial distribution of the dataset, and an analysis of ELA and math scores through the lens of race.
- `3_income_analysis.R`: contains the work done regarding assessing income levels of schools in the dataset, as well as an analysis of ELA and math scores through the lens of income. Also contains analysis of race, income, and the pandemic on scores.
- `4_misc_analysis.R`: contains supplemental information on the number of schools, the levels of the schools, and extra unused graphics.

### HTML / QMD Documents
- `executive_summary`: Provides a brief overview of the final report, describing the key takeaways from the report as well as important figures.
- `final_report`: An in depth EDA of the cleaned dataset, including an introduction, a data overview, the exploration, a conclusion, citations, and extra useful information.