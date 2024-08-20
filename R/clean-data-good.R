library(tidyverse)

# column names
nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")

# read in raw data, replacing missing values with NA
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
				 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
				 skip = 1, col_names = nlsy_cols)

# create factors for categorical variables
nlsy_cats <- nlsy |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

# check to make sure coding is correct
count(nlsy_cats, eyesight, eyesight_cat)

# remove observations with any missing data
nlsy_cc <- na.omit(nlsy_cats)

# check to make sure it worked
count(nlsy_cc, eyesight_cat)

# create data/clean folder if it doesn't already exist
if (!dir.exists(here::here("data", "clean"))) {
	dir.create(here::here("data", "clean"))
}

# save the complete-case data
write_rds(nlsy_cc, here::here("data", "clean", "nlsy-complete-cases.rds"))

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(race_eth_cat, region_cat, income,
							starts_with("sleep")),
	statistic = list(
		income ~ "p10 = {p10}, p90 ={p90}",
		starts_with("sleep") ~ "max = {min}, min ={max}"),
	digits = list(
		income~ c(3,3),
		starts_with("sleep") ~ c(1,1)
	)
) |>
	modify_table_styling(
		columns = label,
		rows = label == "race_eth_cat",
		footnote = "https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
	) |>

