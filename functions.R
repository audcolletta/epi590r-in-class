new_mean_ <- functions(x) {
	n <- lenght(x)
	mean_val <- sum(x) / n
	return(mean_val)
}

x <- c(10, 15, 20, 25, 30)

# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2
square <- function(x) {
	squared <- x^2
	return(squared)
}
# test it out
square(x)
square(53)
53^2 # does this match?

square2 <- function(x) {
	squared <- x^2
	divided <- squared/2
	sum <- squared + divided
	return(sum)
	}

4^2
16/2
16+8

square2(4)

x <- 4
x*x
x^2
#what if we want a function to do this?
#create her yourself
x <- 3
square <- function()  {
	squared_value <- x*x
	return(squared_value)
}
square(x) #why is this not working

raise <- function() {

}
#use this fuction to create a proportion
#want user to muliply by multiplier
prop <- function(x, multiplier) {
	n <- length(x)
	prop_val <- sum(x) / n
	multiplied_val <- multiplier * prop_val
	return(multiplied_val)
}
prop(c(1, 1, 1, 0, 0), multiplier = 1)


x <- c(0, 1, 1)
multiplier <- 100
multiplier * sum(x) / length(x)
multiplier <- 1
prop <- function(x, multiplier) {
	n <- length(x)
	mean_val <- multiplier * sum(x) / n
	return(mean_val)
}
prop(x = c(1, 0, 1, 0), multiplier = 1)
#will get 0.5
prop(x = c(1, 0, 1, 0), multiplier = 100)

#set a default for the argument
prop <- function(x, multiplier = 1) {
	n <- length(x)
	mean_val <- multiplier * sum(x) / n
	return(mean_val)
}
#exercise
raise <- function(x, power, y) {
	x <- 2
	power <- 4
	answer <- x^power
	return(answer)
}

# test with
raise(x = 2, power = 4)
# should give you
2^4

raise2 <- function(x, power, y) {
	x <- 5
	power <- 2
	answer <- x^power
	return(answer)
}
# test
raise2(x = 5)
# should give you
5^2

#functions continueddddd
library(tidyverse)
library(gtsummary)
nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))
logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial()
)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat,
										 data = nlsy, family = poisson()
)
logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat,
												 data = nlsy, family = binomial(link = "log")
)

tbl_regression(
	poisson_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	)
)
#we want the user to give us the model
#replaced poisson with model
new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight"
		) ,tidy_fun = partial(tidy_robust, vcov = "HC1")
	)
}
#robust standard error addresses homoscedasicity maybe
#HC1 is just a specific calculation
new_table_function(logistic_model)
new_table_function(poisson_model)
new_table_function(logbinomial_model)
install.packages("renv")
renv::init()
