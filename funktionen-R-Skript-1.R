library(dplyr)
library(psych)
library(effsize)
library(vcd)

# 1 Descriptive statistics for metric variable
#' Calculates the average of a metric variable in a dataset.
#' @param data the dataset from which the average is calculated.
#' @param var_1 the name of the metric variable as a string, for which the average is calculated.
#' @examples
#' data_cleaned <- read.csv("data_cleaned.csv") 
#' Average(data_cleaned, "Age")
Average <- function(data,var_1) {
  average <- mean(data[[var_1]], na.rm = TRUE)
  print(paste("Average", var_1, ":", average))
}


# 2 Descriptive statistics for categorical variables
#' Calculates the rate (as a percentage) of ones in a categorical variable.
#' @param data the dataset from which the rate is calculated.
#' @param var_2 the name of the metric variable as a string, for which the rate is calculated.
#' @examples 
#' data_cleaned <- read.csv("data_cleaned.csv") 
#' Rate(data_cleaned, "Survived")
Rate <- function(data,var_2) {
  rate <- mean(data[[var_2]] == 1) * 100
  print(paste(var_2, "Rate", ":", rate))
}


# 3 Descriptive bivariate statistics for categorical variables
#' Performs a Chi-square test between two categorical variables to check their association.
#' @param data the dataset from which the rate is calculated.
#' @param var1, var2 the names of the two categorical variables as strings.
#' @examples 
#' data_cleaned <- read.csv("data_cleaned.csv") 
#' Chi_square_test(data_cleaned, "Pclass", "Survived")
Chi_square_test <- function(data, var1, var2) {

  cross_tab <- table(data[,var1], data[,var2])
  chi_square <- chisq.test(cross_tab)

  print("Сross_tab: ")
  print(cross_tab)

  print("Chi_square_test: ")
  print(chi_square)
}


#' Analyze relationship between a metric and a dichotomous variable
#'
#' This function performs statistical analyses to explore the relationship 
#' between a metric (continuous) variable and a dichotomous (binary) variable. 
#' It conducts a T-test to compare the means of the two groups defined by the 
#' dichotomous variable and a Mann-Whitney U test to compare their distributions.
#'
#' @param metric_col Numeric vector representing the metric variable.
#' @param dichotomous_col Numeric vector representing the dichotomous variable. 
#'
#' @examples
#' set.seed(123) 
#' metric_data <- rnorm(100, mean = 50, sd = 10)
#' dichotomous_data <- sample(0:1, 100, replace = TRUE)
#' analyze_metric_dichotomous(metric_data, dichotomous_data)
analyze_metric_dichotomous <- function(metric_col, dichotomous_col) {
  
  data <- data.frame(metric_col, dichotomous_col)
  
  # Subset data based on the dichotomous variable
  group1 <- data[data$dichotomous_col == 0, ]$metric_col
  group2 <- data[data$dichotomous_col == 1, ]$metric_col
  
  # Results(T_test and Mann_Whitney_U_Test added as interior functions)
  ttest_res <- T_test(group1, group2)
  print("T-Test:")
  print(ttest_res)
  mwu_test_res <- Mann_Whitney_U_test(group1, group2)
  print("Mann-Whitney U Test:")
  print(mwu_test_res)
}

# 5 Function to visualize three or four categorical variables
# Mosaicplot function
#Description:
#Produces mosaic plot for 3 or 4 categorical variables
#Parameters: data: data set containing categorical variables
# ...:names of categorical variables
#return:
# prints a mosaic plot for the categorical variables
create_mosaic_plot <- function(data, ...) {
  mosaic(formula(data,...), data = data)
  title(main = "Mosaic Plot of Categorical Variables")
}
# 6 Function to visualize three or four categorical variables after cleaning data
# Mosaicplot function
#Description:
#Produces mosaic plot for 3 or 4 categorical variables
#Parameters: data: data set containing categorical variables
# ...:names of categorical variables
#return:
# prints a mosaic plot for the categorical variables
create_clean_mosaic_plot <- function(data, ...) {
  clean_data <- prepare_mosaic_data(data, ...)
  mosaic(formula(clean_data,...), data = clean_data)
  title(main = "Mosaic Plot of Categorical Variables")
}
  
