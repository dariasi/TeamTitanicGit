library(dplyr)
library(psych)
library(effsize)

# 1 Descriptive statistics for metric variables
# Average Function
# Description:
#   Calculates the average of a metric variable in a dataset.
# Parameters:
#   data: The dataset from which the average is calculated (data.frame).
#   var_1: The name of the metric variable as a string, for which the average is calculated.
# Returns:
#   Prints the average value of the specified variable.
Average <- function(data,var_1) {
  average <- mean(data[[var_1]], na.rm = TRUE)
  print(paste("Average", var_1, ":", average))
}


# 2 Descriptive statistics for categorical variables
# Rate Function
# Description:
#   Calculates the rate (as a percentage) of ones in a categorical variable.
# Parameters:
#   data: The dataset from which the rate is calculated (data.frame).
#   var_2: The name of the categorical variable as a string, for which the rate is calculated.
# Returns:
#   Prints the rate of ones in the specified variable.
Rate <- function(data,var_2) {
  rate <- mean(data[[var_2]] == 1) * 100
  print(paste(var_2, "Rate", ":", rate))
}


# 3 Descriptive bivariate statistics for categorical variables
# Chi_square_test Function
# Description:
#   Performs a Chi-square test between two categorical variables to check their association.
# Parameters:
#   data: The dataset containing the categorical variables (data.frame).
#   var3, var4: The names of the two categorical variables as strings.
# Returns:
#   Prints a cross-tabulation of the two variables and the result of the Chi-square test.
Chi_square_test <- function(data, var3, var4) {
  cross_tab <- table(data[[var3]], data[[var4]])
  chi_square_test <- chisq.test(cross_tab)
  
  print("cross_tab:")
  print(cross_tab)
  print("Chi_square_test:")
  print(chi_square_test)
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
  
  # T-test
  ttest_res <- t.test(group1, group2)
  
  # Mann-Whitney U Test
  mwu_test_res <- wilcox.test(group1, group2)
  
  # Results
  print("T-Test:")
  print(ttest_res)
  
  print("Mann-Whitney U Test:")
  print(mwu_test_res)
}