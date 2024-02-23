library(dplyr)
library(psych)
library(effsize)

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