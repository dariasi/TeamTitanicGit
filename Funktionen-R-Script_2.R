library(dplyr)
library(psych)
library(effsize)


# Helper function for conducting t-test
T_test <- function(group1, group2) {
  ttest_res <- t.test(group1, group2)
  return(ttest_res)
}

# Helper function for conducting Mann-Whitney U test
Mann_Whitney_U_test <- function(group1, group2) {
  mwu_test_res <- wilcox.test(group1, group2)
  return(mwu_test_res)
}

#Helper function for conducting cross-tab
cross_tab <- function(data,var3,var4){
  cross_tab <- table(data[var3], data[var4])
  return(cross_tab)}

#Helper function for conducting chi square test
chi_square_tst <- function(table){
  chi_square_tst <- chisq.test(cross_tab)
  return(chi_square_tst)
}

#Helper function for converting input varible to correct formula for mosaic plotting
formula <- function (data,...){
  categorical_vars <- as.character(substitute(list(...))[-1])
  formula <- as.formula(paste("~", paste(categorical_vars, collapse = "+")))
  return(formula)
}

#Helper function for mosaiqplot
prepare_mosaic_data <- function(data, ...) {
  categorical_vars <- as.character(substitute(list(...)))[-1]
  subset_data <- data[, categorical_vars]
  clean_data <- na.omit(subset_data)
  return(clean_data)
}

