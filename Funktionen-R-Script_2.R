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
  cross_tab <- table(data[[var3]], data[[var4]])
  return(cross_tab)}

#Helper function for conducting chi square test
chi_square_tst <- function(table){
  chi_square_tst <- chisq.test(cross_tab)
  return(chi_square_tst)
}
#Helper function for mosaiqplot
prepare_mosaic_data <- function(data,  kvar1, kvar2, kvar3,kvar4) {
  subset_data <- data[, c( kvar1, kvar2, kvar3,kvar4)]
  clean_data <- na.omit(subset_data)
  return(clean_data)
}
