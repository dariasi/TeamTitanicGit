create_boxplot_metric_dichotomous <- function(data, metric_var, dichotomous_var) {
  # Combine data into a single data frame
  combined_data <- data.frame(metric_var = metric_var, dichotomous_var = dichotomous_var)
  
  # Compute descriptive bivariate statistics
  analyze_metric_dichotomous(metric_var, dichotomous_var)
  
  # Subset data based on the dichotomous variable
  group0 <- combined_data[combined_data$dichotomous_var == 0, "metric_var"]
  group1 <- combined_data[combined_data$dichotomous_var == 1, "metric_var"]
  
  # Create box plot
  boxplot(group0, group1, names = c("Group 0", "Group 1"), xlab = dichotomous_var, ylab = metric_var, at = 1:2)
  
  
  # Add title
  title_text <- paste("Box Plot of", deparse(substitute(metric_var)), "by", deparse(substitute(dichotomous_var)))
  title(main = title_text) 
  # Add legend
  legend("topright", legend = c("NO", "Yes"), fill = c("red", "green"))
}
create_boxplot_metric_dichotomous(data, data$Age, data$Survived)

create_mosaic_plot(data, data$Survived,data$sex, data$Pclass)
Chi_square_test(data , data$Survived, data$Pclass)

# Function that combines data preparation and mosaic plot creation
analyze_and_visualize_categorical_vars <- function(data,  kvar1, kvar2, kvar3,kvar4) {
  # Prepare data for the mosaic plot
  cleaned_data <- prepare_mosaic_data(data, kvar1, kvar2, kvar3,kvar4)
  
  # Create the mosaic plot
  create_mosaic_plot(cleaned_data, categorical_vars)
}

