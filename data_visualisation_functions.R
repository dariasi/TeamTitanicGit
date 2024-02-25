library(ggplot2)

#1) Using the Average function to create a visualization of the difference in fares between the different Pclasses
# Creating the data for different classes
first_class_data <- data_cleaned[data_cleaned$Pclass == 1, ]
second_class_data <- data_cleaned[data_cleaned$Pclass == 2, ]
third_class_data <- data_cleaned[data_cleaned$Pclass == 3, ]

# Calculate average fare for each class
average_fare_class1 <- Average(first_class_data, "Fare")
average_fare_class2 <- Average(second_class_data, "Fare")
average_fare_class3 <- Average(third_class_data, "Fare")

# Create a data frame for plotting
average_fares <- data.frame(
  Pclass = factor(c(1, 2, 3)),
  AverageFare = c(average_fare_class1, average_fare_class2, average_fare_class3)
)

# Plot
ggplot(average_fares, aes(x = Pclass, y = AverageFare, fill = Pclass)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Fare by Passenger Class",
       x = "Passenger Class",
       y = "Average Fare")


#2) Using the Rate function to create a visualization of the difference in survival rates between the different Pclasses
# Calculate survival rates by class
survival_rates <- sapply(unique(data_cleaned$Pclass), function(pclass) {
  class_data <- data_cleaned[data_cleaned$Pclass == pclass,]
  Rate(class_data, "Survived")
})

# Create a data frame for plotting
survival_rates_df <- data.frame(
  Pclass = unique(data_cleaned$Pclass),
  SurvivalRate = survival_rates
)

ggplot(survival_rates_df, aes(x = Pclass, y = SurvivalRate, fill = as.factor(Pclass))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Pastel1") + 
  labs(title = "Survival Rates by Passenger Class",
       x = "Passenger Class",
       y = "Survival Rate (%)") +
  theme_minimal()


#3) Using the Chi_square_test function to create a visualization of the relationship between the variables "Embarked" and "Survived"
# Creating contingency table
cross_tab <- Chi_square_test(data_cleaned, "Embarked", "Survived")

# Visualize the contingency table with a mosaic plot
mosaicplot(cross_tab, main = "Mosaic Plot of Embarked vs Survived", color = TRUE)


#4,5) Using analyze_metric_dichotomous and create_mosaic_plot to compare variables
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


#6) Using the create_clean_mosaic_plot function to create a visualization of the relationship between the variables "Survived" and "Sex"
create_clean_mosaic_plot(data_cleaned, "Survived", "Sex")

