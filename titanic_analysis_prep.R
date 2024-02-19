library(dplyr)
library(stringr)

# Read the Titanic dataset from a CSV file
data <- read.csv("titanic.csv")

# Extract person's titles from the name and create a new variable title
data <- data %>%
  mutate(
    Title = str_extract(Name, "Mr\\.|Mrs\\.|Miss\\.|Master\\.|Ms\\.|Mlle\\.|Mse\\."),
    Name = str_remove(Name, "Mr\\.|Mrs\\.|Miss\\.|Master\\.|Ms\\.|Mlle\\.|Mse\\.")
  )

# Substitute "Miss." or "Mlle." for "Ms."
data <- data %>%
  mutate(title = str_replace_all(Title, c("Miss\\." = "Ms.", "Mlle\\." = "Ms.")))

# Transform 'Survived', 'Sex', 'Embarked' columns into factors:
data$Survived <- factor(data$Survived)
data$Sex <- factor(data$Sex)
data$Embarked <- factor(data$Embarked)

# Transform a Pclass variable into an ordered factor:
data$Pclass <- factor(data$Pclass, levels = c(1, 2, 3), ordered = TRUE)