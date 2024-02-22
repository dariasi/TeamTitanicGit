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
  mutate(Title = str_replace_all(Title, c("Miss\\." = "Ms.", "Mlle\\." = "Ms.")))

# Transform 'Survived', 'Sex', 'Embarked' columns into factors:
data$Survived <- factor(data$Survived)
data$Sex <- factor(data$Sex)
data$Embarked <- factor(data$Embarked)

# Transform a Pclass variable into an ordered factor:
data$Pclass <- factor(data$Pclass, levels = c(1, 2, 3), ordered = TRUE)

#Imput missing values in the variable "Age"
age_medians <- tapply(data$Age, data$Title, median, na.rm = TRUE)
data$Age <- ifelse(is.na(data$Age), age_medians[data$Title], data$Age)

#Extract Cabin information
data$Side <- ifelse(data$Cabin == "", NA,
                    ifelse(as.numeric(gsub(" .*", "", gsub("\\D", "", data$Cabin))) %% 2 == 1, 
                           "Starboard", 
                           "Port"))
data$Deck <- gsub("([A-Za-z]+).*", "\\1", data$Cabin)
data$Deck[data$Deck == ""] <- NA

#Delete "Passenger ID", "Name", "Ticket" and "Cabin"
data$PassengerId <- NULL
data$Name <- NULL
data$Ticket <- NULL
data$Cabin <- NULL

#save csv data
write.csv(data, file = "data_cleaned.csv", row.names = FALSE)

