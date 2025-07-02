# Load packages
install.packages(c("ggplot2", "dplyr", "readr", "corrplot"))
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)

# Load data
students <- read_csv("Students_Cleaned_Data.csv")

# Data summary
summary(students)

# Addiction by country
ggplot(students, aes(x = Country, y = Addicted_Score)) +
  geom_boxplot(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Addiction Score by Country", x = "Country", y = "Addiction Score") +
  coord_flip()

# Usage vs academic impact
ggplot(students, aes(x = Avg_Daily_Usage_Hours, fill = Affects_Academic_Performance)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  theme_minimal() +
  labs(title = "Usage vs Academic Performance", x = "Daily Usage (hrs)", fill = "Affects Academics")

# Addiction vs mental health
cor.test(students$Addicted_Score, students$Mental_Health_Score)

ggplot(students, aes(x = Addicted_Score, y = Mental_Health_Score)) +
  geom_point(color = "tomato") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Addiction vs Mental Health", x = "Addiction Score", y = "Mental Health Score")

# Sleep vs addiction
ggplot(students, aes(x = Sleep_Hours_Per_Night, y = Addicted_Score)) +
  geom_jitter(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Sleep vs Addiction", x = "Sleep Hours", y = "Addiction Score")

# Addiction vs conflicts
ggplot(students, aes(x = Addicted_Score, y = Conflicts_Over_Social_Media)) +
  geom_jitter(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Addiction vs Social Conflicts", x = "Addiction Score", y = "Conflicts")

# Gender differences
ggplot(students, aes(x = Gender, y = Addicted_Score, fill = Gender)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Addiction by Gender", x = "Gender", y = "Addiction Score")

# Platform vs addiction
ggplot(students, aes(x = Most_Used_Platform, y = Addicted_Score, fill = Most_Used_Platform)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Addiction by Platform", x = "Platform", y = "Addiction Score") +
  coord_flip()

# Correlation matrix
num_vars <- students %>%
  select(Avg_Daily_Usage_Hours, Addicted_Score, Sleep_Hours_Per_Night,
         Mental_Health_Score, Conflicts_Over_Social_Media)

corr_matrix <- cor(num_vars, use = "complete.obs")
corrplot(corr_matrix, method = "circle", type = "upper", tl.cex = 0.8)
