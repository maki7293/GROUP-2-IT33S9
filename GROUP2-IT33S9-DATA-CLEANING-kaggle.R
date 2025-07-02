# Load necessary packages
library(dplyr)

# Step 1: Load and inspect the raw dataset
students <- read.csv("Students_Raw_Data.csv")
str(students)                     
summary(students)                
sum(is.na(students))            

# Step 2: Remove unnecessary identifier column
students <- students %>% select(-Student_ID)

# Step 3: Standardize Gender labels and convert to factor
students$Gender <- tolower(trimws(students$Gender))
students$Gender[students$Gender == "male"] <- "Male"
students$Gender[students$Gender == "female"] <- "Female"
students$Gender <- as.factor(students$Gender)

# Step 4: Convert Yes/No responses to logical
students$Affects_Academic_Performance <- students$Affects_Academic_Performance == "Yes"

# Step 5: Clean and convert other categorical columns to factor
students$Academic_Level <- as.factor(trimws(students$Academic_Level))
students$Country <- as.factor(trimws(students$Country))
students$Most_Used_Platform <- as.factor(trimws(students$Most_Used_Platform))
students$Relationship_Status <- as.factor(trimws(students$Relationship_Status))

# Step 6: Remove rows with missing values
students <- na.omit(students)

# Step 7: Validate numerical ranges
summary(select(students, Avg_Daily_Usage_Hours, Sleep_Hours_Per_Night, Addicted_Score))

# Step 8: Save the cleaned dataset
write.csv(students, "Students_Cleaned_Data.csv", row.names = FALSE)
