# Load required libraries to performing the task
library(dplyr)
library(ggplot2)

# Set the working directory
setwd("C:/Users/User/Downloads")

# Step 1: Load and inspect the dataset
dataset <- read.csv("04_City_Expenditures.csv")
summary(dataset)
str(dataset)

# Step 2: Convert selected columns to factors
columns_to_factor <- c("Entity.Name", "County", "Field.Name", "Type", "Category", "Subcategory.1",
                       "Subcategory.2", "Line.Description", "City..State..Zip")
dataset[columns_to_factor] <- lapply(dataset[columns_to_factor], as.factor)

# Step 3: Remove missing values
#Inspect dataset to identify missing values for each columns
summary(dataset)
#Remove rows with missing values
dataset <- na.omit(dataset)
#Inspect dataset to confirm the missing values has been removed 
#and no remaining missing values in each columns. 
summary(dataset)

# Step 4: Identify duplicate rows 
sum(duplicated(dataset))

# Step 5: Handle invalid values
#We assume that the 'Value' column, which holds the expenditure values for the city, should be greater than 0. 
#Based on this assumption, we removed rows where the 'Value' is less than or equal to 0.
dataset <- dataset %>% filter(Value >= 0)

# Step 6: Remove unnecessary columns
dataset <- dataset %>% select(-County, -Field.Name, -Type, -Category, -Subcategory.1, 
                              -Subcategory.2, -Line.Description, -City..State..Zip, -Row.Number)

# Step 7: Aggregate data by City and Year
dataset_grouped <- dataset %>%
  group_by(Entity.Name, Fiscal.Year) %>%
  summarize(
    Total_Expenditure = sum(Value, na.rm = TRUE),
    Avg_Population = mean(Estimated.Population, na.rm = TRUE)
  )
#Inspect the summary of dataset_grouped after aggregate the data
summary(dataset_grouped)

# Step 8: Handle outliers for average population and total expenditure 
# We handle outliers after aggregating the data because
# we want to manage outliers in the context of the aggregated values.

# Filter the dataset to remove outliers based on Avg_Population and Total_Expenditure
dataset_grouped <- dataset_grouped %>%
  filter(
    # Remove values below the 1st percentile (lower bound) for Avg_Population
    Avg_Population > quantile(Avg_Population, 0.01),
    # Remove values above the 99th percentile (upper bound) for Avg_Population
    Avg_Population < quantile(Avg_Population, 0.99),
    # Remove values below the 1st percentile (lower bound) for Total_Expenditure
    Total_Expenditure > quantile(Total_Expenditure, 0.01),
    # Remove values above the 99th percentile (upper bound) for Total_Expenditure
    Total_Expenditure < quantile(Total_Expenditure, 0.99)
  )
#View summary of dataset after remove outliers
summary(dataset_grouped)

# Step 9:Visualize Total Expenditure vs. Population before performing log transformation
plot(dataset_grouped$Avg_Population, dataset_grouped$Total_Expenditure,
     xlab = "Average Population",
     ylab = "Total Expenditure",
     main = "Total Expenditure vs Average Population")

# Step 10: Log transformation to scale the data for Avg_Population and Total_Expenditure
dataset_grouped <- dataset_grouped %>%
  mutate(
    Log_Avg_Population = log(Avg_Population + 1),
    Log_Total_Expenditure = log(Total_Expenditure + 1)
  )

# Step 11: Visualize Total Expenditure vs. Population after performing log transformation
plot(dataset_grouped$Log_Avg_Population, dataset_grouped$Log_Total_Expenditure,
     xlab = "Log(Average Population)",
     ylab = "Log(Total Expenditure)",
     main = "Log-Transformed Total Expenditure vs Population")

# Step 12: Build the linear regression model
log_model <- lm(Log_Total_Expenditure ~ Log_Avg_Population, data = dataset_grouped)
summary(log_model)

# Step 13: Calculate correlation between Log_Avg_Population and Log_Total_Expenditure
correlation <- cor(dataset_grouped$Log_Total_Expenditure, dataset_grouped$Log_Avg_Population)

# Print the correlation
print(correlation)

# Step 14: Visualize the relationship between independent variable and dependent variable 
ggplot(dataset_grouped, aes(x = Log_Avg_Population, y = Log_Total_Expenditure)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Log-Transformed Total Expenditure vs Population",
    x = "Log(Average Population)",
    y = "Log(Total Expenditure)"
  )

# Step 15: Predictions using the model
predicted_values <- predict(log_model, newdata = data.frame(Log_Avg_Population = log(c(50000, 100000, 200000) + 1)))
print(exp(predicted_values) - 1)  # Convert back from log scale
