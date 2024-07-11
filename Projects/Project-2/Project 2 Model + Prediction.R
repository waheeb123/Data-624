# Load necessary libraries
library(caret)
library(readxl)

# Define file paths
training <- "/Users/willberritt/Downloads/rebuild_project_2.xlsx"
file_path <- "/Users/willberritt/Downloads/StudentEvaluation- TO PREDICT.xlsx"

# Read training data from Excel file
train_data <- read_excel(training)

# Remove rows with NA values from training data
train_data <- train_data[complete.cases(train_data), ]

# Create a linear regression model using all predictors except PH
lm_model <- lm(PH ~ ., data = train_data)

# Make predictions on the training data
predictions <- predict(lm_model, newdata = train_data)

# Calculate percent difference between predicted and actual PH values
percent_diff <- abs((predictions - train_data$PH) / train_data$PH) * 100

# Create a results dataframe with actual, predicted, and percent difference values
results_df <- data.frame(
  Correct_Value = train_data$PH,
  Predicted_Value = predictions,
  Percent_Difference = percent_diff
)

# Display summary statistics and the first few rows of the results dataframe
head(results_df)
mean(results_df$Percent_Difference)
summary(results_df$Percent_Difference)

# Read predict data from Excel file
df <- read_excel(file_path)

# Impute missing numerical values with mean of each column
df_imputed <- df
for (col in names(df)) {
  if (is.numeric(df_imputed[[col]])) {
    df_imputed[[col]][is.na(df_imputed[[col]])] <- mean(df_imputed[[col]], na.rm = TRUE)
  }
}

# Find the most common brand code from available data
most_common_brand <- df %>%
  filter(!is.na(`Brand Code`)) %>%
  count(`Brand Code`) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(`Brand Code`)

# Impute missing 'Brand Code' values with the most common brand code
df_imputed <- df_imputed %>%
  mutate(`Brand Code` = ifelse(is.na(`Brand Code`), most_common_brand, `Brand Code`))

# Update lm_model to include 'Brand Code' (new or imputed) as a predictor
lm_model <- update(lm_model, . ~ . + `Brand Code`, data = train_data)

# Make predictions using updated model and imputed data
predictions <- predict(lm_model, newdata = df_imputed)

# Store predictions in the PH column of df_imputed
df$PH <- predictions

# Print the dataframe with predicted PH values
print(df$PH)

# Load necessary package
library(openxlsx)

# Assuming df_imputed is your data frame
# Export df_imputed to an Excel file named "df_imputed.xlsx"
write.xlsx(df, file = "624_project_2_results.xlsx")

