# Load necessary libraries
library(caret)
library(readxl)
library(dplyr)
library(car)
library(xgboost)
library(e1071)  # For SVM

# Define file paths
training <- "/Users/willberritt/Downloads/rebuild_project_2.xlsx"

# Read training data from Excel file
train_data <- read_excel(training)

# Remove rows with any NA values
train_data_remove_nulls <- na.omit(train_data)

# Create dummy variables
dummies <- model.matrix(~ `Brand Code` - 1, data = train_data_remove_nulls)

# Combine the dummy variables with the original dataframe
df <- cbind(train_data_remove_nulls, dummies)

# Assuming your dataframe is named df
df <- df[ , !(names(df) %in% c("Brand Code"))]

# Function to remove rows containing outliers based on IQR for a specific column
remove_outliers_specific <- function(df, column_name) {
  # Copy the dataframe to avoid modifying the original
  df_clean <- df
  
  # Calculate quartiles and IQR for the specified column
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR_val <- Q3 - Q1
  
  # Define the lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR_val
  upper_bound <- Q3 + 1.5 * IQR_val
  
  # Identify rows with outliers in the specified column
  outliers <- df[[column_name]] < lower_bound | df[[column_name]] > upper_bound
  
  # Subset the dataframe to keep only rows without outliers for the specified column
  df_clean <- df[!outliers, ]
  
  return(df_clean)
}

# Remove outliers specifically from the PH column
df_clean <- remove_outliers_specific(df, "PH")

# Compute correlation matrix a
cor_df_clean <- cor(df_clean)

# Replace values in correlation matrix with NA where absolute value is less than 0.9
cor_matrix_filtered <- cor_df_clean
cor_matrix_filtered[abs(cor_df_clean) < 0.8] <- NA

# Print the filtered correlation matrix
options(max.print = 10000)
print(cor_matrix_filtered, digits = 2)

# Remove collinearities
df_clean <- df_clean[, !colnames(df_clean) %in% "Carb Pressure"]
df_clean <- df_clean[, !colnames(df_clean) %in% "Hyd Pressure3"]
df_clean <- df_clean[, !colnames(df_clean) %in% "Balling Lvl"]
df_clean <- df_clean[, !colnames(df_clean) %in% "Carb Rel"]

# Split data into training and testing sets
set.seed(123)  # for reproducibility
trainIndex <- createDataPartition(df_clean$PH, p = 0.8, list = FALSE)
trainData <- df_clean[trainIndex, ]
testData <- df_clean[-trainIndex, ]

# Linear Regression
model_lm <- lm(PH ~ ., data = trainData)
predictions_lm <- predict(model_lm, newdata = testData)
rsq_lm <- cor(predictions_lm, testData$PH)^2

# XGBoost
dtrain <- xgb.DMatrix(as.matrix(trainData[, -which(names(trainData) == "PH")]), label = trainData$PH)
dtest <- xgb.DMatrix(as.matrix(testData[, -which(names(testData) == "PH")]))
model_xgb <- xgboost(data = dtrain, max.depth = 6, eta = 0.3, nrounds = 100, objective = "reg:squarederror")
predictions_xgb <- predict(model_xgb, dtest)
rsq_xgb <- cor(predictions_xgb, testData$PH)^2


# Support Vector Regression (SVR)
model_svr <- svm(PH ~ ., data = trainData)
predictions_svr <- predict(model_svr, newdata = testData)
rsq_svr <- cor(predictions_svr, testData$PH)^2

# Print R-squared values for comparison
cat("Linear Regression - R-squared:", rsq_lm, "\n")
cat("XGBoost - R-squared:", rsq_xgb, "\n")
cat("Support Vector Regression - R-squared:", rsq_svr, "\n")

# Predict using the XGBoost model on the test data
predictions_xgb <- predict(model_xgb, dtest)

# Create a plot comparing actual vs. predicted values
plot(testData$PH, predictions_xgb, 
     main = "Actual vs. Predicted PH (XGBoost)",
     xlab = "Actual PH",
     ylab = "Predicted PH",
     col = "blue",
     pch = 19,
     cex = 1.5)

# Add a diagonal line to show perfect predictions
abline(0, 1, col = "red")

# Add legend
legend("topleft", legend = "Predicted vs. Actual", col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), cex = 0.8)










# Load necessary libraries
library(readxl)  # For reading Excel files

# Define file path for new data
file_path <- "/Users/willberritt/Downloads/StudentEvaluation- TO PREDICT.xlsx"

# Read new data from Excel file
submission_df <- read_excel(file_path)
new_data <- read_excel(file_path)

# Determine the most common brand using base R
brand_counts <- table(new_data$`Brand Code`)
most_common_brand <- names(which.max(brand_counts))

# Replace null values in `Brand Code` with the most common brand
new_data$`Brand Code`[is.na(new_data$`Brand Code`)] <- most_common_brand

# Print the updated data frame
print(head(new_data))

# Calculate column means for numeric columns (excluding `PH` and `Brand Code`)
numeric_cols <- names(new_data)[sapply(new_data, is.numeric) & !(names(new_data) %in% c("PH", "Brand Code"))]
column_means <- colMeans(new_data[, numeric_cols], na.rm = TRUE)

# Replace NA values with column means
new_data$`Carb Volume`[is.na(new_data$`Carb Volume`)] <- column_means["Carb Volume"]
new_data$`Fill Ounces`[is.na(new_data$`Fill Ounces`)] <- column_means["Fill Ounces"]
new_data$`PC Volume`[is.na(new_data$`PC Volume`)] <- column_means["PC Volume"]
new_data$`Carb Pressure`[is.na(new_data$`Carb Pressure`)] <- column_means["Carb Pressure"]
new_data$`Carb Temp`[is.na(new_data$`Carb Temp`)] <- column_means["Carb Temp"]
new_data$PSC[is.na(new_data$PSC)] <- column_means["PSC"]
new_data$`PSC Fill`[is.na(new_data$`PSC Fill`)] <- column_means["PSC Fill"]
new_data$`PSC CO2`[is.na(new_data$`PSC CO2`)] <- column_means["PSC CO2"]
new_data$`Mnf Flow`[is.na(new_data$`Mnf Flow`)] <- column_means["Mnf Flow"]
new_data$`Carb Pressure1`[is.na(new_data$`Carb Pressure1`)] <- column_means["Carb Pressure1"]
new_data$`Fill Pressure`[is.na(new_data$`Fill Pressure`)] <- column_means["Fill Pressure"]
new_data$`Hyd Pressure1`[is.na(new_data$`Hyd Pressure1`)] <- column_means["Hyd Pressure1"]
new_data$`Hyd Pressure2`[is.na(new_data$`Hyd Pressure2`)] <- column_means["Hyd Pressure2"]
new_data$`Hyd Pressure3`[is.na(new_data$`Hyd Pressure3`)] <- column_means["Hyd Pressure3"]
new_data$`Hyd Pressure4`[is.na(new_data$`Hyd Pressure4`)] <- column_means["Hyd Pressure4"]
new_data$`Filler Level`[is.na(new_data$`Filler Level`)] <- column_means["Filler Level"]
new_data$`Filler Speed`[is.na(new_data$`Filler Speed`)] <- column_means["Filler Speed"]
new_data$`Temperature`[is.na(new_data$`Temperature`)] <- column_means["Temperature"]
new_data$`Usage cont`[is.na(new_data$`Usage cont`)] <- column_means["Usage cont"]
new_data$`Carb Flow`[is.na(new_data$`Carb Flow`)] <- column_means["Carb Flow"]
new_data$`Density`[is.na(new_data$`Density`)] <- column_means["Density"]
new_data$`MFR`[is.na(new_data$`MFR`)] <- column_means["MFR"]
new_data$`Balling`[is.na(new_data$`Balling`)] <- column_means["Balling"]
new_data$`Pressure Vacuum`[is.na(new_data$`Pressure Vacuum`)] <- column_means["Pressure Vacuum"]
new_data$`Oxygen Filler`[is.na(new_data$`Oxygen Filler`)] <- column_means["Oxygen Filler"]
new_data$`Bowl Setpoint`[is.na(new_data$`Bowl Setpoint`)] <- column_means["Bowl Setpoint"]
new_data$`Pressure Setpoint`[is.na(new_data$`Pressure Setpoint`)] <- column_means["Pressure Setpoint"]
new_data$`Air Pressurer`[is.na(new_data$`Air Pressurer`)] <- column_means["Air Pressurer"]
new_data$`Alch Rel`[is.na(new_data$`Alch Rel`)] <- column_means["Alch Rel"]
new_data$`Carb Rel`[is.na(new_data$`Carb Rel`)] <- column_means["Carb Rel"]
new_data$`Balling Lvl`[is.na(new_data$`Balling Lvl`)] <- column_means["Balling Lvl"]


# Remove collinearities
new_data <- new_data[, !colnames(new_data) %in% "Carb Pressure"]
new_data <- new_data[, !colnames(new_data) %in% "Hyd Pressure3"]
new_data <- new_data[, !colnames(new_data) %in% "Balling Lvl"]
new_data <- new_data[, !colnames(new_data) %in% "Carb Rel"]

# Create dummy variables
dummies <- model.matrix(~ `Brand Code` - 1, data = new_data)

# Combine the dummy variables with the original dataframe
df_test <- cbind(new_data, dummies)

# Assuming df_test is your data frame
df_test <- subset(df_test, select = -`Brand Code`)

# Convert df_test to xgb.DMatrix format
dtest <- xgb.DMatrix(as.matrix(df_test[, -which(names(df_test) == "PH")]))

# Predict using the XGBoost model
predictions_xgb <- predict(model_xgb, dtest)

# Replace PH column in df_test with predictions
submission_df$PH <- predictions_xgb

# Assuming submission_df is your data frame
library(openxlsx)

# Define file path for the Excel file
file_path <- "submission_df_proj_2_624.xlsx"

# Export submission_df to Excel
write.xlsx(submission_df, file_path, rowNames = FALSE)
