library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)
library(zoo)
library(forecast)
library(httr)

# URL of the Excel file
url <- "https://github.com/waheeb123/Data-624/raw/main/Projects/Data%20Set%20for%20Class.xls"

# Temporary file to download
temp_file <- tempfile(fileext = ".xls")

# Download the file
GET(url, write_disk(temp_file))

# Read the Excel file into a data frame
data <- read_excel(temp_file)

# Create subsets based on category
subset_S01 <- data %>%
  filter(category == "S01") %>%
  select(SeriesInd, category, Var01, Var02)

subset_S02 <- data %>%
  filter(category == "S02") %>%
  select(SeriesInd, category, Var02, Var03)

subset_S03 <- data %>%
  filter(category == "S03") %>%
  select(SeriesInd, category, Var05, Var07)

subset_S04 <- data %>%
  filter(category == "S04") %>%
  select(SeriesInd, category, Var01, Var02)

subset_S05 <- data %>%
  filter(category == "S05") %>%
  select(SeriesInd, category, Var02, Var03)

subset_S06 <- data %>%
  filter(category == "S06") %>%
  select(SeriesInd, category, Var05, Var07)

# Create plots for each subset
plot_S01_Var01 <- ggplot(subset_S01, aes(x = SeriesInd, y = Var01)) +
  geom_line() +
  labs(title = "Var01 over SeriesInd for Category S01", x = "SeriesInd", y = "Var01")

plot_S01_Var02 <- ggplot(subset_S01, aes(x = SeriesInd, y = Var02)) +
  geom_line() +
  labs(title = "Var02 over SeriesInd for Category S01", x = "SeriesInd", y = "Var02")

plot_S02_Var02 <- ggplot(subset_S02, aes(x = SeriesInd, y = Var02)) +
  geom_line() +
  labs(title = "Var02 over SeriesInd for Category S02", x = "SeriesInd", y = "Var02")

plot_S02_Var03 <- ggplot(subset_S02, aes(x = SeriesInd, y = Var03)) +
  geom_line() +
  labs(title = "Var03 over SeriesInd for Category S02", x = "SeriesInd", y = "Var03")

plot_S03_Var05 <- ggplot(subset_S03, aes(x = SeriesInd, y = Var05)) +
  geom_line() +
  labs(title = "Var05 over SeriesInd for Category S03", x = "SeriesInd", y = "Var05")

plot_S03_Var07 <- ggplot(subset_S03, aes(x = SeriesInd, y = Var07)) +
  geom_line() +
  labs(title = "Var07 over SeriesInd for Category S03", x = "SeriesInd", y = "Var07")

plot_S04_Var01 <- ggplot(subset_S04, aes(x = SeriesInd, y = Var01)) +
  geom_line() +
  labs(title = "Var01 over SeriesInd for Category S04", x = "SeriesInd", y = "Var01")

plot_S04_Var02 <- ggplot(subset_S04, aes(x = SeriesInd, y = Var02)) +
  geom_line() +
  labs(title = "Var02 over SeriesInd for Category S04", x = "SeriesInd", y = "Var02")

plot_S05_Var02 <- ggplot(subset_S05, aes(x = SeriesInd, y = Var02)) +
  geom_line() +
  labs(title = "Var02 over SeriesInd for Category S05", x = "SeriesInd", y = "Var02")

plot_S05_Var03 <- ggplot(subset_S05, aes(x = SeriesInd, y = Var03)) +
  geom_line() +
  labs(title = "Var03 over SeriesInd for Category S05", x = "SeriesInd", y = "Var03")

plot_S06_Var05 <- ggplot(subset_S06, aes(x = SeriesInd, y = Var05)) +
  geom_line() +
  labs(title = "Var05 over SeriesInd for Category S06", x = "SeriesInd", y = "Var05")

plot_S06_Var07 <- ggplot(subset_S06, aes(x = SeriesInd, y = Var07)) +
  geom_line() +
  labs(title = "Var07 over SeriesInd for Category S06", x = "SeriesInd", y = "Var07")

# Print each plot
print(plot_S01_Var01)
print(plot_S01_Var02)
print(plot_S02_Var02)
print(plot_S02_Var03)
print(plot_S03_Var05)
print(plot_S03_Var07)
print(plot_S04_Var01)
print(plot_S04_Var02)
print(plot_S05_Var02)
print(plot_S05_Var03)
print(plot_S06_Var05)
print(plot_S06_Var07)


# Filtering outliers for each subset
subset_S01_clean_Var01 <- subset_S01 %>%
  filter(Var01 >= quantile(Var01, 0.25, na.rm = TRUE) - 1.5 * IQR(Var01, na.rm = TRUE) & 
           Var01 <= quantile(Var01, 0.75, na.rm = TRUE) + 1.5 * IQR(Var01, na.rm = TRUE))

subset_S01_clean_Var02 <- subset_S01 %>%
  filter(Var02 >= quantile(Var02, 0.25, na.rm = TRUE) - 1.5 * IQR(Var02, na.rm = TRUE) & 
           Var02 <= quantile(Var02, 0.75, na.rm = TRUE) + 1.5 * IQR(Var02, na.rm = TRUE))

subset_S02_clean_Var02 <- subset_S02 %>%
  filter(Var02 >= quantile(Var02, 0.25, na.rm = TRUE) - 1.5 * IQR(Var02, na.rm = TRUE) & 
           Var02 <= quantile(Var02, 0.75, na.rm = TRUE) + 1.5 * IQR(Var02, na.rm = TRUE))

subset_S02_clean_Var03 <- subset_S02 %>%
  filter(Var03 >= quantile(Var03, 0.25, na.rm = TRUE) - 1.5 * IQR(Var03, na.rm = TRUE) & 
           Var03 <= quantile(Var03, 0.75, na.rm = TRUE) + 1.5 * IQR(Var03, na.rm = TRUE))

subset_S03_clean_Var05 <- subset_S03 %>%
  filter(Var05 >= quantile(Var05, 0.25, na.rm = TRUE) - 1.5 * IQR(Var05, na.rm = TRUE) & 
           Var05 <= quantile(Var05, 0.75, na.rm = TRUE) + 1.5 * IQR(Var05, na.rm = TRUE))

subset_S03_clean_Var07 <- subset_S03 %>%
  filter(Var07 >= quantile(Var07, 0.25, na.rm = TRUE) - 1.5 * IQR(Var07, na.rm = TRUE) & 
           Var07 <= quantile(Var07, 0.75, na.rm = TRUE) + 1.5 * IQR(Var07, na.rm = TRUE))

subset_S04_clean_Var01 <- subset_S04 %>%
  filter(Var01 >= quantile(Var01, 0.25, na.rm = TRUE) - 1.5 * IQR(Var01, na.rm = TRUE) & 
           Var01 <= quantile(Var01, 0.75, na.rm = TRUE) + 1.5 * IQR(Var01, na.rm = TRUE))

subset_S04_clean_Var02 <- subset_S04 %>%
  filter(Var02 >= quantile(Var02, 0.25, na.rm = TRUE) - 1.5 * IQR(Var02, na.rm = TRUE) & 
           Var02 <= quantile(Var02, 0.75, na.rm = TRUE) + 1.5 * IQR(Var02, na.rm = TRUE))

subset_S05_clean_Var02 <- subset_S05 %>%
  filter(Var02 >= quantile(Var02, 0.25, na.rm = TRUE) - 1.5 * IQR(Var02, na.rm = TRUE) & 
           Var02 <= quantile(Var02, 0.75, na.rm = TRUE) + 1.5 * IQR(Var02, na.rm = TRUE))

subset_S05_clean_Var03 <- subset_S05 %>%
  filter(Var03 >= quantile(Var03, 0.25, na.rm = TRUE) - 1.5 * IQR(Var03, na.rm = TRUE) & 
           Var03 <= quantile(Var03, 0.75, na.rm = TRUE) + 1.5 * IQR(Var03, na.rm = TRUE))

subset_S06_clean_Var05 <- subset_S06 %>%
  filter(Var05 >= quantile(Var05, 0.25, na.rm = TRUE) - 1.5 * IQR(Var05, na.rm = TRUE) & 
           Var05 <= quantile(Var05, 0.75, na.rm = TRUE) + 1.5 * IQR(Var05, na.rm = TRUE))

subset_S06_clean_Var07 <- subset_S06 %>%
  filter(Var07 >= quantile(Var07, 0.25, na.rm = TRUE) - 1.5 * IQR(Var07, na.rm = TRUE) & 
           Var07 <= quantile(Var07, 0.75, na.rm = TRUE) + 1.5 * IQR(Var07, na.rm = TRUE))


# Impute missing values using linear interpolation for subsets
subset_S06_clean_Var07$Var05 <- na.approx(subset_S06_clean_Var07$Var05)
subset_S06_clean_Var07$Var07 <- na.approx(subset_S06_clean_Var07$Var07)
subset_S05_clean_Var02$Var03 <- na.approx(subset_S05_clean_Var02$Var03)
subset_S05_clean_Var02$Var02 <- na.approx(subset_S05_clean_Var02$Var02)
subset_S04_clean_Var01$Var02 <- na.approx(subset_S04_clean_Var01$Var02)
subset_S04_clean_Var01$Var01 <- na.approx(subset_S04_clean_Var01$Var01)
subset_S03_clean_Var07$Var05 <- na.approx(subset_S03_clean_Var07$Var05)
subset_S03_clean_Var07$Var07 <- na.approx(subset_S03_clean_Var07$Var07)
subset_S02_clean_Var02$Var03 <- na.approx(subset_S02_clean_Var02$Var03)
subset_S02_clean_Var02$Var02 <- na.approx(subset_S02_clean_Var02$Var02)
subset_S01_clean_Var01$Var02 <- na.approx(subset_S01_clean_Var01$Var02)
subset_S01_clean_Var01$Var01 <- na.approx(subset_S01_clean_Var01$Var01)

# Find the last observation index for subsets
last_observation_index_S06 <- max(which(!is.na(subset_S06_clean_Var07$Var05)))
last_observation_index_S05 <- max(which(!is.na(subset_S05_clean_Var02$Var03)))
last_observation_index_S04 <- max(which(!is.na(subset_S04_clean_Var01$Var02)))
last_observation_index_S03 <- max(which(!is.na(subset_S03_clean_Var07$Var05)))
last_observation_index_S02 <- max(which(!is.na(subset_S02_clean_Var02$Var03)))
last_observation_index_S01 <- max(which(!is.na(subset_S01_clean_Var01$Var02)))

# Create time series objects for forecasting
ts_S06_Var05 <- ts(subset_S06_clean_Var07$Var05[1:last_observation_index_S06])
ts_S06_Var07 <- ts(subset_S06_clean_Var07$Var07[1:last_observation_index_S06])
ts_S05_Var02 <- ts(subset_S05_clean_Var02$Var02[1:last_observation_index_S05])
ts_S05_Var03 <- ts(subset_S05_clean_Var02$Var03[1:last_observation_index_S05])
ts_S04_Var01 <- ts(subset_S04_clean_Var01$Var01[1:last_observation_index_S04])
ts_S04_Var02 <- ts(subset_S04_clean_Var01$Var02[1:last_observation_index_S04])
ts_S03_Var05 <- ts(subset_S03_clean_Var07$Var05[1:last_observation_index_S03])
ts_S03_Var07 <- ts(subset_S03_clean_Var07$Var07[1:last_observation_index_S03])
ts_S02_Var02 <- ts(subset_S02_clean_Var02$Var02[1:last_observation_index_S02])
ts_S02_Var03 <- ts(subset_S02_clean_Var02$Var03[1:last_observation_index_S02])
ts_S01_Var01 <- ts(subset_S01_clean_Var01$Var01[1:last_observation_index_S01])
ts_S01_Var02 <- ts(subset_S01_clean_Var01$Var02[1:last_observation_index_S01])

# Forecast using auto.arima
forecast_S06_Var05 <- forecast(auto.arima(ts_S06_Var05), h = 140)
forecast_S06_Var07 <- forecast(auto.arima(ts_S06_Var07), h = 140)
forecast_S05_Var02 <- forecast(auto.arima(ts_S05_Var02), h = 140)
forecast_S05_Var03 <- forecast(auto.arima(ts_S05_Var03), h = 140)
forecast_S04_Var01 <- forecast(auto.arima(ts_S04_Var01), h = 140)
forecast_S04_Var02 <- forecast(auto.arima(ts_S04_Var02), h = 140)
forecast_S03_Var05 <- forecast(auto.arima(ts_S03_Var05), h = 140)
forecast_S03_Var07 <- forecast(auto.arima(ts_S03_Var07), h = 140)
forecast_S02_Var02 <- forecast(auto.arima(ts_S02_Var02), h = 140)
forecast_S02_Var03 <- forecast(auto.arima(ts_S02_Var03), h = 140)
forecast_S01_Var01 <- forecast(auto.arima(ts_S01_Var01), h = 140)
forecast_S01_Var02 <- forecast(auto.arima(ts_S01_Var02), h = 140)

# Create dataframe for forecasts
forecasts_df_S06 <- data.frame(
  SeriesInd = (subset_S06_clean_Var07$SeriesInd[last_observation_index_S06] + 1):(subset_S06_clean_Var07$SeriesInd[last_observation_index_S06] + 140),
  category = rep("S06", 140),
  Var05 = forecast_S06_Var05$mean,
  Var07 = forecast_S06_Var07$mean)
forecasts_df_S05 <- data.frame(
  SeriesInd = (subset_S05_clean_Var02$SeriesInd[last_observation_index_S05] + 1):(subset_S05_clean_Var02$SeriesInd[last_observation_index_S05] + 140),
  category = rep("S05", 140),
  Var02 = forecast_S05_Var02$mean,
  Var03 = forecast_S05_Var03$mean)
forecasts_df_S04 <- data.frame(
  SeriesInd = (subset_S04_clean_Var01$SeriesInd[last_observation_index_S04] + 1):(subset_S04_clean_Var01$SeriesInd[last_observation_index_S04] + 140),
  category = rep("S04", 140),
  Var01 = forecast_S04_Var01$mean,
  Var02 = forecast_S04_Var02$mean)
forecasts_df_S03 <- data.frame(
  SeriesInd = (subset_S03_clean_Var07$SeriesInd[last_observation_index_S03] + 1):(subset_S03_clean_Var07$SeriesInd[last_observation_index_S03] + 140),
  category = rep("S03", 140),
  Var05 = forecast_S03_Var05$mean,
  Var07 = forecast_S03_Var07$mean)
forecasts_df_S02 <- data.frame(
  SeriesInd = (subset_S02_clean_Var02$SeriesInd[last_observation_index_S02] + 1):(subset_S02_clean_Var02$SeriesInd[last_observation_index_S02] + 140),
  category = rep("S02", 140),
  Var02 = forecast_S02_Var02$mean,
  Var03 = forecast_S02_Var03$mean)
forecasts_df_S01 <- data.frame(
  SeriesInd = (subset_S01_clean_Var01$SeriesInd[last_observation_index_S01] + 1):(subset_S01_clean_Var01$SeriesInd[last_observation_index_S01] + 140),
  category = rep("S01", 140),
  Var01 = forecast_S01_Var01$mean,
  Var02 = forecast_S01_Var02$mean)

# Remove the last 140 rows from subset_S06 to append forecasts
n_rows_S06 <- nrow(subset_S06)
subset_S06 <- subset_S06[1:(n_rows_S06 - 140), ]
n_rows_S05 <- nrow(subset_S05)
subset_S05 <- subset_S05[1:(n_rows_S05 - 140), ]
n_rows_S04 <- nrow(subset_S04)
subset_S04 <- subset_S04[1:(n_rows_S04 - 140), ]
n_rows_S03 <- nrow(subset_S03)
subset_S03 <- subset_S03[1:(n_rows_S03 - 140), ]
n_rows_S02 <- nrow(subset_S02)
subset_S02 <- subset_S02[1:(n_rows_S02 - 140), ]
n_rows_S01 <- nrow(subset_S01)
subset_S01 <- subset_S01[1:(n_rows_S01 - 140), ]

# Combine original and forecasted data
combined_df_S06 <- rbind(subset_S06, forecasts_df_S06)
combined_df_S05 <- rbind(subset_S05, forecasts_df_S05)
combined_df_S04 <- rbind(subset_S04, forecasts_df_S04)
combined_df_S03 <- rbind(subset_S03, forecasts_df_S03)
combined_df_S02 <- rbind(subset_S02, forecasts_df_S02)
combined_df_S01 <- rbind(subset_S01, forecasts_df_S01)

# Add a label column to differentiate original and predicted data points
n_rows_combined_S06 <- nrow(combined_df_S06)
combined_df_S06$label <- "original"
combined_df_S06$label[(n_rows_combined_S06 - 139):n_rows_combined_S06] <- "predicted"
n_rows_combined_S05 <- nrow(combined_df_S05)
combined_df_S05$label <- "original"
combined_df_S05$label[(n_rows_combined_S05 - 139):n_rows_combined_S05] <- "predicted"
n_rows_combined_S04 <- nrow(combined_df_S04)
combined_df_S04$label <- "original"
combined_df_S04$label[(n_rows_combined_S04 - 139):n_rows_combined_S04] <- "predicted"
n_rows_combined_S03 <- nrow(combined_df_S03)
combined_df_S03$label <- "original"
combined_df_S03$label[(n_rows_combined_S03 - 139):n_rows_combined_S03] <- "predicted"
n_rows_combined_S02 <- nrow(combined_df_S02)
combined_df_S02$label <- "original"
combined_df_S02$label[(n_rows_combined_S02 - 139):n_rows_combined_S02] <- "predicted"
n_rows_combined_S01 <- nrow(combined_df_S01)
combined_df_S01$label <- "original"
combined_df_S01$label[(n_rows_combined_S01 - 139):n_rows_combined_S01] <- "predicted"

# Plot Var05 over SeriesInd for subset_S06
plot_SO6_var05 <- ggplot(combined_df_S06, aes(x = SeriesInd, y = Var05, color = label)) +
  geom_line() +
  labs(title = "Var05 over SeriesInd", x = "SeriesInd", y = "Var05") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_SO6_var07 <- ggplot(combined_df_S06, aes(x = SeriesInd, y = Var07, color = label)) +
  geom_line() +
  labs(title = "Var07 over SeriesInd", x = "SeriesInd", y = "Var07") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S05_Var02 <- ggplot(combined_df_S05, aes(x = SeriesInd, y = Var02, color = label)) +
  geom_line() +
  labs(title = "Var02 over SeriesInd - S05", x = "SeriesInd", y = "Var02") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S05_Var03 <- ggplot(combined_df_S05, aes(x = SeriesInd, y = Var03, color = label)) +
  geom_line() +
  labs(title = "Var03 over SeriesInd - S05", x = "SeriesInd", y = "Var03") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S04_Var01 <- ggplot(combined_df_S04, aes(x = SeriesInd, y = Var01, color = label)) +
  geom_line() +
  labs(title = "Var01 over SeriesInd - S04", x = "SeriesInd", y = "Var01") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S04_Var02 <- ggplot(combined_df_S04, aes(x = SeriesInd, y = Var02, color = label)) +
  geom_line() +
  labs(title = "Var02 over SeriesInd - S04", x = "SeriesInd", y = "Var02") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S03_Var05 <- ggplot(combined_df_S03, aes(x = SeriesInd, y = Var05, color = label)) +
  geom_line() +
  labs(title = "Var05 over SeriesInd - S03", x = "SeriesInd", y = "Var05") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S03_Var07 <- ggplot(combined_df_S03, aes(x = SeriesInd, y = Var07, color = label)) +
  geom_line() +
  labs(title = "Var07 over SeriesInd - S03", x = "SeriesInd", y = "Var07") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S02_Var02 <- ggplot(combined_df_S02, aes(x = SeriesInd, y = Var02, color = label)) +
  geom_line() +
  labs(title = "Var02 over SeriesInd - S02", x = "SeriesInd", y = "Var02") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S02_Var03 <- ggplot(combined_df_S02, aes(x = SeriesInd, y = Var03, color = label)) +
  geom_line() +
  labs(title = "Var03 over SeriesInd - S02", x = "SeriesInd", y = "Var03") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S01_Var01 <- ggplot(combined_df_S01, aes(x = SeriesInd, y = Var01, color = label)) +
  geom_line() +
  labs(title = "Var01 over SeriesInd - S01", x = "SeriesInd", y = "Var01") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()
plot_S01_Var02 <- ggplot(combined_df_S01, aes(x = SeriesInd, y = Var02, color = label)) +
  geom_line() +
  labs(title = "Var02 over SeriesInd - S01", x = "SeriesInd", y = "Var02") +
  scale_color_manual(values = c("original" = "blue", "predicted" = "red")) +
  theme_minimal()

# Print the plots
print(plot_SO6_var05)
print(plot_SO6_var07)
print(plot_S05_Var02)
print(plot_S05_Var03)
print(plot_S04_Var01)
print(plot_S04_Var02)
print(plot_S03_Var05)
print(plot_S03_Var07)
print(plot_S02_Var02)
print(plot_S02_Var03)
print(plot_S01_Var01)
print(plot_S01_Var02)
