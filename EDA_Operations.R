# Load necessary libraries
library(dplyr)
library(lubridate)

# Read the dataset
df <- read.csv("./Dataset/Operations.csv")

# Drop the case_id column
df <- df %>% select(-case_id)

# Convert admission_time from minutes since 2070-01-01 to proper datetime
df$admission_time <- as.POSIXct(df$admission_time * 60, origin = "1970-01-01", tz = "UTC")

# Define a function to convert relative times to actual timestamps
convert_to_absolute_time <- function(reference_time, offset_minutes) {
  offset_minutes <- ifelse(offset_minutes == -1, NA, offset_minutes)  # Replace -1 with NA
  absolute_time <- reference_time + minutes(offset_minutes)
  return(absolute_time)
}

# Apply the function to relevant time columns
time_columns <- c("opstart_time", "opend_time", "anstart_time", "anend_time", 
                  "orin_time", "orout_time", "discharge_time")

for (col in time_columns) {
  df[[col]] <- convert_to_absolute_time(df$admission_time, df[[col]])
}

# Handle unrealistic values in opdate (should be 0 to 5 years max)
df$opdate <- ifelse(df$opdate < 0 | df$opdate > 1825, NA, df$opdate)

# Convert opdate to proper date format
df$opdate <- df$admission_time + days(df$opdate)

# Identify columns with more than 50% missing values and remove them
missing_threshold <- 0.5
missing_percent <- colSums(is.na(df)) / nrow(df)
columns_to_drop <- names(missing_percent[missing_percent > missing_threshold])

df <- df %>% select(-all_of(columns_to_drop))

# Handle remaining missing values, including anend_time
df <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ ifelse(is.na(.), Mode(.), .)))

# Specifically impute missing values in anend_time using median
df$anend_time <- ifelse(is.na(df$anend_time), median(df$anend_time, na.rm = TRUE), df$anend_time)
df$orin_time <- ifelse(is.na(df$orin_time), median(df$orin_time, na.rm = TRUE), df$orin_time)
df$orout_time <- ifelse(is.na(df$orout_time), median(df$orout_time, na.rm = TRUE), df$orout_time)
df$opstart_time <- ifelse(is.na(df$opstart_time), median(df$opstart_time, na.rm = TRUE), df$opstart_time)
df$opend_time <- ifelse(is.na(df$opend_time), median(df$opend_time, na.rm = TRUE), df$opend_time)
df$admission_time <- ifelse(is.na(df$admission_time), median(df$admission_time, na.rm = TRUE), df$admission_time)
df$discharge_time <- ifelse(is.na(df$discharge_time), median(df$discharge_time, na.rm = TRUE), df$discharge_time)
df$anstart_time <- ifelse(is.na(df$anstart_time), median(df$anstart_time, na.rm = TRUE), df$anstart_time)
df$anend_time <- ifelse(is.na(df$anend_time), median(df$anend_time, na.rm = TRUE), df$anend_time)

# Convert necessary time columns to POSIXct
time_columns <- c("admission_time", "discharge_time", "opstart_time", "opend_time", 
                  "anstart_time", "anend_time", "orin_time", "orout_time")

df[time_columns] <- lapply(df[time_columns], function(x) as.POSIXct(x, format="%Y-%m-%d %H:%M:%S", tz="UTC"))

# Remove outliers in weight (≤ 30kg or ≥ 250kg)
df$weight <- ifelse(df$weight < 30 | df$weight > 250, median(df$weight, na.rm = TRUE), df$weight)

# Remove outliers in height (≤ 100cm or ≥ 250cm)
df$height <- ifelse(df$height < 100 | df$height > 250, median(df$height, na.rm = TRUE), df$height)

# Save the final cleaned dataset
write.csv(df, "./Dataset/Operations_Clean.csv", row.names = FALSE)

print("Final dataset cleaned, missing values handled, and saved as Operations_Clean.csv")