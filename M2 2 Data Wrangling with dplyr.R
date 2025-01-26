library(tidyverse)
library(stringr)  # For string manipulation
library(readr)    # For reading CSV files (read_csv())
library(dplyr)

setwd("C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project")

bike_sharing_df <- read_csv("raw_seoul_bike_sharing.csv")
head(bike_sharing_df)

summary(bike_sharing_df)
dim(bike_sharing_df)
map(bike_sharing_df, ~sum(is.na(.))) #RENTED_BIKE_COUNT HAS 295 NAs

####TASK: Detect and handle missing values

# Drop rows with `RENTED_BIKE_COUNT` column == NA
bike_sharing_df <- bike_sharing_df %>% drop_na(RENTED_BIKE_COUNT)
# Print the dataset dimension again after those rows are dropped
dim(bike_sharing_df)

#TEMPERATURE
bike_sharing_df %>% 
  filter(is.na(TEMPERATURE))
#RESULT: NAs related to SUMMER

# Calculate the summer average temperature
summer_temp <- bike_sharing_df[bike_sharing_df$SEASONS == "Summer", ] #[rows with summer, empty cuz all columns are in]
summer_avg_temp <- mean(summer_temp$TEMPERATURE, na.rm=TRUE)
print(summer_avg_temp)

# Impute missing values for TEMPERATURE column with summer average temperature
bike_sharing_df <- bike_sharing_df %>%
  mutate(TEMPERATURE = replace_na(TEMPERATURE, summer_avg_temp))

# Print the summary of the dataset again to make sure no missing values in all columns
summary(bike_sharing_df)

# Save the dataset as `seoul_bike_sharing.csv`
write.csv(bike_sharing_df, "C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project/seoul_bike_sharing.csv", row.names = FALSE)


####TASK: Create indicator (dummy) variables for categorical variables

#HOUR
bike_sharing_df <-bike_sharing_df %>%
  mutate(HOUR = as.character(HOUR))
class(bike_sharing_df$HOUR)

#SEASONS
bike_sharing_df$SEASONS <- factor(bike_sharing_df$SEASONS)
#Create dummy columns for each season using `mutate()` and `spread()`
bike_sharing_df <- bike_sharing_df %>%
  mutate(dummy = 1) %>%  # Create a dummy column with 1
  spread(
    key = SEASONS,   # Spread the SEASONS values into separate columns
    value = dummy,   # Use the dummy column for spreading
    fill = 0         # If a season is missing, fill with 0
  )


#HOLIDAY
bike_sharing_df$HOLIDAY <- factor(bike_sharing_df$HOLIDAY)
bike_sharing_df <- bike_sharing_df %>%
  mutate(dummy = 1) %>%  # Create a dummy column with 1
  spread(
    key = HOLIDAY,   # Spread the SEASONS values into separate columns
    value = dummy,   # Use the dummy column for spreading
    fill = 0         # If a season is missing, fill with 0
  )

#FUNCTIONING_DAY
summary(bike_sharing_df$FUNCTIONING_DAY)
bike_sharing_df$FUNCTIONING_DAY <- factor(bike_sharing_df$FUNCTIONING_DAY)

# Print the dataset summary again to make sure the indicator columns are created properly
summary(bike_sharing_df)

# Save the dataset as `seoul_bike_sharing_converted.csv`
write.csv(bike_sharing_df, "C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project/seoul_bike_sharing_converted.csv", row.names = FALSE)


####TASK: Normalize data

# Apply min-max normalization for each specified column
SCALED_bike_sharing_df <- bike_sharing_df %>%
  mutate(
    RENTED_BIKE_COUNT = (RENTED_BIKE_COUNT - min(RENTED_BIKE_COUNT)) / (max(RENTED_BIKE_COUNT) - min(RENTED_BIKE_COUNT)),
    TEMPERATURE = (TEMPERATURE - min(TEMPERATURE)) / (max(TEMPERATURE) - min(TEMPERATURE)),
    HUMIDITY = (HUMIDITY - min(HUMIDITY)) / (max(HUMIDITY) - min(HUMIDITY)),
    WIND_SPEED = (WIND_SPEED - min(WIND_SPEED)) / (max(WIND_SPEED) - min(WIND_SPEED)),
    VISIBILITY = (VISIBILITY - min(VISIBILITY)) / (max(VISIBILITY) - min(VISIBILITY)),
    DEW_POINT_TEMPERATURE = (DEW_POINT_TEMPERATURE - min(DEW_POINT_TEMPERATURE)) / (max(DEW_POINT_TEMPERATURE) - min(DEW_POINT_TEMPERATURE)),
    SOLAR_RADIATION = (SOLAR_RADIATION - min(SOLAR_RADIATION)) / (max(SOLAR_RADIATION) - min(SOLAR_RADIATION)),
    RAINFALL = (RAINFALL - min(RAINFALL)) / (max(RAINFALL) - min(RAINFALL)),
    SNOWFALL = (SNOWFALL - min(SNOWFALL)) / (max(SNOWFALL) - min(SNOWFALL))
  )

summary(SCALED_bike_sharing_df$RENTED_BIKE_COUNT)
summary(SCALED_bike_sharing_df$TEMPERATURE)

# Save the dataset as `seoul_bike_sharing_converted_normalized.csv`
write.csv(bike_sharing_df, "C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project/seoul_bike_sharing_converted_normalized.csv", row.names = FALSE)


#### Standardize the column names again for the new datasets

dataset_list <- c('seoul_bike_sharing.csv', 'seoul_bike_sharing_converted.csv', 'seoul_bike_sharing_converted_normalized.csv')

for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read_csv(dataset_name)
  # Standardized its columns:
  # Convert all columns names to uppercase
  names(dataset) <- toupper(names(dataset))
  # Replace any white space separators by underscore, using str_replace_all function
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  # Save the dataset back
  write.csv(dataset, dataset_name, row.names=FALSE)
}









