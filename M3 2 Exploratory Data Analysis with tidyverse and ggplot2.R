library(tidyverse)
library(stringr)  # For string manipulation
library(readr)    # For reading CSV files (read_csv())
library(dplyr)
library(ggplot2)
setwd("C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project")

#Task 1 - Load the dataset
seoul_bike_sharing <- read_csv("seoul_bike_sharing.csv")
head(seoul_bike_sharing)

#Task 2 - Recast DATE as a date
seoul_bike_sharing$DATE <- as.Date(seoul_bike_sharing$DATE, format = "%d/%m/%Y")
seoul_bike_sharing$DATE

#Task 3 - Cast HOUR as a categorical variable
seoul_bike_sharing$HOUR <-as.factor(seoul_bike_sharing$HOUR)

#Check the structure of the dataframe 
str(seoul_bike_sharing)

#Ensure there are no NAs
sum(is.na(seoul_bike_sharing))

#### DESCRIPTIVE STATISTICS

#Task 4 - Dataset Summary
summary(seoul_bike_sharing)
seoul_bike_sharing$SEASONS <-as.factor(seoul_bike_sharing$SEASONS)
#Observations:
#Temp has a large range: It may explain at least some of the variation in bike rentals
#Rainfall & snowfall are rare: Just happening in the 4th quartiles.
#Avg. WindSpeed is light (1.7m/s). Even the max. is a moderate breeze. 


#Task 5 - Based on the above stats, calculate how many Holidays there are.
holidays_count <- seoul_bike_sharing %>%
  filter(HOLIDAY == "Holiday") %>%
  distinct(DATE) %>%  
  count()
holidays_count


#Task 6 - Calculate the percentage of records that fall on a holiday.
total_records <- nrow(seoul_bike_sharing) # Total records in the dataset

holiday_percentage <- (holidays_count / total_records) * 100 #Calculate %
holiday_percentage

#Task 7 - Given there is exactly a full year of data, determine how many records we expect to have.
day_count <- seoul_bike_sharing %>%
  distinct(DATE) %>%  
  count()
expected_records <- day_count * 24
expected_records

#Task 8 - Given the observations for the 'FUNCTIONING_DAY' how many records must there be?
seoul_bike_sharing %>% 
  count(FUNCTIONING_DAY)

#Task 9 - Use the summarize() function to calculate the seasonal total rainfall and snowfall.
seoul_bike_sharing %>%
  group_by(SEASONS) %>%
  summarize(total_rainfall = sum(RAINFALL), 
            total_snowfall = sum(SNOWFALL))

#### DATA VISUALIZATION
#Task 10 - Create a scatter plot of RENTED_BIKE_COUNT vs DATE
ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT)) +
  geom_point(alpha = 0.3) +  # Adjust alpha for opacity (0 = fully transparent, 1 = fully opaque)
  labs(
    title = "Scatter Plot of RENTED_BIKE_COUNT vs DATE",
    x = "Date",
    y = "Rented Bike Count") +
  theme_minimal()  # A cleaner theme for better visibility

#Task 11 - Create the same plot of the RENTED_BIKE_COUNT time series, but now add HOUR as the colour.
ggplot(seoul_bike_sharing, aes(x = DATE, y = RENTED_BIKE_COUNT, colour = HOUR)) +
  geom_point(alpha = 0.5) +  # Adjust alpha for opacity (0 = fully transparent, 1 = fully opaque)
  labs(
    title = "Scatter Plot of RENTED_BIKE_COUNT vs DATE",
    x = "Date",
    y = "Rented Bike Count",
    colour= "Hour of the Day")
#during the summer there is more use of bikes in the afternoon
#less people use it in the morning
#the highest amount of bikes are used in the summer and way less in the winter and autumn

### Distributions

#Task 12 - Create a histogram overlaid with a kernel density curve
ggplot(seoul_bike_sharing, aes(x = RENTED_BIKE_COUNT)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +  # Histogram
  geom_density(alpha = 0.3, fill = "red") +  # Kernel Density Curve
  labs(
    title = "Histogram and Kernel Density Curve of RENTED_BIKE_COUNT",
    x = "Rented Bike Count",
    y = "Density")
#happens that less than 1000 bikes are rented in general
#around 3K are less likely to be rented

### Correlation between two variables (scatter plot)

#Task 13 - Use a scatter plot to visualize the correlation between RENTED_BIKE_COUNT and TEMPERATURE by SEASONS
#WITH SEASONALITY
ggplot(seoul_bike_sharing, aes(x = TEMPERATURE, y = RENTED_BIKE_COUNT, colour = factor(HOUR))) +
  geom_point(alpha = 0.5) +  # Scatter plot with opacity
  facet_wrap(~ SEASONS) +  # Separate by SEASON
  labs(title = "Bike Rentals vs Temperature by Season") +
  theme_minimal()  # Minimal theme
#WITHOUT SEASONALITY
ggplot(seoul_bike_sharing) +
  geom_point(aes(x=TEMPERATURE,y=RENTED_BIKE_COUNT,colour=HOUR),alpha=1/5)

##Describe the patterns you see
#Visually, we can see some strong correlations as approximately linear patterns.


### Outliers (Boxplot)
#Task 14 - Create a display of four boxplots of RENTED_BIKE_COUNT vs. HOUR grouped by SEASONS.
ggplot(seoul_bike_sharing, aes(x = factor(HOUR), y = RENTED_BIKE_COUNT)) +
  geom_boxplot() +  # Create boxplot
  facet_wrap(~ SEASONS) +  # Separate by SEASON
  labs(title = "Boxplot of RENTED_BIKE_COUNT vs HOUR by SEASON",
       x = "Hour of Day",
       y = "Rented Bike Count") +
  theme_minimal()


##Compare and contrast the key features of these boxplots between seasons.
#Although the overall scale of bike rental counts changes with the seasons, key features remain very similar.
#For example, peak demand times are the same across all seasons, at 8 am and 6 pm.


#Task 15 - Group the data by DATE, and use the summarize() function to calculate the daily total rainfall and snowfall.
daily_weather <- seoul_bike_sharing %>%
  group_by(DATE) %>%
  summarise(
    total_rainfall = sum(RAINFALL, na.rm = TRUE),
    total_snowfall = sum(SNOWFALL, na.rm = TRUE)
  )
daily_weather

ggplot(seoul_bike_sharing, aes(x = DATE, y = RAINFALL)) +
  geom_point(alpha = 0.3) +  # Adjust alpha for opacity (0 = fully transparent, 1 = fully opaque)
  labs(
    title = "Scatter Plot of RAINFALL vs DATE",
    x = "Date",
    y = "Rainfall") +
  theme_minimal()  # A cleaner theme for better visibility

#Task 16 - Determine how many days had snowfall
days_with_snowfall <- seoul_bike_sharing %>%
  group_by(DATE) %>%
  summarise(total_snowfall = sum(SNOWFALL, na.rm = TRUE)) %>%
  filter(total_snowfall > 0) %>%
  count()
days_with_snowfall

#Visualization to show bike count depending on the day of the week.
seoul_bike_sharing %>%
  mutate(DAY_OF_WEEK = as.factor(wday(DATE, abbr = TRUE, label = TRUE))) %>%
  ggplot(aes(x = DAY_OF_WEEK, y = RENTED_BIKE_COUNT, color = HOUR, alpha = 0.25)) +
  geom_point() +
  facet_wrap(~SEASONS)

#####part of PPT presentation
# Task 17 - Group the data by DATE and calculate the daily total rainfall and snowfall
daily_weather <- seoul_bike_sharing %>%
  group_by(DATE) %>%
  summarise(
    total_rainfall = sum(RAINFALL, na.rm = TRUE),
    total_snowfall = sum(SNOWFALL, na.rm = TRUE)
  )

# Reshaping data to long format for easy plotting
daily_weather_long <- daily_weather %>%
  gather(key = "weather_type", value = "total", total_rainfall, total_snowfall)

# Bar chart for daily total rainfall and snowfall
ggplot(daily_weather_long, aes(x = DATE, y = total, fill = weather_type)) +
  geom_bar(stat = "identity", position = "dodge") +  # 'dodge' separates the bars for rainfall and snowfall
  labs(
    title = "Daily Total Rainfall and Snowfall",
    x = "Date",
    y = "Total (mm)",
    fill = "Weather Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


