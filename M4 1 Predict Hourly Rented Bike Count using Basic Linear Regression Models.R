library(tidyverse)
library(tidymodels)
library(stringr)  # For string manipulation
library(readr)    # For reading CSV files (read_csv())
library(dplyr)
library(ggplot2)
library(yardstick)# Load yardstick for metrics

setwd("C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project")

# Dataset URL
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
# View the column specification
glimpse(bike_sharing_df)  # Compact view of the dataset

# Deleting DATE and FUNCTIONING DAY from variables
bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)

#### TASK: Split data into training and testing datasets
set.seed(1234)
bike_sharing_split <- initial_split(bike_sharing_df, prop = 0.75)
train_data <- training(bike_sharing_split)
test_data <- testing(bike_sharing_split)

# Verify the split
nrow(train_data)  # Number of rows in the training data
nrow(test_data)  # Number of rows in the testing data


#### TASK: Build a linear regression model using only the weather variables

# Define a linear regression model specification
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Fit the linear regression model to the training data using the weather variables as predictors.
lm_model_weather <- lm_spec %>%
  fit(RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED + VISIBILITY +
        DEW_POINT_TEMPERATURE + SOLAR_RADIATION + RAINFALL + SNOWFALL,
      data = train_data)

#Print the fit summary for the "lm_model_weather" model.
summary(lm_model_weather$fit)


#### TASK: Build a linear regression model using all variables
#Other factors that may affect bike rental demand, such as the hour or if it's holiday.

# Fit the model using all variables
lm_model_all <- lm_spec %>%
  fit(RENTED_BIKE_COUNT ~ ., data = train_data)

# Print the model summary
summary(lm_model_all$fit)


#### TASK: Evaluate the models and identify important variables

# 1. Predictions for the weather-only model on the test data
test_data <- test_data %>%
  mutate(test_results_weather = predict(lm_model_weather, new_data = test_data)$.pred)

# 2. Predictions for the all-variables model on the test data
test_data <- test_data %>%
  mutate(test_results_all = predict(lm_model_all, new_data = test_data)$.pred)

# 3. Create two separate dataframes for test results: weather-only and all-variables models

weather_test_results <- test_data %>%
  select(test_results_weather) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

all_test_results <- test_data %>%
  select(test_results_all) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

# 4. Print the test result dataframes (optional)
print(weather_test_results)
print(all_test_results)

# 5. Evaluate the weather-only model on the test data
weather_rmse <- rmse(weather_test_results, truth = truth, estimate = test_results_weather)
weather_rsq <- rsq(weather_test_results, truth = truth, estimate = test_results_weather)

# 6. Evaluate the all-variables model on the test data
all_rmse <- rmse(all_test_results, truth = truth, estimate = test_results_all)
all_rsq <- rsq(all_test_results, truth = truth, estimate = test_results_all)

# 7. Print the evaluation results for both models
print(weather_rmse)
print(weather_rsq)
print(all_rmse)
print(all_rsq)
#test results from lm_model_all are much better

#Variables with larger coefficients in the model means they attribute more in the prediction of RENTED_BIKE_COUNT
#Print all coefficients:
lm_model_all$fit$coefficients
names(lm_model_all$fit$coefficients) #Extracts the names of the predictors (row names of the coefficients vector)

# Sort coefficient list
lm_model_all %>%
  tidy() %>%
  arrange(desc(abs(estimate)))

# Visualize the list using ggplot and geom_bar
lm_model_all %>%  ## Start with the regression model object
  tidy() %>%  ## Convert the model object into a tidy data frame with coefficients and stats
  filter(!is.na(estimate)) %>%  ## Remove any predictors with NA coefficients to avoid errors in plotting
  ggplot(aes(  ## Begin creating a ggplot visualization
    x = fct_reorder(term, abs(estimate)),  ## Reorder predictors based on the absolute coefficient values
    y = abs(estimate)  ## Plot the absolute coefficient values for comparison
  )) +
  geom_bar(stat = "identity", fill = "RED") +  ## Create a bar plot with red bars, using actual coefficient values
  coord_flip() +  ## Flip the axes so predictor variables appear on the y-axis for better readability
  theme(  ## Customize the plot's appearance
    axis.text.y = element_text(  ## Modify the text of the y-axis labels
      angle = 10,  ## Slightly tilt the text for better alignment
      colour = "RED",  ## Change the text color to red
      size = 7  ## Adjust the font size for compact display
    )
  ) +
  xlab("variable")  ## Rename the x-axis label to "variable"

#The main reason we use absolute value is to easily identify important variables, 
#i.e. variables with large magnitudes, no matter it's negative or positive. 





#####without absolute value

lm_model_all %>%  ## Start with the regression model object
  tidy() %>%  ## Convert the model object into a tidy data frame with coefficients and stats
  filter(!is.na(estimate)) %>%  ## Remove any predictors with NA coefficients to avoid errors in plotting
  ggplot(aes(  ## Begin creating a ggplot visualization
    x = fct_reorder(term, abs(estimate)),  ## Reorder predictors based on the absolute coefficient values
    y = estimate  ## Plot the absolute coefficient values for comparison
  )) +
  geom_bar(stat = "identity", fill = "RED") +  ## Create a bar plot with red bars, using actual coefficient values
  coord_flip() +  ## Flip the axes so predictor variables appear on the y-axis for better readability
  theme(  ## Customize the plot's appearance
    axis.text.y = element_text(  ## Modify the text of the y-axis labels
      angle = 10,  ## Slightly tilt the text for better alignment
      colour = "RED",  ## Change the text color to red
      size = 7  ## Adjust the font size for compact display
    )
  ) +
  xlab("variable")  ## Rename the x-axis label to "variable"







