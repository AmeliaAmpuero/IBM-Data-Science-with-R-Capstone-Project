# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(yardstick)
library('glmnet')
library(dplyr)

# Dataset URL
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)

# Deleting DATE and FUNCTIONING DAY from variables
bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)

# Split the data into training and testing sets (75% train, 25% test)
set.seed(1234)
bike_sharing_split <- initial_split(bike_sharing_df, prop = 0.75)
train_data <- training(bike_sharing_split)
test_data <- testing(bike_sharing_split)

# Verify the split
nrow(train_data)  # Number of rows in the training data
nrow(test_data)   # Number of rows in the testing data

# Fit a polynomial regression model with higher-order terms on important variables (TEMPERATURE, HUMIDITY, RAINFALL)
lm_poly <- lm(
  RENTED_BIKE_COUNT ~ poly(RAINFALL, 6) + poly(HUMIDITY, 4) + poly(TEMPERATURE, 3) + poly(DEW_POINT_TEMPERATURE, 2),
  data = train_data
)

# Print model summary
summary(lm_poly)

# Step 2: Generate predictions for the test dataset
# Use the predict() function to generate test results
lm_poly_test_results <- predict(lm_poly, newdata = test_data) %>%
  as.data.frame()  # Convert the predictions to a data frame

# Add the actual values (truth) from test_data for comparison
lm_poly_test_results <- lm_poly_test_results %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

# Step 3: Handle negative predictions (we can't have negative rented bike counts)
# Convert all negative predictions to zero
lm_poly_test_results$. <- replace(lm_poly_test_results$.,
                                  lm_poly_test_results$. < 0, 0)

# Check the first few rows of the results
head(lm_poly_test_results)

# Step 4: Calculate R-squared and RMSE for the test results
# Calculate R-squared
rsq <- rsq_vec(lm_poly_test_results$. , lm_poly_test_results$truth)

# Calculate RMSE (Root Mean Squared Error)
rmse_result <- rmse(lm_poly_test_results, truth = truth, estimate = .)

# Print R-squared and RMSE values
cat("R-squared:", rsq, "\n")
cat("RMSE:", rmse_result$.estimate, "\n")



#TASK: Add interactions terms

# Fit a polynomial regression model with interaction terms
lm_poly_interactions <- lm(
  RENTED_BIKE_COUNT ~ 
    poly(TEMPERATURE, 6) + poly(HUMIDITY, 6) + poly(RAINFALL, 4) + poly(DEW_POINT_TEMPERATURE, 4) +
    TEMPERATURE*HUMIDITY + RAINFALL*HUMIDITY + TEMPERATURE*RAINFALL,
  data = train_data)

# Print model summary
summary(lm_poly_interactions)

# Step 2: Generate predictions for the test dataset
lm_poly_interactions_test_results <- predict(lm_poly_interactions, newdata = test_data) %>%
  as.data.frame()

# Add the actual values for comparison
lm_poly_interactions_test_results <- lm_poly_interactions_test_results %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

# Handle negative predictions
lm_poly_interactions_test_results$. <- replace(
  lm_poly_interactions_test_results$.,
  lm_poly_interactions_test_results$. < 0, 0)

# Calculate R-squared and RMSE for the new model
rsq_interaction <- rsq_vec(lm_poly_interactions_test_results$. , lm_poly_interactions_test_results$truth)
rmse_interaction <- rmse(lm_poly_interactions_test_results, truth = truth, estimate = .)

# Print R-squared and RMSE
cat("R-squared with Interactions:", rsq_interaction, "\n")
cat("RMSE with Interactions:", rmse_interaction$.estimate, "\n")



#TASK: Add regularizations terms

# Create the recipe for your bike-sharing dataset (with RENTED_BIKE_COUNT as the target)
bike_recipe <- recipe(RENTED_BIKE_COUNT ~ ., data = train_data)

# 1. **Fitting a model with fixed penalty and mixture values (no grid search)**
# Define the Elastic Net model (with penalty and mixture fixed)
elastic_net_spec <- linear_reg(penalty = 0.5, mixture = 0.2) %>%
  set_engine("glmnet")

# Create the workflow and add the recipe
elastic_net_wf <- workflow() %>%
  add_recipe(bike_recipe)

# Fit the model to the training data
elastic_net_fit <- elastic_net_wf %>%
  add_model(elastic_net_spec) %>%
  fit(data = train_data)

# View the model's coefficients
elastic_net_fit %>%
  pull_workflow_fit() %>%
  tidy()

# 2. **Performing Grid Search for Hyperparameter Tuning (penalty and mixture)**

# Define the Elastic Net model with tunable penalty and mixture parameters
tune_spec <- linear_reg(
  penalty = tune(),               # Tuning the penalty (regularization strength)
  mixture = tune()                # Tuning the mixture (balance between Lasso and Ridge)
) %>%
  set_engine("glmnet")

# Create a workflow and add the recipe
bike_wf <- workflow() %>%
  add_recipe(bike_recipe)

# Create cross-validation folds (5-fold in this case)
bike_cvfolds <- vfold_cv(train_data, v = 5)

# Create a grid of hyperparameters to test. We're tuning penalty and mixture.
# Note: penalty should be in log scale (log10 scale) for glmnet
lambda_grid <- grid_regular(
  penalty(range = c(-3, 0.3)),    # Log scale for regularization strength
  mixture(range = c(0, 1)),       # Lasso/Ridge mixture (0 = Ridge, 1 = Lasso)
  levels = 50                      # Number of parameter combinations to try
)

# Perform grid search with cross-validation
bike_grid <- tune_grid(
  bike_wf %>% add_model(tune_spec),    # Workflow with model to tune
  resamples = bike_cvfolds,            # Cross-validation resamples
  grid = lambda_grid                   # Grid of hyperparameter combinations
)

# View the best models based on RMSE
show_best(bike_grid, metric = "rmse")

# View the best models based on R² (for evaluation)
show_best(bike_grid, metric = "rsq")


best_spec <- linear_reg(penalty = 2.00, mixture = 0.0204) %>%
  set_engine("glmnet")

best_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(best_spec)

final_fit <- best_wf %>%
  fit(data = train_data)

# View the final model's coefficients
final_fit %>%
  pull_workflow_fit() %>%
  tidy()

final_predictions <- predict(final_fit, new_data = test_data)

# Calculate RMSE and R² on the test data
test_results <- bind_cols(test_data, final_predictions) %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

# Print RMSE and R²
print(test_results)



#TASK: Experiment to search for improved models

# Model 1 (from before): Polynomial Regression with Higher-Order Terms
rsq_model1 <- rsq_vec(lm_poly_test_results$. , lm_poly_test_results$truth)
rmse_model1 <- rmse(lm_poly_test_results, truth = truth, estimate = .)

# Model 2: Polynomial Terms, Interaction Terms, and L2 Regularization
model2 <- linear_reg(penalty = 0.02, mixture = 1) %>% 
  set_engine("glmnet")

model2_fit <- model2 %>% 
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER * `18` + DEW_POINT_TEMPERATURE + SOLAR_RADIATION + SUMMER * `18` + HUMIDITY, data = train_data)

model2_train_results <- model2_fit %>% 
  predict(new_data = train_data) %>% 
  mutate(truth = train_data$RENTED_BIKE_COUNT)

model2$.pred <- replace(model2_train_results$.pred, model2_train_results$.pred < 0, 0)

rsq_model2 <- rsq(model2_train_results, truth = truth, estimate = .pred)
rmse_model2 <- rmse(model2_train_results, truth = truth, estimate = .pred)

# Model 3: Polynomial Terms, Interaction Terms, and Elastic Regularization (Lasso + Ridge)
model3 <- linear_reg(penalty = 0.02, mixture = 0.2) %>% 
  set_engine("glmnet")

model3_fit <- model3 %>% 
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER * `18` + poly(DEW_POINT_TEMPERATURE, 6) + SOLAR_RADIATION + SUMMER * `18` + TEMPERATURE * HUMIDITY, data = train_data)

model3_train_results <- model3_fit %>% 
  predict(new_data = train_data) %>% 
  mutate(truth = train_data$RENTED_BIKE_COUNT)

model3$.pred <- replace(model3_train_results$.pred, model3_train_results$.pred < 0, 0)

rsq_model3 <- rsq(model3_train_results, truth = truth, estimate = .pred)
rmse_model3 <- rmse(model3_train_results, truth = truth, estimate = .pred)

# Model 4: Polynomial Terms, Interaction Terms, and Elastic Regularization with More Complex Terms
model4 <- linear_reg(penalty = 0.0015, mixture = 0.2) %>% 
  set_engine("glmnet")

model4_fit <- model4 %>% 
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER * `18` + poly(DEW_POINT_TEMPERATURE, 6) + poly(SOLAR_RADIATION, 6) + SUMMER * `18` + TEMPERATURE * HUMIDITY + poly(HUMIDITY, 6), data = train_data)

model4_train_results <- model4_fit %>% 
  predict(new_data = train_data) %>% 
  mutate(truth = train_data$RENTED_BIKE_COUNT)

model4$.pred <- replace(model4_train_results$.pred, model4_train_results$.pred < 0, 0)

rsq_model4 <- rsq(model4_train_results, truth = truth, estimate = .pred)
rmse_model4 <- rmse(model4_train_results, truth = truth, estimate = .pred)


# Model 5: Polynomial Terms, Interaction Terms, and Elastic Regularization with Even More Complex Terms
model5 <- linear_reg(penalty = 0.0015, mixture = 0.2) %>% 
  set_engine("glmnet")

model5_fit <- model5 %>% 
  fit(RENTED_BIKE_COUNT ~ . + poly(TEMPERATURE, 6) + WINTER * `18` + poly(DEW_POINT_TEMPERATURE, 6) + poly(SOLAR_RADIATION, 6) + poly(VISIBILITY, 6) + SUMMER * `18` + TEMPERATURE * HUMIDITY + poly(HUMIDITY, 6) + RAINFALL * TEMPERATURE + SNOWFALL * TEMPERATURE + RAINFALL * HUMIDITY + SNOWFALL * HUMIDITY, data = train_data)

model5_train_results <- model5_fit %>% 
  predict(new_data = train_data) %>% 
  mutate(truth = train_data$RENTED_BIKE_COUNT)

model5$.pred <- replace(model5_train_results$.pred, model5_train_results$.pred < 0, 0)

rsq_model5 <- rsq(model5_train_results, truth = truth, estimate = .pred)
rmse_model5 <- rmse(model5_train_results, truth = truth, estimate = .pred)
print(rsq_model5)
print(rmse_model5)

# Compare the RMSE and R-squared of all models
model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5")
rsq_values <- c(0.514364, 0.707, 0.733, 0.762, 0.766)
rmse_values <- c(441., 349, 333, 315, 312)

# Create a data frame with the necessary columns
comparison_df <- data.frame(model_names, rsq_values,rmse_values)
print(comparison_df)
# Now reshape the data for visualization
comparison_df %>%
  pivot_longer(cols = c(rsq_values, rmse_values), names_to = "Metric", values_to = "Value") %>%
  ggplot(aes(x = model_names, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of R-squared and RMSE Across Models", fill = "Metric")

# Use the best model (model5 in this case) to generate a Q-Q plot comparing predictions and true values
ggplot(model5_train_results) +
  stat_qq(aes(sample = truth), color = "green") +
  stat_qq(aes(sample = .pred), color = "red") +
  labs(title = "Q-Q Plot: Predicted vs Actual")
