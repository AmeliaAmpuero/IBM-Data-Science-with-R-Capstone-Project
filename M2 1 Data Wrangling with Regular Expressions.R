library(tidyverse)
library(stringr)  # For string manipulation
library(readr)    # For reading CSV files (read_csv())
library(dplyr)
#### TASK: Standardize column names for all collected datasets
setwd("C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project")

dataset_list <- c('raw_bike_sharing_systems.csv', 'raw_seoul_bike_sharing.csv', 'raw_cities_weather_forecast.csv', 'raw_worldcities.csv')

for (dataset_name in dataset_list) {
  
  # Read dataset
  dataset <- read_csv(dataset_name)
  
# Standardized its columns:
  
  # Convert all column names to uppercase
  
  names(dataset) <- toupper(names(dataset))
  
  # Replace any white space separators by underscores, using the str_replace_all function
  
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  
  # Save the dataset
  write.csv(dataset, dataset_name, row.names = FALSE)
}


# Print a summary for each data set to check whether the column names were correctly converted

for (dataset_name in dataset_list) {
  dataset <- read_csv(dataset_name)
  print(colnames(dataset))
}

####TASK: Remove undesired reference links from the scraped bike-sharing systems dataset

# First load the dataset
bike_sharing_df <- read_csv("raw_bike_sharing_systems.csv")
head(bike_sharing_df)

# Select the four columns
sub_bike_sharing_df <- bike_sharing_df %>% select(COUNTRY, CITY, SYSTEM, BICYCLES)

#Types of the selected columns
sub_bike_sharing_df %>% 
  summarize_all(class) %>%
  gather(variable, class)

#ABOUT COLUMN BICLYCLE: 
#Let's see why it wasn't loaded as a numeric column:possibly some entries contain characters.
# grepl searches a string for non-digital characters, and returns TRUE or FALSE
# if it finds any non-digital characters, then the bicyle column is not purely numeric
find_character <- function(strings) grepl("[^0-9]", strings)

#To find any elements in the Bicycles column containing non-numeric characters
sub_bike_sharing_df %>% 
  select(BICYCLES) %>% 
  filter(find_character(BICYCLES)) %>%
  slice(0:10)
#RESULT: many rows have non-numeric characters

# Define a 'reference link' character class, 
# `[A-z0-9]` means at least one character 
# `\\[` and `\\]` means the character is wrapped by [], such as for [12] or [abc]
ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)

# Check whether the COUNTRY column has any reference links
sub_bike_sharing_df %>% 
  select(COUNTRY) %>% 
  filter(find_reference_pattern(COUNTRY)) %>%
  slice(0:10)
#RESULT: COUNTRY IS CLEAN

# Check whether the CITY column has any reference links
sub_bike_sharing_df %>% 
  select(CITY) %>% 
  filter(find_reference_pattern(CITY)) %>%
  slice(0:10)
#RESULT: CITY HAS REFERENCE LINKS TO BE REMOVED

# Check whether the System column has any reference links
sub_bike_sharing_df %>% 
  select(SYSTEM) %>% 
  filter(find_reference_pattern(SYSTEM)) %>%
  slice(0:10)
#RESULT: SYSTEM HAS REFERENCE LINKS TO BE REMOVED

##CONCLUSION: 
#CITY and SYSTEM columns have some undesired reference links
#BICYCLES column has both reference links and some textual annotations.


####TASK: Remove undesired reference links using regular expressions

# To replace all reference links with an empty character for columns CITY and SYSTEM
# Define the remove_ref function
remove_ref <- function(strings) {
  # Pattern to match reference links, e.g., [1], [2], etc.
  ref_pattern <- "\\[\\d+\\]"  # Matches text like [1], [23], etc.
  
  # Replace all matched substrings with a white space using str_replace_all
  result <- str_replace_all(strings, ref_pattern, "")
  
  # Trim the result to remove any unnecessary spaces
  result <- str_trim(result)
  
  # Return the cleaned string
  return(result)
}

# Use the function to remove the reference links
# Apply remove_ref to CITY and SYSTEM columns
sub_bike_sharing_df <- sub_bike_sharing_df %>%
  mutate(SYSTEM = remove_ref(SYSTEM),
         CITY = remove_ref(CITY))

# Check whether all reference links are removed
# Select specific columns and filter rows with references
result <- sub_bike_sharing_df %>%
  select(CITY, SYSTEM, BICYCLES) %>%
  filter(find_reference_pattern(CITY) | 
           find_reference_pattern(SYSTEM) | 
           find_reference_pattern(BICYCLES))

# Print the result to check if any references remain
print(result)
head(result)


####TASK: Extract the numeric value using regular expressions

# Extract the first number
extract_num <- function(columns){
  
  # Define a digital pattern
  digitals_pattern <- "\\d+"  # Matches any sequence of digits
  
  # Find the first match using str_extract
  result <- str_extract(columns, digitals_pattern)
  
  # Convert the result to numeric using the as.numeric() function
  result <- as.numeric(result)
  
  # Return the numeric result
  return(result)
}

# Use the mutate() function on the BICYCLES column
sub_bike_sharing_df <- sub_bike_sharing_df %>%
  mutate(BICYCLES = extract_num(BICYCLES))

summary(sub_bike_sharing_df$BICYCLES)

# Write the dataset to a CSV file
write.csv(sub_bike_sharing_df, "C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project/bike_sharing_systems.csv", row.names = FALSE)


