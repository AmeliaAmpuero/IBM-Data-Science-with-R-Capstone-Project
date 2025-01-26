require("rvest")

library(rvest) #To obtain table from web page, convert it into a df, and write the df to a csv file..
library(httr) #To get some HTML pages by sending HTTP GET request


########### Web scrape a Global Bike-Sharing Systems Wiki Page ###########

# Get the root HTML node by calling the `read_html()` method with URL
table_url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"

root_node <-read_html(table_url)

table_nodes <- html_node(root_node, "table")

# Extract content from table_node and convert the data into a dataframe
data_frame <- html_table(table_nodes)
head(data_frame)
tail(data_frame)
names(data_frame)

# Export the dataframe into a csv file
write.csv(data_frame, file = "C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project/raw_bike_sharing_systems.csv", row.names = FALSE)


########### OpenWeather APIs Calls ###########

# URL for Current Weather API
current_weather_url <- 'https://api.openweathermap.org/data/2.5/weather'
# need to be replaced by your real API key
your_api_key <- "a393f0ea9de1d0a60106be937226fd46"
# Input `q` is the city name
# Input `appid` is your API KEY, 
# Input `units` are preferred units such as Metric or Imperial
current_query <- list(q = "Seoul", appid = your_api_key, units="metric")

#we can make a HTTP request to the current weather API
response <- GET(current_weather_url, query=current_query)

#If we check the response type, we can see it is in JSON format
http_type(response)

#To read the JSON HTTP response, you can use the content()
json_result <- content(response, as="parsed")

class(json_result) #To see it as a R list object
json_result #To print JSON result

# Create some empty vectors to hold data temporarily
weather <- c()
visibility <- c()
temp <- c()
temp_min <- c()
temp_max <- c()
pressure <- c()
humidity <- c()
wind_speed <- c()
wind_deg <- c()


# $weather is also a list with one element, its $main element indicates the weather status such as clear or rain
weather <- c(weather, json_result$weather[[1]]$main)
# Get Visibility
visibility <- c(visibility, json_result$visibility)
# Get current temperature 
temp <- c(temp, json_result$main$temp)
# Get min temperature 
temp_min <- c(temp_min, json_result$main$temp_min)
# Get max temperature 
temp_max <- c(temp_max, json_result$main$temp_max)
# Get pressure
pressure <- c(pressure, json_result$main$pressure)
# Get humidity
humidity <- c(humidity, json_result$main$humidity)
# Get wind speed
wind_speed <- c(wind_speed, json_result$wind$speed)
# Get wind direction
wind_deg <- c(wind_deg, json_result$wind$deg)

# Combine all vectors
weather_data_frame <- data.frame(weather=weather, 
                                 visibility=visibility, 
                                 temp=temp, 
                                 temp_min=temp_min, 
                                 temp_max=temp_max, 
                                 pressure=pressure, 
                                 humidity=humidity, 
                                 wind_speed=wind_speed, 
                                 wind_deg=wind_deg)
# Check the generated data frame
print(weather_data_frame)


#### TASK: Get 5-day weather forecasts for a list of cities using the OpenWeather API
# Your OpenWeather API key
your_api_key <- "a393f0ea9de1d0a60106be937226fd46"
# Create some empty vectors to hold data temporarily
city <- c()
weather <- c()
visibility <- c()
temp <- c()
temp_min <- c()
temp_max <- c()
pressure <- c()
humidity <- c()
wind_speed <- c()
wind_deg <- c()
forecast_datetime <- c()

# Get 5 -day weather forecast for a list of cities
get_weather_forecast_by_cities <- function(city_names) {
  df <- data.frame()
  for (city_name in city_names) {
    #forecast API URL
    forecast_url <-'https://api.openweathermap.org/data/2.5/weather' 
    #create query parameter
    forecast_query <- list(q=city_name,appid=your_api_key, units="metric")
    #make HTTP GET call for the given city
    response <- GET(forecast_url, query=forecast_query)
    json_result <- content(response, as="parsed")
    results <- json_result$list
    #Loop the json result
    for(result in results) {
      city <- c(city, city_name)
    }
    # Add R lists into a data frame
    city <- c(city, json_result$name)
    weather <- c(weather, json_result$weather[[1]]$main)
    visibility <- c(visibility, json_result$visibility)
    temp <- c(temp, json_result$main$temp)
    temp_min <-c(temp_min, json_result$main$temp_min)
    temp_max <- c(temp_max, json_result$main$temp_max)
    pressure <- c(pressure, json_result$main$pressure)
    humidity <- c(humidity, json_result$main$humidity)
    wind_speed <- c(wind_speed, json_result$wind$speed)
    wind_deg <-c(wind_deg, json_result$wind$deg)
    forecast_datetime <- c(forecast_datetime,json_result$dt)
    
    #Combine all vector into data frame
    df <- data.frame(city = city,
                     weather=weather, 
                     visibility=visibility, 
                     temp=temp, 
                     temp_min=temp_min, 
                     temp_max=temp_max, 
                     pressure=pressure, 
                     humidity=humidity, 
                     wind_speed=wind_speed, 
                     wind_deg=wind_deg,
                     forecast_datetime=forecast_datetime)
  }
  return(df)
}

cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
cities_weather_df <- get_weather_forecast_by_cities(cities)
print(cities_weather_df)

# Write cities_weather_df to `cities_weather_forecast.csv`
write.csv(cities_weather_df, file = "C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project/cities_weather_df.csv", row.names=FALSE)


#### TASK: Download datasets as csv files from cloud storage
# Download several datasets

# Download some general city information such as name and locations
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
# download the file
download.file(url, destfile = "C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project/raw_worldcities.csv")

# Download a specific hourly Seoul bike sharing demand dataset
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
# download the file
download.file(url, destfile = "C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project/raw_seoul_bike_sharing.csv")


