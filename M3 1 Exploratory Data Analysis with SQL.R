#Install RSQLite package
install.packages("https://cran.r-project.org/src/contrib/Archive/RSQLite/RSQLite_0.10.0.tar.gz", repos = NULL, type = "source", dependencies = TRUE) 
library("RSQLite")
setwd("C:/Users/Amelia/Desktop/IBM CERTIFICATE/9 Capstone Project")

library(tidyverse)
library(stringr)  # For string manipulation
library(readr)    # For reading CSV files (read_csv())
library(dplyr)
library(ggplot2)

#Establish Connection
conn <- dbConnect(RSQLite::SQLite(),"RDB.sqlite")

#Read Datasets
SEOUL_BIKE_SHARING <- read_csv("seoul_bike_sharing.csv")
CITIES_WEATHER_FORECAST <- read_csv("cities_weather_forecast.csv")
BIKE_SHARING_SYSTEMS <- read_csv("bike_sharing_systems.csv")
WORLD_CITIES <- read_csv("world_cities.csv")

#Load Tables
dbWriteTable(conn, "SEOUL_BIKE_SHARING", SEOUL_BIKE_SHARING, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "CITIES_WEATHER_FORECAST", CITIES_WEATHER_FORECAST, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "BIKE_SHARING_SYSTEMS", BIKE_SHARING_SYSTEMS, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "WORLD_CITIES", WORLD_CITIES, overwrite=TRUE, header = TRUE)
dbListTables(conn)


#### TASK 1: Determine how many records are in the seoul_bike_sharing dataset.
dbGetQuery(conn, 'SELECT COUNT(*) AS Records FROM SEOUL_BIKE_SHARING')

#### TASK 2: Determine how many hours had non-zero rented bike count.
dbGetQuery(conn, "SELECT count(HOUR) as Numer_of_hours FROM SEOUL_BIKE_SHARING
WHERE RENTED_BIKE_COUNT > 0")

#### TASK 3: Query the the weather forecast for Seoul over the next 3 hours.
#Recall that the records in the CITIES_WEATHER_FORECAST dataset are 3 hours apart, so we just need the first record from the query

dbGetQuery(conn, "SELECT * FROM CITIES_WEATHER_FORECAST
WHERE CITY = 'Seoul'
Limit 1")

#### TASK 4: Find which seasons are included in the seoul bike sharing dataset.
dbGetQuery(conn, "SELECT distinct SEASONS as Seasons 
FROM SEOUL_BIKE_SHARING")

#### TASK 5: Find the first and last dates in the Seoul Bike Sharing dataset.
dbGetQuery(conn, "SELECT MIN(DATE) as Start_Date, MAX(DATE) as End_Date 
FROM SEOUL_BIKE_SHARING")

#### TASK 6: Determine which date and hour had the most bike rentals.
dbGetQuery(conn, "
SELECT DATE, HOUR, RENTED_BIKE_COUNT AS Maximum_COUNT 
FROM SEOUL_BIKE_SHARING 
WHERE RENTED_BIKE_COUNT = (SELECT MAX(RENTED_BIKE_COUNT) FROM SEOUL_BIKE_SHARING)")

#### TASK 7: Determine the average hourly temperature and the average number of bike rentals per hour over each season. List the top ten results by average bike count.
dbGetQuery(conn, "SELECT SEASONS, HOUR, AVG(RENTED_BIKE_COUNT), AVG(TEMPERATURE) 
           FROM SEOUL_BIKE_SHARING 
           GROUP BY SEASONS, HOUR 
           ORDER BY AVG(RENTED_BIKE_COUNT) DESC
           LIMIT 10")

#### TASK 8: Find the average hourly bike count during each season.
#Include:  min, max, and sd of the hourly bike count for each season
dbGetQuery(conn, "
SELECT 
    SEASONS,
    AVG(RENTED_BIKE_COUNT) AS Avg_Bike_Count,
    MIN(RENTED_BIKE_COUNT) AS Min_Bike_Count,
    MAX(RENTED_BIKE_COUNT) AS Max_Bike_Count,
    SQRT(AVG(RENTED_BIKE_COUNT * RENTED_BIKE_COUNT) - AVG(RENTED_BIKE_COUNT) * AVG(RENTED_BIKE_COUNT)) AS Std_Dev_Bike_Count
FROM SEOUL_BIKE_SHARING
GROUP BY SEASONS")

#### TASK 9: Consider the weather over each season. 
#On avg, what were the TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, DEW_POINT_TEMPERATURE, SOLAR_RADIATION, RAINFALL, and SNOWFALL per season?
#Include the average bike count as well , and rank the results by average bike count
dbGetQuery(conn, "
SELECT 
    SEASONS,
    AVG(TEMPERATURE) AS Avg_Temperature,
    AVG(HUMIDITY) AS Avg_Humidity,
    AVG(WIND_SPEED) AS Avg_WindSpeed,
    AVG(VISIBILITY) AS Avg_Visibility,
    AVG(DEW_POINT_TEMPERATURE) AS Avg_DewPointTemp,
    AVG(SOLAR_RADIATION) AS Avg_SolarRad,
    AVG(RAINFALL) AS Avg_Rainfall,
    AVG(SNOWFALL) AS Avg_Snowfall,
    AVG(RENTED_BIKE_COUNT) AS Avg_RentedBikes
FROM SEOUL_BIKE_SHARING
GROUP BY SEASONS
ORDER BY AVG(RENTED_BIKE_COUNT) DESC")

#### TASK 10: Use an implicit join across the WORLD_CITIES and the BIKE_SHARING_SYSTEMS tables to determine the total number of bikes avaialble in Seoul, plus the following city information about Seoul: CITY, COUNTRY, LAT, LON, POPULATION, in a single view.
dbGetQuery(conn, "SELECT B.BICYCLES, B.CITY, B.COUNTRY, W.LAT, W.LNG, W.POPULATION 
FROM BIKE_SHARING_SYSTEMS AS B
LEFT JOIN WORLD_CITIES AS W 
ON B.CITY = W.CITY_ASCII
WHERE B.CITY = 'Seoul'")

#TASK 11: Find all cities with total bike counts between 15000 and 20000.
#Return the city and country names, plus the coordinates (LAT, LNG), population, and number of bicycles for each city.

dbGetQuery(conn, "
SELECT 
    B.BICYCLES, 
    B.CITY, 
    B.COUNTRY, 
    W.LAT, 
    W.LNG, 
    W.POPULATION  
FROM BIKE_SHARING_SYSTEMS AS B 
LEFT JOIN WORLD_CITIES AS W 
    ON B.CITY = W.CITY_ASCII
WHERE B.CITY = 'Seoul' 
   OR (B.BICYCLES BETWEEN 15000 AND 20000)
ORDER BY B.BICYCLES DESC
")

dbListTables(conn)

# Once you're done with the connection, close it
dbDisconnect(conn)
