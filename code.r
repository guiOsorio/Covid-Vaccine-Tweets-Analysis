# Clean global environment
rm(list = ls())

# Data from:
# https://www.kaggle.com/gpreda/all-covid19-vaccines-tweets

# Load libraries
library(dplyr)
library(sqldf)
library(ggplot2)
library(lubridate)

# Read local csv file with data
data <- read.csv("data/vaccination_all_tweets.csv")
View(data)
dim(data)

# How many different user locations are there?
user_locations <- as.factor(data$user_location)
length(levels(user_locations))
# There are 25459 distinct user_locations

# What are the 20 most common user_location?
Q1 <- "SELECT user_location, COUNT(*) FROM data
          GROUP BY user_location
          ORDER BY COUNT(*) DESC LIMIT 20"
sqldf(Q1)

# Based on these results, we decided it would be best to divide
# our data into 5 groups: India, Canada, US, China, and the UK

# Create new df with only the tweets from India (36846 obs.)
data_India <- filter(data, user_location == "Bengaluru, India"
                     | user_location == "India"
                     | user_location == "New Delhi, India"
                     | user_location == "Mumbai, India"
                     | user_location == "New Delhi"
                     | user_location == "Mumbai"
                     | user_location == "Hyderabad, India"
                     | user_location == "Chennai, India")
# Change all user_location in India to be 'India'
data_India$user_location[] = "India"

# Create new df with only the tweets from Canada (4558 obs.)
data_Canada <- filter(data, user_location == "Toronto, Canada  and Worldwide"
                     | user_location == "Canada"
                     | user_location == "Toronto, Ontario")
# Change all user_location in Canada to be 'Canada'
data_Canada$user_location[] = "Canada"

# Create new df with only the tweets from US (3766 obs.)
data_US <- filter(data, user_location == "United States"
                      | user_location == "Los Angeles, CA"
                      | user_location == "California, USA")
# Change all user_location in US to be 'US'
data_US$user_location[] = "US"

# Create new df with only the tweets from China (1715 obs.)
data_China <- filter(data, user_location == "Beijing, China"
                  | user_location == "Beijing")
# Change all user_location in China to be 'China'
data_China$user_location[] = "China"

# Create new df with only the tweets from the UK (1876 obs.)
data_UK <- filter(data, user_location == "London"
                     | user_location == "London, England")
# Change all user_location in the UK to be 'UK'
data_UK$user_location[] = "UK"

# Merge all data frames into 1 (48761 obs.)
data_5 <- bind_rows(
  data_India, data_Canada, data_US, data_China, data_UK
)
View(data_5)

# Create separate table (data frame) to store user_location info
# Create new data frame with 2 columns (country_id, country)
country_id <- c(1, 2, 3, 4, 5)
country <- distinct(data_5, user_location)
countries_df <- data.frame(country_id, country)
View(countries_df)

# On data_5, add new column with corresponding country_id from the
  # new table
data_5 <- 
  mutate(
    data_5, 
    country_id = 
      if_else(user_location == "India", 1, 
        if_else(user_location == "Canada", 2, 
          if_else(user_location == "US", 3,
            if_else(user_location == "China", 4,
              if_else(user_location == "UK", 5, 0)
            )
          )
        )
      )
    )

# Remove user_location column from data_5
data_5 <- subset(data_5, select = -user_location)

# Add location's GDP info to locations table
# Data from: https://worldpopulationreview.com/countries/countries-by-gdp
countries_df <- 
  mutate(
    countries_df, 
    nominal_gdp_trillions = 
      if_else(country_id == 1, 2.72, 
              if_else(country_id == 2, 1.71, 
                      if_else(country_id == 3, 20.49,
                              if_else(country_id == 4, 13.4,
                                      if_else(country_id == 5, 2.83, 0)
                              )
                      )
              )
      )
  )

# Make sure tables look good
View(countries_df)
View(data_5)

# Top 10 user names with the most tweets
Q2 <- "SELECT user_name, COUNT(*) AS 'Number of tweets',
          user_location AS 'Country' FROM data_5
        JOIN countries_df ON 
          data_5.country_id = countries_df.country_id
        GROUP BY user_name 
        ORDER BY COUNT(*) DESC LIMIT 10"
sqldf(Q2)

# Average number of favorites by tweet by country
Q3 <- "SELECT user_location AS 'Country', AVG(favorites) AS 'Favorites by tweet' FROM data_5
        JOIN countries_df ON data_5.country_id = countries_df.country_id
        GROUP BY user_location
        ORDER BY AVG(favorites) DESC"
sqldf(Q3)

# Average number of tweets by user_name
Q4 <- "SELECT user_name, COUNT(*) AS 'Num_tweets', user_location AS 'Country' FROM data_5
        JOIN countries_df ON data_5.country_id = countries_df.country_id
        GROUP BY user_name"
username_num_tweets <- sqldf(Q4)

# Average number of tweets by user_name by country
# and number of users by country
Q5 <- "SELECT Country, AVG(Num_tweets) AS 'Tweets_per_user',
        COUNT(DISTINCT(user_name)) AS 'Total_users' FROM username_num_tweets
        GROUP BY Country ORDER BY AVG(Num_tweets) DESC"
sqldf(Q5)

# Run query based on how often the term antivax gets mentioned in a tweet
Q6 <- "SELECT COUNT(*) FROM data_5
        WHERE text LIKE '%antivax%' OR text LIKE '%anti-vax%'"
sqldf(Q6)

# Visualization of evolution of monthly number of tweets
class(data_5$date)

# Add new columns to the dataset to extract the year and month 
# of each tweet, and add a new column to add to the count of tweets
data_5 <- mutate(
    data_5,
    year = substr(date,1,4), month = substr(date,6,7)
  )

# Change December to be -12 so visualization is 
# in correct order
data_5$month[data_5$month == 12] <- -12


View(data_5)

# DID NOT WORK - attempt to create a new column of date format
# data_5 <- mutate(data_5, new_date = substr(1,10))
# head(data_5$new_date)
# as.Date(data_5$new_date, format("%y-%m-%d"))
# head(data_5$new_date)

data_5 %>% 
  group_by(year, month) %>%
  ggplot(aes(x = month)) +
  geom_bar(fill = "red") +
  ggtitle("Evolution of tweets per month (total)") +
  theme(plot.title = element_text(hjust = 0.5))

########## VISUALIZATION 1
# Evolution of tweets per month per country
# using facet_wrap, facet_grid

Q7 <- "SELECT user_location AS country, month, COUNT(*) AS total FROM data_5
      JOIN countries_df ON data_5.country_id = countries_df.country_id
      WHERE user_location != 'India'
      GROUP BY user_location, month"
Q7_df <- sqldf(Q7)
Q7_df %>% 
  ggplot(aes(month, total)) +
  geom_line(group = 1) +
  facet_wrap(. ~ country) +
  ggtitle("Evolution of tweets per month (per country)") +
  theme(plot.title = element_text(hjust = 0.5))

Q8 <- "SELECT user_location AS country, month, COUNT(*) AS total FROM data_5
      JOIN countries_df ON data_5.country_id = countries_df.country_id
      WHERE user_location = 'India'
      GROUP BY user_location, month"
Q8_df <- sqldf(Q8)
Q8_df %>% 
  ggplot(aes(month, total)) +
  geom_line(group=1) +
  ggtitle("Evolution of tweets per month in India") +
  theme(plot.title = element_text(hjust = 0.5))

########## VISUALIZATION 2
# Visualization of favorites per tweet combined with 
# average number of tweets by country (check queries)
# Q3, Q5
# goal is to see if there is a correlation between
# number of tweets and favorites per tweet
library(MASS)
favs <- sqldf(Q3)
tweets_count <- sqldf(Q5)
favs_tweetscount <- merge(favs, tweets_count)

favs_tweetscount %>%
  ggplot(aes(Total_users, Tweets_per_user)) +
  geom_point(aes(colour = Country)) +
  geom_smooth(method = "rlm") +
  ggtitle("Tweets Per User VS Total Users (per country)") +
  theme(plot.title = element_text(hjust = 0.5))

cor(favs_tweetscount$Tweets_per_user, favs_tweetscount$Total_users)

########## VISUALIZATION 3
# Friends by country
Q9 <- "SELECT user_location, user_name, user_friends FROM data_5
        JOIN countries_df ON data_5.country_id = countries_df.country_id
        GROUP BY user_location, user_name"
friends_bycountry <- sqldf(Q9)
friends_bycountry %>%
  ggplot(aes(user_location, user_friends)) +
  geom_boxplot() +
  scale_y_log10()

########## VISUALIZATION 4
# Relationship between ratio of count of user_verified and total_users
# and gdp

# Change user_verified to 1 if 'True' and to 0 if 'False'
v4_df <- data_5
v4_df$user_verified <- ifelse(v4_df$user_verified == 'True', 1, 0)
View(v4_df)

# Remove China because it is an outlier
Q10 <- "SELECT user_location AS 'country', nominal_gdp_trillions AS 'gdp',
        SUM(user_verified)/COUNT(*) AS 'ratio' FROM v4_df JOIN countries_df 
            ON v4_df.country_id = countries_df.country_id
        WHERE user_location != 'China' GROUP BY user_location"
gdp_count <- sqldf(Q10)
View(gdp_count)
gdp_count %>%
  ggplot(aes(gdp, ratio)) +
  geom_point(aes(colour = country)) +
  geom_smooth(method = 'rlm') +
  ggtitle("% of Verified Users VS GDP (per country)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "GDP", y = "Verified Users / Total Users")

########## VISUALIZATION 5
# Relationship between user_created year and user_followers
v5_data <- mutate(
  data_5,
  year_created = substr(user_created,1,4)
)
View(v5_data)
class(v5_data$user_followers)

Q11 <- "SELECT year_created, SUM(user_followers)/COUNT(*) AS 'ratio'
        FROM v5_data GROUP BY year_created"
v5_df <- sqldf(Q11)

v5_df %>%
  ggplot(aes(year_created, ratio)) +
  geom_line(group=1) +
  ggtitle("Ratio of Followers over Total Users (by year)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Year Created", y = "Followers / Users") +
  scale_y_log10()

########## VISUALIZATION 6
# Followers vs Retweets scatterplot
ggplot(data_5, aes(user_followers,retweets,color=user_verified)) +
  geom_point() +
  ggtitle("Followers VS Retweets") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_log10()

########## VISUALIZATION 7
# Histogram of total tweets per country
Q12 <- "SELECT user_location FROM data_5
        JOIN countries_df ON data_5.country_id = countries_df.country_id"
v7_df <- sqldf(Q12)
ggplot(v7_df, aes(x = user_location, fill = user_location)) + 
  geom_histogram(stat="count") +
  labs(x = "Country", y = "Total tweets") +
  ggtitle("Histogram of Total Tweets per Country") +
  theme(plot.title = element_text(hjust = 0.5))

