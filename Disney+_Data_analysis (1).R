library(tidyverse)
library(here)
library(janitor)
library(skimr)
disney_data <- read_csv("disney_movies_tg.csv")

#exploring data
nrow(disney_data)
ncol(disney_data)
n_distinct(disney_data)
colnames(disney_data)

#checking for missing and null values in the dataset
disney_data %>% is.na()
disney_data %>% is.na() %>% sum()

disney_data$mpaa_rating %>% is.na() %>% sum()

disney_data %>% is_null()
disney_data %>% is_null() %>% sum()

str(disney_data)

skim_without_charts(disney_data)

#Statistical summary of the dataset
summary(disney_data)

#Data visualization
disney_data %>%
  group_by(genre) %>%
  summarise(total_gross = sum(total_gross)) %>%
  ggplot() + geom_col(mapping = aes(x=genre, y=total_gross)) + labs(title = "Total Gross by Genre", x="Genre", y="Total Gross") + theme(axis.text.x = element_text(face = "bold", size = 6, angle = 90))

disney_data %>%
  ggplot() + geom_bar(mapping = aes(y=genre)) + labs(title = "Movie Genre Count", y="Genre", x="Count")

#The adventure genre has brought in the most money over time for Disney, 
#but they have produced more comedy over the same period of time and next is Drama.

disney_data <- na.omit(disney_data)
disney_data$total_gross <- as.numeric(disney_data$total_gross)

#Ratings and total gross
disney_data %>%
  group_by(mpaa_rating) %>%
  summarise(total_rating_gross = sum(total_gross)) %>%
  ggplot() + geom_col(mapping = aes(x=mpaa_rating, y=total_rating_gross)) + labs(title = "Total Gross by Ratings", x="mpaa rating", y="Total Gross")

disney_data %>%
  group_by(mpaa_rating) %>%
  summarise(total_rating_gross = sum(inflation_adjusted_gross)) %>%
  ggplot() + geom_col(mapping = aes(x=mpaa_rating, y=total_rating_gross)) + labs(title = "Total Inflation Adjusted Gross by Ratings", x="mpaa rating", y="Inflation Adjusted Gross")

disney_data %>%
  ggplot() + geom_bar(mapping = aes(x=mpaa_rating)) + labs(title = "MPAA Rating Count", x="mpaa rating", y="Count")

#PG films have made the most in total gross, but when inflation is adjusted 
#for these films G rating has been the biggest grossing for Disney. There have been more PG films made than any other for the company.

#Release date by year
disney_data %>%
  separate(release_date, c("year", "month", "day"), sep = "-") %>%
  group_by(year) %>%
  summarise(total_gross = sum(total_gross)) %>%
  ggplot() + geom_line(mapping = aes(x=year, y=total_gross, group=1)) + theme(axis.text.x = element_text(face = "bold", size = 6, angle = 90)) + labs(title="Total Gross Over Time", x="Year", y="Total Gross")

disney_data %>%
  separate(release_date, c("year", "month", "day"), sep = "-") %>%
  group_by(year) %>%
  summarise(inflated_adjusted_gross = sum(inflation_adjusted_gross)) %>%
  ggplot() + geom_line(mapping = aes(x=year, y=inflated_adjusted_gross, group=1)) + theme(axis.text.x = element_text(face = "bold", size=6, angle = 90)) + labs(title = "Total Inflation Adjusted Gross Over Time", x="Year", y="Total Inflation Adjusted Gross")

#Over the years, the total gross has consistently increased annually. 
#However, upon adjusting for inflation, it becomes evident that there was an 
#initial peak, succeeded by a notable decline, and then a gradual recovery.

#Release date by month
disney_data %>%
  separate(release_date, c("year", "month", "day"), sep = "-") %>%
  group_by(month) %>%
  count(month) %>%
  ggplot() + geom_col(mapping = aes(x=month, y=n)) + labs(title = "Releases by Month", x="Month", y="Count")

# we could find that more movies are released during the holiday season.
#Releases are in March that is during the Spring and November aroud the Thanksgiving vacation

# the least grossing films
disney_data %>%
  arrange(total_gross) %>%
  head(10) %>%
  ggplot() + geom_col(mapping=aes(y=movie_title, x=total_gross)) +
  labs(title="Top 10 Least Grossing Films", y="Movie Titles", x="Total Gross")

#based on inflation-adjusted gross to get the least grossing films
disney_data %>%
  arrange(inflation_adjusted_gross) %>%
  head(10) %>%
  ggplot() + geom_col(mapping = aes(x=inflation_adjusted_gross, y=movie_title)) +
  labs(title = "Top 10 Least Grossing Films with Adjusted Inflation", x= "Inflation Adjusted Gross", y="Movie Titles")

