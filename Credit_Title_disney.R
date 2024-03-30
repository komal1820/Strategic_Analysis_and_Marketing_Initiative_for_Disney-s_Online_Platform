library(tidyverse)
library(lubridate)
library(ggpubr)

disney_credits <- read_csv("credits.csv")
disney_titles <- read_csv("titles.csv")
titles <- disney_titles %>%
  mutate(release_year = make_date(release_year),
         year = year(release_year),
         genres = gsub("\\[","",genres),
         genres = gsub("\\]", "", genres),
         genres = gsub("\\'","", genres),
         production_countries = gsub("\\[","",production_countries),
         production_countries = gsub("\\]", "", production_countries),
         production_countries = gsub("\\'","", production_countries)) %>%
  separate(genres, c("genre1", "genre2", "genre3", "genre4", "genre5"), sep = ",") %>%
  separate(production_countries, c("country1", "country2"), sep = ",") 
# not all genres were separated, 
#some were excluded, because I wanted to focus on the main genres.

titles %>%
  filter(type == "MOVIE") %>%
  ggplot(aes(year)) +
  geom_histogram(bins = 50) +
  theme_minimal() +
  labs(x = "Release year") +
  guides(fill = "none") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1920, 2022, 5))

#On 2009, Disney acquired Hulu and Marvel which may have led to this sudden 
#increase in movies released annually.On 2019, Disney+ started, leading to 
#even more movies being released.

#How ratings were affected?
titles %>%
  group_by(release_year) %>%
  filter(type == "MOVIE") %>%
  filter(n()>20) %>% #selecting only years that had more than 20 movies releases (that's a lot!)
  filter(!is.na(imdb_score)) %>%
  ggplot(aes(imdb_score, fill = as.factor(release_year)))+
  geom_density()+
  facet_wrap(~as.factor(year(release_year)))+
  theme_bw()+
  theme(legend.position = "none") +
  labs(x = "IMDB Score")

#For years that produced more than 20 movies (it includes short stories), it seems that the ratings 
#from IMDB congregate around 6.5-7.5 (which is pretty good).
#Disney, however, produces many types of movies genres. 
#Could the public be partial for a movie genre? Is there any 
#indication that Disney excels in a specific genre? Let's plot the data according to the genre.

name <- titles %>% #creating a new file, with the counts of movies per genre
  group_by(release_year, genre1, type) %>%
  filter(type == "MOVIE") %>%
  filter(genre1 != "") %>%
  filter(!is.na(imdb_score)) %>%
  summarise(count = n(),
            mean_rating = mean(imdb_score))

titles %>%
  group_by(release_year, genre1, type) %>%
  filter(type == "MOVIE") %>%
  filter(genre1 != "") %>%
  filter(!is.na(imdb_score)) %>%
  ggplot(aes(release_year, imdb_score)) +
  geom_point(name, mapping = aes(release_year, mean_rating, size = count), shape = 21)+ #adding the number of movies released per genre
  geom_smooth(method = "lm", se = FALSE, aes(colour = as.factor(genre1))) +
  facet_wrap(~genre1) +
  labs(x = "Release year", y = "Average of IMDB ratings")+
  scale_size_continuous(name = "Number of movies released")+
  theme_bw()+
  guides(colour = "none", alpha = "none") +
  theme(legend.position = "bottom")

#In the graphs above, you can see that Disney started releasing a lot of sci-fi, 
#fantasy and comedy movies in the past 10 years which did not affect their ratings. 
#It appears that the animation genre has stable ratings over the years.
#One genre which is increasing its ratings and number of movies released is the 
#action genre. While romantic and family movies are on a decline in both their ratings and number of movies released. It is interesting to notice that Disney started releasing a lot of "documentation" (i.e. documentaries) movies, if you check the data you'll notice that they are not nature documentaries, but movies about behind-the-scenes and making-off.


big_movies <- titles %>% #new filtered dataset for only movies with runtime of at least 90'
  filter(type == "MOVIE") %>%
  filter(runtime >= 90) %>%
  select(title, id, type, release_year, imdb_score, imdb_votes, tmdb_popularity, 
         tmdb_score, runtime, genre1, genre2)

count_big_movies <- big_movies %>% #dataset with number of movies released per year of animation and fantasy genre
  group_by(release_year)%>%
  filter(genre1 %in% c("fantasy", "animation")) %>%
  filter(!is.na(imdb_score)) %>%
  filter(year(release_year) %in% c(2005:2022)) %>%
  summarise(count = n(),
            mean = mean(imdb_score))
#boxplots of movies released in recent years to see 
#if their ratings were affected by the number of movies released per year.

big_movies %>%
  group_by(release_year) %>%
  filter(!is.na(imdb_score)) %>%
  filter(genre1 %in% c("animation", "fantasy")) %>%
  mutate(mean_rating = mean(imdb_score),
         voters = sum(imdb_votes)) %>%
  filter(year(release_year) %in% c(2005:2022)) %>% #filtering more recent years
  ungroup()%>%
  ggplot(aes(release_year, imdb_score, group = release_year))+
  geom_boxplot(aes(fill = mean_rating), alpha = 0.5)+
  coord_flip() +
  scale_fill_viridis_c(name = "Average rating", breaks=seq(5,9,0.5), 
                       option = "B")+
  labs(x = "Release Year", y = "IMDB Score") +
  theme_bw() +
  guides(alpha = "none") +
  scale_x_date(date_breaks = "1 year",  date_labels = "%Y")+
  scale_size_continuous(name = 'Number of movies released') +
  geom_point(count_big_movies, mapping = aes(release_year, mean, 
                                             size = count), shape = 21)
# To check if the directors affect the ratings.
titles_credits <- big_movies %>% #joining the datasets
  inner_join(disney_credits, by = "id")

directors_movie_count <- titles_credits %>%
  filter(role == "DIRECTOR") %>%
  filter(!is.na(imdb_score)) %>%
  group_by(name) %>%
  filter(n() > 2) %>% #filtering directors that made more than 2 movies
  summarise(mean_rating = mean(imdb_score),
            voters = sum(imdb_votes),
            count = n())

titles_credits %>%
  filter(role == "DIRECTOR") %>%
  filter(!is.na(imdb_score)) %>%
  group_by(name) %>%
  filter(n() > 2) %>%
  mutate(mean_rating = mean(imdb_score),
         voters = sum(imdb_votes)) %>%
  ggplot(aes(release_year, imdb_score, group = name)) +
  geom_point(aes(colour = imdb_score), size = 1, alpha = 0.8) +
  geom_line(aes(colour = imdb_score), size = 0.1) +
  scale_color_viridis_c(name = "Average rating", breaks=seq(3.5,8.5,0.5), option = "B")+
  scale_x_date(date_labels = "'%y")+
  labs(x = "Release year", y = "IMDB Score") +
  facet_wrap(~name)+
  theme_bw()

#The director seem to have an influence in the ratings, however this can also 
#be said that perhaps some movies ideas are better liked by the public, 
#thus leading to higher ratings.
