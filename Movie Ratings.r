
# Download the movie ratings and IMDB movie titles from the sample datasets collection
library("AzureML")
ws <- workspace()
ratings <- download.datasets(ws, "Movie Ratings")
titles <- download.datasets(ws,"IMDB Movie Titles")

# Examine the first few rows in each dataset
head(ratings) 
head(titles)

# Merge the two datasets over the movie id
movieData <- merge(ratings, titles, by.x = "MovieId", by.y = "Movie.ID")
head(movieData)

# What is the average rating for each movie?
# install.packages("dplyr")
library(dplyr)

select(movieData, MovieId, Movie.Name, Rating) %>%
  group_by(Movie.Name) %>%
  summarize("MeanRating" = mean(Rating)) %>%
  arrange(desc(MeanRating))

# Install the lubridate and xts packages
install.packages("lubridate")
library(lubridate)

install.packages("xts")
library(xts)

# Step 1: Convert the dataset into an xts object (the Timestamp is a POSIXct datetime)
xtsMovieData <- xts(movieData[c("MovieId", "UserId", "Rating", "Movie.Name")], order.by=as.POSIXct(movieData$Timestamp, origin = "1970-01-01"))
head(xtsMovieData)

# Step 2: Count the number of ratings posted per day
ratingsPerDay <- apply.daily(xtsMovieData, function(data) { nrow(data$Rating) })
head(ratingsPerDay)


# Install the broom and ggplot2 packages
install.packages("broom")
library(broom)

install.packages("ggplot2")
library(ggplot2)

# Step 3: Graph the results
graphData <- tidy(ratingsPerDay)
graphData$index <- as.POSIXct(graphData$index,  origin="1970-01-01")

options(repr.plot.width=15, repr.plot.height=4)
ggplot(graphData, aes(x = index, y = value)) +
  ggtitle("Ratings Posted Per Day") +
  xlab("Date") +
  ylab("Number of Ratings") +
  geom_line(color = "blue")

