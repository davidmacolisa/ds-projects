#======================================================================================================================#
# Recommender engine for movies
# Davidmac Olisa Ekeocha
# August 23, 2024
#======================================================================================================================#
# Packages
#======================================================================================================================#
library(recommenderlab)
library(ggplot2)
library(dplyr)
library(ggplot2)
#======================================================================================================================#
# Importing data
#======================================================================================================================#
data("MovieLense", package = "recommenderlab")
MovieLenseMeta
MovieLenseUser
#======================================================================================================================#
# Convert matri to vector and remove zero rated movies
#======================================================================================================================#
vector_ratings <- as.vector(MovieLense@data)
sort(unique(vector_ratings))

vector_ratings <- vector_ratings[vector_ratings != 0]
table(vector_ratings)
qplot(vector_ratings)
#======================================================================================================================#
# Counts the number of ratings each movie has received.
#======================================================================================================================#
views_per_movie <- colCounts(MovieLense)
table_views <- data.frame(movie_name = names(views_per_movie), views = views_per_movie)
head(table_views)
#======================================================================================================================#
# Plot the most viewed movies
#======================================================================================================================#
table_views <- table_views %>% arrange(desc(views))
head(table_views)

ggplot(table_views[1:5,], aes(x = movie_name, y = views)) + geom_bar(stat = "identity")
ggplot(table_views[1:5,], aes(x = movie_name, y = views, fill = movie_name)) + geom_bar(stat = "identity")
ggplot(table_views[1:5,], aes(x = movie_name, y = views, fill = views)) + geom_bar(stat = "identity")
#======================================================================================================================#
# Calculates the average rating for each movie, and plot their distribution
#======================================================================================================================#
average_rating <- colMeans(MovieLense) %>% round(digits = 0)
head(average_rating)
qplot(average_rating) + ggtitle(label = "Distribution of average rating")
#======================================================================================================================#
# Filters the dataset to include only users who have rated more than 50 movies and movies with more than 100 ratings
# Split into train and test with a 90% / 10% split ratio
#======================================================================================================================#
rating_movies <- MovieLense[rowCounts(MovieLense) > 50, colCounts(MovieLense) > 100]
split_movie <- sample(x = c(T, F), size = nrow(rating_movies), replace = T, prob = c(0.9, 0.2))
recc_train <- rating_movies[split_movie,]
recc_test <- rating_movies[!split_movie,]
#======================================================================================================================#
# User-Based Collaborative Filtering (UBCF)
#======================================================================================================================#
recc_model_ubcf <- Recommender(data = recc_train, method = "UBCF")
n_recommended_ubcf <- 6

# Predict
recc_predicted_ubcf <- predict(object = recc_model_ubcf, newdata = recc_test, n = n_recommended_ubcf)
recc_predicted_ubcf

user1_movie_numbers <- recc_predicted_ubcf@items[[1]]
user1_movie_numbers
recc_predicted_ubcf@itemLabels[user1_movie_numbers]

user5_movie_numbers <- recc_predicted_ubcf@items[[5]]
recc_predicted_ubcf@itemLabels[user5_movie_numbers]
#======================================================================================================================#
# Item-Based Collaborative Filtering (IBCF)
#======================================================================================================================#
recc_model_ibcf <- Recommender(data = recc_train, method = "IBCF")
n_recommended_ibcf <- 6
recc_predicted_ibcf <- predict(object = recc_model_ibcf, newdata = recc_test, n = n_recommended_ibcf)

user1_movie_numbers <- recc_predicted_ibcf@items[[1]]
recc_predicted_ibcf@itemLabels[user1_movie_numbers]

user5_movie_numbers <- recc_predicted_ibcf@items[[5]]
recc_predicted_ibcf@itemLabels[user5_movie_numbers]
#======================================================================================================================#