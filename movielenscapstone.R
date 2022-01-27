
# Setup -------------------------------------------------------------------


#Prep movielens dataset for analysis
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)
#Define function to get residual mean squared error of predictions
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


# Partition edx data ------------------------------------------------------

#Set seed to 1 and create partitions for tuning & training purposes
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)
rm(edx, test_index, temp, removed)


# Preliminary Data Exploration --------------------------------------------

library(lubridate)
first_half_star <- train_set %>% 
  filter(rating %in% c(0.5,1.5,2.5,3.5,4.5)) %>%
  pull(timestamp) %>%
  min()

# Build cumulative Naive, Movie, User, Genre, & Date Avg Models -----------

#Build Naive Avg model

#Find average movie rating, use this to predict all ratings, and save RMSE result
mu <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu)
rmse_results <- data.frame(model = "Naive average", RMSE = naive_rmse)

#Build Movie Avg model

#Sweep out overall average and find residual average for each individual movie
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

#use this + naive avg to predict ratings and save RMSE result
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred
movie_avg_rmse <- RMSE(test_set$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(model="Naive + movie average",
                                     RMSE = movie_avg_rmse ))
rmse_results %>% knitr::kable()

#Build User Avg model

#Sweep out overall and movie averages and find residual average for each user
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#use this + naive + movie avgs to predict ratings and save RMSE result
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
if(any(predicted_ratings > 5 | predicted_ratings < 1)){
  predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
}
user_avg_rmse <- RMSE(test_set$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(model="Naive + movie + user average",  
                                     RMSE = user_avg_rmse ))
rmse_results %>% knitr::kable()

#Build Genre Avg model

#Sweep out overall, movie, and user averages and find residual average for each 
#genre combination
genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u))

#use this + naive + movie + user avg to predict ratings and save RMSE result
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred
if(any(predicted_ratings > 5 | predicted_ratings < 1)){
  predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
}
genre_avg_rmse <- RMSE(test_set$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(model="Naive + movie + user + genre average",
                                     RMSE = genre_avg_rmse ))
rmse_results %>% knitr::kable()

#Build Date Avg model

library(lubridate)

#Tune optimal rounding  (week, month, or year) with 5-fold cross validation 
set.seed(1,sample.kind="Rounding")
folds <- createFolds(train_set$rating, k = 5, list = TRUE, returnTrain = FALSE)

date_avg_rmse <- map_dfr(c("week","month","year"),function(tuner){
  rmse <- map(1:5,function(curr_fold){
    #Create folds and make sure all rounded dates in tune_set are in tr_set
    tr_set <- train_set[-folds[[curr_fold]],] %>%
      mutate(date = round_date(as.Date(as_datetime(timestamp)),unit=tuner))
    temp <- train_set[folds[[curr_fold]],] %>%
      mutate(date = round_date(as.Date(as_datetime(timestamp)),unit=tuner))
    tune_set <- temp %>% 
      semi_join(tr_set, by = "date")
    removed <- suppressMessages(anti_join(temp,tune_set))
    tr_set <- rbind(tr_set, removed)
    
    #Sweep out overall, movie, user, and genre averages and find residual average 
    #for each date in the time series
    date_avgs <- tr_set %>% 
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genre_avgs, by='genres') %>%
      group_by(date) %>% 
      summarize(b_d = mean(rating - mu - b_i - b_u - b_g))
    
    #use this + naive + movie + user + genre avg to predict ratings
    predicted_ratings <- tune_set %>% 
      left_join(movie_avgs, by='movieId') %>%
      left_join(user_avgs, by='userId') %>%
      left_join(genre_avgs, by='genres') %>%
      left_join(date_avgs, by='date') %>%
      mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
      .$pred
    
    #disallow predictions < 0.5 or > 5.0
    if(any(predicted_ratings > 5 | predicted_ratings < 1)){
      predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
      predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
      predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
    }
    
    #Save rmse result
    RMSE(tune_set$rating,predicted_ratings)
  }) %>% unlist()
  date_avg_rmse <- data.frame(tuner = tuner,rmse = mean(rmse))
})

#Plot tuning results and get optimal tuning parameter
ggplot(date_avg_rmse,aes(x=fct_reorder(tuner,c(1,2,3)),y=rmse)) + 
  geom_point(shape=1) +
  xlab("rounding unit") +
  ggtitle("Tuning result for date bin size")
date_avg_rmse <- date_avg_rmse %>% filter(rmse == min(rmse))

#Add a rounded date variable to our train and test sets using the optimal tuner
train_set <- train_set %>% 
  mutate(date = round_date(as.Date(as_datetime(timestamp)),unit=date_avg_rmse$tuner[1]))
test_set <- test_set %>% 
  mutate(date = round_date(as.Date(as_datetime(timestamp)),unit=date_avg_rmse$tuner[1]))

#Use our optimal tuning parameter to predict on the test set and save rmse
date_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  group_by(date) %>% 
  summarize(b_d = mean(rating - mu - b_i - b_u - b_g))
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  left_join(date_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
  .$pred
if(any(predicted_ratings > 5 | predicted_ratings < 1)){
  predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
}
date_avg_rmse$rmse <- RMSE(test_set$rating,predicted_ratings)

rmse_results <- bind_rows(rmse_results,
                          data.frame(model=paste("Naive + movie + user + genre + date average"),
                                     RMSE = date_avg_rmse$rmse[1],
                                     tuning_param = date_avg_rmse$tuner[1]))
rmse_results %>% knitr::kable()


# Movie, User, Genre, & Date Avg Models with Regularization -------------------

#Now we will create a model that "regularizes" average ratings based on n() 
#(because average ratings based on a small n are less reliable).

#Regularization punishes large estimates that are based on a small sample size by
#shrinking (dividing) the estimates by some factor lambda. To do this, we sum
#the centered ratings and then divide by the number of ratings plus lambda.

#We try a series of different values for lambda to see which one minimizes RMSE

#Build Movie Avg model

#Set range of values to try for lambda
lambdas <- seq(0, 10, 0.25)

#Tune optimal lambda with 5-fold cross validation 
set.seed(2,sample.kind="Rounding")
folds <- createFolds(train_set$rating, k = 5, list = TRUE, returnTrain = FALSE)

reg_movie_rmses <- map_dfr(lambdas,function(l){
  rmse <- map(1:5,function(curr_fold){
    #Create folds and make sure all movie IDs in tune_set are in tr_set
    tr_set <- train_set[-folds[[curr_fold]],]
    temp <- train_set[folds[[curr_fold]],]
    tune_set <- temp %>% 
      semi_join(tr_set, by = "movieId")
    removed <- suppressMessages(anti_join(temp,tune_set))
    tr_set <- rbind(tr_set, removed)
    
    #Sweep out overall average rating and calculate sum of residual averages by
    #individual movie. Also count n in sample for each movie.
    #Divide sums by n + lambda to calculate regularized avg.
    reg_movie_avgs <- tr_set %>% 
      group_by(movieId) %>% 
      summarize(s = sum(rating - mu), n = n()) %>%
      mutate(b_i = s/(n+l))
    
    #Add regularized avg to naive avg to predict ratings on tune set
    predicted_ratings <- tune_set %>% 
      left_join(reg_movie_avgs, by='movieId') %>% 
      mutate(pred = mu + b_i) %>%
      .$pred
    
    #Disallow predictions < 0.5 or > 5.0
    if(any(predicted_ratings > 5 | predicted_ratings < 1)){
      predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
      predicted_ratings <- if_else(tune_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
      predicted_ratings <- if_else(tune_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
    }
    
    #Return rmse result
    RMSE(tune_set$rating,predicted_ratings)
  }) %>% unlist()
  reg_movie_rmses <- data.frame(lambda = l,rmse = mean(rmse))
})

#Plot tuning results
plot(reg_movie_rmses)

#Get optimal tuning parameter
reg_movie_rmses <- reg_movie_rmses %>%
  filter(rmse == min(rmse))

#Use our optimal tuning parameter to predict on the test set and save rmse
reg_movie_avgs <- train_set %>%
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n = n()) %>%
  mutate(b_i = s/(n+reg_movie_rmses$lambda[1]))
predicted_ratings <- test_set %>%
  left_join(reg_movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred
if(any(predicted_ratings > 5 | predicted_ratings < 1)){
  predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
}

#Save RMSE result
reg_movie_rmses$rmse <- RMSE(test_set$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(model="Reg. naive + movie average",
                                     RMSE = reg_movie_rmses$rmse[1],
                                     tuning_param = as.character(reg_movie_rmses$lambda[1])))
rmse_results %>% knitr::kable()


#Build User Avg model


#Set range of values to try for lambda
lambdas <- seq(0, 10, 0.25)

#Tune optimal lambda with 5-fold cross validation 
set.seed(3,sample.kind="Rounding")
folds <- createFolds(train_set$rating, k = 5, list = TRUE, returnTrain = FALSE)

reg_user_rmses <- map_dfr(lambdas,function(l){
  rmse <- map(1:5,function(curr_fold){
    #Create folds and make sure all user IDs in tune_set are in tr_set
    tr_set <- train_set[-folds[[curr_fold]],]
    temp <- train_set[folds[[curr_fold]],]
    tune_set <- temp %>% 
      semi_join(tr_set, by = "userId")
    removed <- suppressMessages(anti_join(temp,tune_set))
    tr_set <- rbind(tr_set, removed)
    
    #Sweep out overall & movie rating and calculate sum of residual averages by
    #individual user. Also count n in sample for each user.
    #Divide sums by n + lambda to calculate regularized avg.
    reg_user_avgs <- tr_set %>% 
      left_join(reg_movie_avgs, by='movieId') %>%
      group_by(userId) %>% 
      summarize(s = sum(rating - mu - b_i), n = n()) %>%
      mutate(b_u = s/(n+l))
    
    #Add regularized avg to naive avg to predict ratings on tune set
    predicted_ratings <- tune_set %>% 
      left_join(reg_movie_avgs, by='movieId') %>% 
      left_join(reg_user_avgs, by='userId') %>%
      mutate(pred = mu + b_i + b_u) %>%
      .$pred
    
    #Disallow predictions < 0.5 or > 5.0
    if(any(predicted_ratings > 5 | predicted_ratings < 1)){
      predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
      predicted_ratings <- if_else(tune_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
      predicted_ratings <- if_else(tune_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
    }
    
    #Return rmse result
    RMSE(tune_set$rating,predicted_ratings)
  }) %>% unlist()
  reg_user_rmses <- data.frame(lambda = l,rmse = mean(rmse))
})

#Plot tuning results
plot(reg_user_rmses)

#Get optimal tuning parameter
reg_user_rmses <- reg_user_rmses %>%
  filter(rmse == min(rmse))

#Predict ratings in the test set with our optimal tuning parameter
reg_user_avgs <- train_set %>%
  left_join(reg_movie_avgs, by='movieId') %>%
  group_by(userId) %>% 
  summarize(s = sum(rating - mu - b_i), n = n()) %>%
  mutate(b_u = s/(n+reg_user_rmses$lambda[1]))
predicted_ratings <- test_set %>% 
  left_join(reg_movie_avgs, by='movieId') %>% 
  left_join(reg_user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
if(any(predicted_ratings > 5 | predicted_ratings < 1)){
  predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
}

#Save RMSE result
reg_user_rmses$rmse <- RMSE(test_set$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(model="Reg. naive + movie + user average",
                                     RMSE = reg_user_rmses$rmse[1],
                                     tuning_param = as.character(reg_user_rmses$lambda[1])))
rmse_results %>% knitr::kable()


#Build Genre Avg model

#Set range of values to try for lambda
lambdas <- seq(0, 1000, 25)

#Tune optimal lambda with 5-fold cross validation 
set.seed(4,sample.kind="Rounding")
folds <- createFolds(train_set$rating, k = 5, list = TRUE, returnTrain = FALSE)

reg_genre_rmses <- map_dfr(lambdas,function(l){
  rmse <- map(1:5,function(curr_fold){
    #Create folds and make sure all genre combos in tune_set are in tr_set
    tr_set <- train_set[-folds[[curr_fold]],]
    temp <- train_set[folds[[curr_fold]],]
    tune_set <- temp %>% 
      semi_join(tr_set, by = "genres")
    removed <- suppressMessages(anti_join(temp,tune_set))
    tr_set <- rbind(tr_set, removed)
    
    #Sweep out overall, movie & user rating and calculate sum of residual averages 
    #by genre combo. Also count n in sample for each genre combo.
    #Divide sums by n + lambda to calculate regularized avg.
    reg_genre_avgs <- tr_set %>% 
      left_join(reg_movie_avgs, by='movieId') %>%
      left_join(reg_user_avgs, by='userId') %>%
      group_by(genres) %>% 
      summarize(s = sum(rating - mu - b_i - b_u), n = n()) %>%
      mutate(b_g = s/(n+l))
    
    #Add regularized avg to naive avg to predict ratings on tune set
    predicted_ratings <- tune_set %>% 
      left_join(reg_movie_avgs, by='movieId') %>% 
      left_join(reg_user_avgs, by='userId') %>%
      left_join(reg_genre_avgs, by='genres') %>%
      mutate(pred = mu + b_i + b_u + b_g) %>%
      .$pred
    
    #Disallow predictions < 0.5 or > 5.0
    if(any(predicted_ratings > 5 | predicted_ratings < 1)){
      predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
      predicted_ratings <- if_else(tune_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
      predicted_ratings <- if_else(tune_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
    }
    
    #Return rmse result
    RMSE(tune_set$rating,predicted_ratings)
  }) %>% unlist()
  reg_genre_rmses <- data.frame(lambda = l,rmse = mean(rmse))
})

#Plot tuning results
plot(reg_genre_rmses)

#Get optimal tuning parameter
reg_genre_rmses <- reg_genre_rmses %>%
  filter(rmse == min(rmse))

#Predict ratings in the test set with our optimal tuning parameter
reg_genre_avgs <- train_set %>%
  left_join(reg_movie_avgs, by='movieId') %>%
  left_join(reg_user_avgs, by='userId') %>%
  group_by(genres) %>% 
  summarize(s = sum(rating - mu - b_i - b_u), n = n()) %>%
  mutate(b_g = s/(n+reg_genre_rmses$lambda[1]))
predicted_ratings <- test_set %>% 
  left_join(reg_movie_avgs, by='movieId') %>% 
  left_join(reg_user_avgs, by='userId') %>%
  left_join(reg_genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred
if(any(predicted_ratings > 5 | predicted_ratings < 1)){
  predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
}

#Save RMSE result
reg_genre_rmses$rmse <- RMSE(test_set$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(model="Reg. naive + movie + user + genre average",
                                     RMSE = reg_genre_rmses$rmse[1],
                                     tuning_param = as.character(reg_genre_rmses$lambda[1])))
rmse_results %>% knitr::kable()


#Build Date Avg model

library(lubridate)

#Set range of values to try for lambda
lambdas <- seq(0, 5000, 100)

#Tune optimal lambda with 5-fold cross validation 
set.seed(5,sample.kind="Rounding")
folds <- createFolds(train_set$rating, k = 5, list = TRUE, returnTrain = FALSE)

reg_date_rmses <- map_dfr(lambdas,function(l){
  rmse <- map(1:5,function(curr_fold){
    #Create folds and make sure all dates in tune_set are in tr_set
    tr_set <- train_set[-folds[[curr_fold]],]
    temp <- train_set[folds[[curr_fold]],]
    tune_set <- temp %>% 
      semi_join(tr_set, by = "date")
    removed <- suppressMessages(anti_join(temp,tune_set))
    tr_set <- rbind(tr_set, removed)
    
    #Sweep out overall, movie, user, & genre rating and calculate sum of residual 
    #averages by genre combo. Also count n in sample for each genre combo.
    #Divide sums by n + lambda to calculate regularized avg.
    reg_date_avgs <- tr_set %>% 
      left_join(reg_movie_avgs, by='movieId') %>%
      left_join(reg_user_avgs, by='userId') %>%
      left_join(reg_genre_avgs, by='genres') %>%
      group_by(date) %>% 
      summarize(s = sum(rating - mu - b_i - b_u - b_g), n = n()) %>%
      mutate(b_d = s/(n+l))
    
    #Add regularized avg to naive avg to predict ratings on tune set
    predicted_ratings <- tune_set %>% 
      left_join(reg_movie_avgs, by='movieId') %>% 
      left_join(reg_user_avgs, by='userId') %>%
      left_join(reg_genre_avgs, by='genres') %>%
      left_join(reg_date_avgs, by='date') %>%
      mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
      .$pred
    
    #Disallow predictions < 0.5 or > 5.0
    if(any(predicted_ratings > 5 | predicted_ratings < 1)){
      predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
      predicted_ratings <- if_else(tune_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
      predicted_ratings <- if_else(tune_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
    }
    
    #Return rmse result
    RMSE(tune_set$rating,predicted_ratings)
  }) %>% unlist()
  reg_date_rmses <- data.frame(lambda = l,rmse = mean(rmse))
})

#Plot tuning results
ggplot(reg_date_rmses,aes(x=lambda,y=rmse)) + 
  geom_point(shape=1) +
  ggtitle("Tuning result for regularized date average correction factor")

#Get optimal tuning parameter
reg_date_rmses <- reg_date_rmses %>%
  filter(rmse == min(rmse))

#Predict ratings in the test set with our optimal tuning parameter
reg_date_avgs <- train_set %>%
  left_join(reg_movie_avgs, by='movieId') %>%
  left_join(reg_user_avgs, by='userId') %>%
  left_join(reg_genre_avgs, by='genres') %>%
  group_by(date) %>% 
  summarize(s = sum(rating - mu - b_i - b_u - b_g), n = n()) %>%
  mutate(b_d = s/(n+reg_date_rmses$lambda[1]))
predicted_ratings <- test_set %>% 
  left_join(reg_movie_avgs, by='movieId') %>% 
  left_join(reg_user_avgs, by='userId') %>%
  left_join(reg_genre_avgs, by='genres') %>%
  left_join(reg_date_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
  .$pred
if(any(predicted_ratings > 5 | predicted_ratings < 1)){
  predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
}

#Save RMSE result
reg_date_rmses$rmse <- RMSE(test_set$rating,predicted_ratings)
rmse_results <- bind_rows(rmse_results,
                          data.frame(model="Reg. naive + movie + user + genre + date average",
                                     RMSE = reg_date_rmses$rmse[1],
                                     tuning_param = as.character(reg_date_rmses$lambda[1])))
rmse_results %>% knitr::kable()

# Loess Smoothing -----------------------------------------------------

#For our continuous date variable, we will try smoothing with a loess model

#Train the model and tune span parameter using 10-fold cross-validation method
#included in the caret package
grid <- expand.grid(span=seq(0.05, 1, len = 10),degree=1)
train_loess <- train(b_d ~ date,
                     method="gamLoess",
                     tuneGrid=grid,
                     data=reg_date_avgs)

#Predict ratings using a combination of regularized averages and loess predictions
predicted_ratings <- test_set %>%
  left_join(reg_movie_avgs, by='movieId') %>%
  left_join(reg_user_avgs, by='userId') %>%
  left_join(reg_genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g + predict(train_loess,.)) %>%
  .$pred

#Disallow any predicted ratings < -.5 or > 5.0
if(any(predicted_ratings > 5 | predicted_ratings < 1)){
  predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
}

#Plot the loess model on our date residuals data
test_set %>%
  left_join(reg_movie_avgs, by='movieId') %>%
  left_join(reg_user_avgs, by='userId') %>%
  left_join(reg_genre_avgs, by='genres') %>%
  left_join(reg_date_avgs, by='date') %>%
  group_by(date) %>%
  summarize(b_d = first(b_d)) %>%
  mutate(smoother = predict(train_loess,.)) %>%
  ggplot(aes(x=date)) +
  geom_point(aes(y=b_d)) +
  geom_line(aes(y=smoother),col="red") +
  ggtitle("Weekly regularized averages vs. smoothed loess model") +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")

#Calculate and report RMSE
date_loess_rmse <- data.frame(rmse=RMSE(test_set$rating,predicted_ratings),
                              span=train_loess$bestTune[1])
data.frame(model="Reg. naive + movie + user + genre average + date loess model",
           RMSE = date_loess_rmse$rmse[1],
           tuning_param = as.character(date_loess_rmse$span[1])) %>% 
  knitr::kable(caption="Table 3")


# Validation --------------------------------------------------------------

#Add rounded date variable to validation set using optimal tuner
validation <- validation %>% 
  mutate(date = round_date(as.Date(as_datetime(timestamp)),unit=date_avg_rmse$tuner[1]))

#Predict ratings in the validation set with our regularization
reg_date_avgs <- train_set %>%
  left_join(reg_movie_avgs, by='movieId') %>%
  left_join(reg_user_avgs, by='userId') %>%
  left_join(reg_genre_avgs, by='genres') %>%
  group_by(date) %>% 
  summarize(s = sum(rating - mu - b_i - b_u - b_g), n = n()) %>%
  mutate(b_d = s/(n+reg_date_rmses$lambda[1]))
predicted_ratings <- validation %>% 
  left_join(reg_movie_avgs, by='movieId') %>% 
  left_join(reg_user_avgs, by='userId') %>%
  left_join(reg_genre_avgs, by='genres') %>%
  left_join(reg_date_avgs, by='date') %>%
  mutate(pred = mu + b_i + b_u + b_g + b_d) %>%
  .$pred
if(any(predicted_ratings > 5 | predicted_ratings < 1)){
  predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
  predicted_ratings <- if_else(test_set$timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
}

#Print RMSE result
RMSE(validation$rating,predicted_ratings)




# Regularized Model with Movie Age ----------------------------------------

train_set %>%
  mutate(release_year = str_extract(title,"\\(\\d{4}\\)")) %>%
  mutate(release_year = str_remove_all(release_year,"[\\(\\)]")) %>%
  mutate(release_year = as.numeric(release_year)) %>%
  left_join(reg_date_avgs, by='date') %>%
  left_join(reg_movie_avgs, by='movieId') %>%
  left_join(reg_user_avgs, by='userId') %>%
  left_join(reg_genre_avgs, by='genres') %>%
  mutate(age = year(date)-release_year) %>%
  mutate(log_age = round(log2(age)*4)/4) %>%
  group_by(log_age) %>%
  summarize(b_a = mean(rating - mu - b_i - b_u - b_g - b_d), n = n()) %>%
  ggplot(aes(x=log_age,y=b_a)) +
  geom_hline(yintercept=0,linetype="dashed",col="red") +
  geom_point(aes(size=n)) +
  geom_smooth(method="loess") +
  ylab("residual average rating") +
  xlab("age of movie at time of rating") +
  ggtitle("Age of movie at time of rating vs. residual average rating") +
  scale_x_continuous(limits=c(0,8),breaks=c(0:7),labels = 2^(0:7))

# #Build regularized age of movie model
# #Perhaps users like brand new movies more than old ones, or classics more than
# #new releases. Define a variable that subtracts the release year from the year
# #in the review time stamp, and try using this as a predictor.
# #Note: movie ages range from -2 years to 93 years
# 
# 
# #Could I use a movie's number of ratings (an indicator of popularity) to refine my prediction of its rating? This approach runs into problems of scaling. I finally hit upon a winner when I try using the age of the rated movie at the time of rating. On the theory that users, on average, will prefer either older or newer movies, I subtract the release year from the year of rating to get movie age in years, and then find the regularized average rating by year. When I add this into my regularized model, RMSE improves very significantly.
# 
# library(lubridate)
# 
# #Sweep out overall, movie, user, genre, date, and n of ratings averages and
# #find regularized residual average for each age of movie & use this + naive +
# #movie + user + genre + date + n of ratings avg to predict ratings and save
# #RMSE result
# lambdas <- seq(0, 200, by = 10)
# just_the_sum <- train_set %>%
#   mutate(release_year = str_extract(title,"\\(\\d{4}\\)")) %>%
#   mutate(release_year = str_remove_all(release_year,"[\\(\\)]")) %>%
#   mutate(release_year = as.numeric(release_year)) %>%
#   mutate(date = round_date(as.Date(as_datetime(timestamp)),unit=date_avg_rmse$tuner[1])) %>%
#   left_join(reg_date_avgs, by='date') %>%
#   left_join(n_per_year, by='movieId') %>%
#   left_join(reg_movie_avgs, by='movieId') %>%
#   left_join(reg_user_avgs, by='userId') %>%
#   left_join(reg_genre_avgs, by='genres') %>%
#   mutate(n_per_year = round(n_per_year/100)*100) %>%
#   left_join(reg_n_avgs, by='n_per_year') %>%
#   mutate(age = year(date)-release_year) %>%
#   group_by(age) %>%
#   summarize(s = sum(rating - mu - b_i - b_u - b_g - b_d - b_n), n = n())
# range(just_the_sum$age)
# 
# reg_age_rmses <- map_dfr(lambdas, function(l){
#   predicted_ratings <- test_set %>%
#     mutate(release_year = str_extract(title,"\\(\\d{4}\\)")) %>%
#     mutate(release_year = str_remove_all(release_year,"[\\(\\)]")) %>%
#     mutate(release_year = as.numeric(release_year)) %>%
#     mutate(date = round_date(as.Date(as_datetime(timestamp)),unit=date_avg_rmse$tuner[1])) %>%
#     left_join(reg_date_avgs, by='date') %>%
#     left_join(n_per_year, by='movieId') %>%
#     left_join(reg_movie_avgs, by='movieId') %>%
#     left_join(reg_user_avgs, by='userId') %>%
#     left_join(reg_genre_avgs, by='genres') %>%
#     mutate(n_per_year = round(n_per_year/100)*100) %>%
#     left_join(reg_n_avgs, by='n_per_year') %>%
#     mutate(age = year(date)-release_year) %>%
#     left_join(just_the_sum, by='age') %>%
#     mutate(b_a = s/(n+l)) %>%
#     mutate(pred = mu + b_i + b_u + b_g + b_d + b_n + b_a) %>%
#     .$pred
#   if(any(predicted_ratings > 5 | predicted_ratings < 1)){
#     predicted_ratings <- if_else(predicted_ratings > 5,5,predicted_ratings)
#     predicted_ratings <- if_else(timestamp < first_half_star & predicted_ratings < 1,1,predicted_ratings)
#     predicted_ratings <- if_else(timestamp >= first_half_star & predicted_ratings < 0.5,0.5,predicted_ratings)
#   }
#   data.frame(lambda = l,rmse = RMSE(test_set$rating,predicted_ratings))
# })
# plot(reg_age_rmses)
# reg_age_rmses <- reg_age_rmses %>%
#   filter(rmse == min(rmse))
# 
# reg_age_avgs <- train_set %>%
#   mutate(release_year = str_extract(title,"\\(\\d{4}\\)")) %>%
#   mutate(release_year = str_remove_all(release_year,"[\\(\\)]")) %>%
#   mutate(release_year = as.numeric(release_year)) %>%
#   mutate(date = round_date(as.Date(as_datetime(timestamp)),unit=date_avg_rmse$tuner[1])) %>%
#   left_join(reg_date_avgs, by='date') %>%
#   left_join(n_per_year, by='movieId') %>%
#   left_join(reg_movie_avgs, by='movieId') %>%
#   left_join(reg_user_avgs, by='userId') %>%
#   left_join(reg_genre_avgs, by='genres') %>%
#   mutate(n_per_year = round(n_per_year/100)*100) %>%
#   left_join(reg_n_avgs, by='n_per_year') %>%
#   mutate(age = year(date)-release_year) %>%
#   left_join(just_the_sum, by='age') %>%
#   group_by(age) %>%
#   summarize(b_a = first(s)/(first(n)+reg_age_rmses$lambda[1]))
# 
# rmse_results <- bind_rows(rmse_results,
#                           data.frame(model=paste("Reg. naive + movie + user + genre +",date_avg_rmse$tuner[1],"+ n of ratings + age average"),
#                                      RMSE = reg_age_rmses$rmse[1],
#                                      tuning_param = as.character(reg_age_rmses$lambda[1])))
# rmse_results %>% knitr::kable()