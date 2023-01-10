###########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(knitr)) install.packages("knitr")
if(!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)
library(knitr)
library(reshape2)
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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
#Create csv files with the new dataframes:
#write.csv(edx, "edx.csv", row.names = F)
#write.csv(validation, "validation.csv", row.names=F)
#upload the created dataframes:
#edx<- read.csv("edx.csv")
#validation<- read.csv("validation.csv")

#*********************************Release date **********************************
#number of rating distribution per year 
edx%>%mutate(rating_year=year(as_datetime(timestamp)))%>%group_by(rating_year)%>%ggplot(aes(rating_year))+
  geom_bar()
###Stock Market crash 1997 ??? thepeoplehistory.com
edx%>%mutate(rating_year=year(as_datetime(timestamp)))%>%filter(rating_year==1996:1998)%>%ggplot(aes(rating))+
  geom_bar()
#mean rating per year
edx%>%mutate(rating_year=year(as_datetime(timestamp)))%>%group_by(rating_year)%>%summarise(r=mean(rating))%>%
  ggplot(aes(rating_year,r))+
  geom_point()+
  geom_smooth()
#number of rating distribution per month
edx%>%mutate(rating_month=month(as_datetime(timestamp)))%>%group_by(rating_month)%>%ggplot(aes(rating_month))+
  geom_bar()
#mean rating per month
edx%>%mutate(rating_month=month(as_datetime(timestamp)))%>%group_by(rating_month)%>%summarise(r=mean(rating))%>%
  ggplot(aes(rating_month,r))+
  geom_point()+
  geom_smooth()
#number of rating distribution per week
edx%>%mutate(rating_week=week(as_datetime(timestamp)))%>%group_by(rating_week)%>%ggplot(aes(rating_week))+
  geom_bar()
#mean rating per week
edx%>%mutate(rating_week=week(as_datetime(timestamp)))%>%group_by(rating_week)%>%summarise(r=mean(rating))%>%
  ggplot(aes(rating_week,r))+
  geom_point()+
  geom_smooth()

#Add a time column to both edx and validation tables
edx<-edx%>%mutate(time_stamp=as_date(as_datetime(timestamp)))%>%arrange(time_stamp)%>%
  group_by(movieId)%>%mutate(first_rating_time=first(time_stamp))%>%ungroup()%>%
  mutate(rating_time=round(difftime(time_stamp,first_rating_time, units="weeks"))+1)
validation<-validation%>%mutate(time_stamp=as_date(as_datetime(timestamp)))%>%arrange(time_stamp)%>%
  group_by(movieId)%>%mutate(first_rating_time=first(time_stamp))%>%ungroup()%>%
  mutate(rating_time=round(difftime(time_stamp,first_rating_time, units="weeks"))+1)
##########
edx%>%group_by(userId)%>%ggplot(aes(userId))+
  geom_bar()+
  xlab("User ID")+
  ylab("Number of ratings")+
  ggtitle("User rating distribution")

#************Seperate edx into train and test sets**********************************
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test_set set are also in train_set set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test_set set back into train_set set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)



#************DATA EXPLORATION:******************************************#
train_set%>%group_by(movieId)%>%summarise(n=n(),r=mean(rating))
train_set%>%group_by(movieId)%>%summarise(n_ratings=n(),average_rating=mean(rating))%>%arrange(average_rating)%>%head()
train_set%>%group_by(movieId)%>%summarise(n_ratings=n(),average_rating=mean(rating))%>%arrange(average_rating)%>%tail()

str(edx)
dim(edx)
####table exploration
head(edx)
####number of distinct movies
n_distinct(edx$movieId)
####number of distict users
n_distinct(edx$userId)

# number of films per genre
genre<- unique(str_extract_all(unique(edx$genres),'[A-Z]+([a-z]+\\-*[A-Za-z]*)',simplify = T))
genre<-genre[genre!=""]
genres<-unique(genre)

count<-sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

sort(count)[-1:-8]
#rating distribution between the users: 
edx%>%group_by(userId)%>%filter(n()>100)%>%summarise(mu=round(mean(rating),1),n=n(),sd=sd(rating))%>%ggplot(aes(mu))+
  geom_bar()
#rating distribution between movies:
edx%>%group_by(movieId)%>%filter(n()>100)%>%summarise(mu=round(mean(rating),1),n=n(),sd=sd(rating))%>%ggplot(aes(mu))+
  geom_bar()


#**********************************Machine Learning ALgorithm***********************
####Average rating
mu_hat<- mean(train_set$rating)
#naive_rmse
naive_rmse<-RMSE(mu_hat,test_set$rating)
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

#movie effect
movie_avg<-train_set%>%group_by(movieId)%>%summarise(b_i=mean(rating-mu_hat)) 
qplot(b_i, data=movie_avg)
prediction_movie<- mu_hat+test_set%>%left_join(movie_avg, by="movieId")%>%pull(b_i)
RMSE(prediction_movie, test_set$rating)
#user effect
user_avg<-train_set%>%group_by(userId)%>%summarise(b_u=mean(rating-mu_hat)) 
qplot(b_u, data=user_avg)
prediction_user<- mu_hat+test_set%>%left_join(user_avg, by="userId")%>%pull(b_u)
RMSE(prediction_user, test_set$rating)
#Time effect
time_avg<-train_set%>%group_by(rating_time)%>%summarise(b_t=mean(rating-mu_hat)) 
qplot(b_t, data=time_avg)
prediction_time<- mu_hat+test_set%>%left_join(time_avg, by="rating_time")%>%pull(b_t)
RMSE(prediction_time, test_set$rating)

#user+movie effect
um_avg<-train_set%>%left_join(movie_avg, by="movieId")%>%group_by(userId)%>%summarise(b_u=mean(rating-mu_hat-b_i))
predicted_um<-test_set%>%left_join(movie_avg, by="movieId")%>%left_join(um_avg,by="userId")%>%mutate(p=mu_hat+b_i+b_u)%>%pull(p)
RMSE(predicted_um,test_set$rating)
#user+movie+time effect
umt_avg<-train_set%>%left_join(movie_avg, by="movieId")%>%left_join(user_avg, by = "userId")%>%group_by(rating_time)%>%summarise(b_t=mean(rating-mu_hat-b_u-b_i))
predicted_umt<-test_set%>%left_join(movie_avg, by="movieId")%>%left_join(user_avg,by="userId")%>%left_join(umt_avg, by="rating_time")%>%mutate(p=mu_hat+b_i+b_u+b_t)%>%pull(p)
RMSE(predicted_umt,test_set$rating)

#************Residual***********************************************
test_set %>% 
  left_join(movie_avg, by='movieId') %>%
  mutate(residual = rating - (mu_hat + b_i)) %>%arrange(desc(abs(residual)))%>%
  arrange(desc(abs(residual)))%>%pull(title)
title<-edx%>%select(movieId,title)%>%distinct()
movie_avg%>%left_join(title, by="movieId")%>%arrange(desc(b_i))

#rating frequency of the best/worse rated films
train_set%>%count(movieId)%>%left_join(movie_avg, by="movieId")%>%
  left_join(title, by="movieId")%>%arrange(desc(b_i))
train_set%>%count(movieId)%>%left_join(movie_avg, by="movieId")%>%
  left_join(title, by="movieId")%>%arrange(b_i)

residual_sum<-train_set%>%group_by(movieId)%>%summarise(s=sum(rating-mu_hat), n=n())
#testing the tuning parameter
  #Tuning user effect
lambda<-seq(0,10,.25)
rmses<-sapply(lambda, function(L){
  prediction<-test_set%>%left_join(residual_sum, by="movieId")%>%mutate(b_i=s/(n+L))%>%
    mutate(p=mu_hat+b_i)%>%pull(p)
  return(RMSE(prediction, test_set$rating))
})
qplot(lambda,rmses)
L<-lambda[which.min(rmses)]
  #Tuning user/movie/time effect
lambda2<-seq(0,10,.25)
rmse_user_movie<-sapply(lambda2, function(L){
  b_i<- train_set%>%group_by(movieId)%>%summarise(b_i=sum(rating-mu_hat)/(n()+L))
  b_u<- train_set%>%left_join(b_i, by="movieId")%>%group_by(userId)%>%summarise(b_u=sum(rating-mu_hat-b_i)/(n()+L))
  b_t<- train_set%>%left_join(b_i, by="movieId")%>%left_join(b_u, by="userId")%>%group_by(rating_time)%>%summarise(b_t=sum(rating-mu_hat-b_i-b_u)/(n()+L))
  prediction<-test_set%>%left_join(b_i, by="movieId")%>%left_join(b_u, by="userId")%>%left_join(b_t, by="rating_time")%>%
    mutate(p=mu_hat+b_i+b_u+b_t)%>%pull(p)
  return(RMSE(prediction, test_set$rating))
})
qplot(lambda2,rmse_user_movie)
l<-lambda2[which.min(rmse_user_movie)] #chosen parameter
  tibble(lambda2, rmse_user_movie)%>%ggplot(aes(lambda2, rmse_user_movie))+
    geom_point()+
    xlab("lambda")+
    ylab("RMSE")
  #Tuned user/movie/time effect

  b_i_t<- train_set%>%group_by(movieId)%>%summarise(b_i=sum(rating-mu_hat)/(n()+l))
  b_u_t<- train_set%>%left_join(b_i_t, by="movieId")%>%group_by(userId)%>%summarise(b_u=sum(rating-mu_hat-b_i)/(n()+l))
  b_t_t<- train_set%>%left_join(b_i_t, by="movieId")%>%left_join(b_u_t, by="userId")%>%group_by(rating_time)%>%summarise(b_t=sum(rating-mu_hat-b_i-b_u)/(n()+l))
  prediction_t<-test_set%>%left_join(b_i_t, by="movieId")%>%left_join(b_u_t, by="userId")%>%left_join(b_t_t, by="rating_time")%>%
    mutate(p=mu_hat+b_i+b_u+b_t)%>%pull(p)
RMSE(prediction_t, test_set$rating)


#*******Validation RMSE********
prediction_v<-validation%>%left_join(movie_avg, by="movieId")%>%left_join(um_avg, by="userId")%>%
  left_join(time_avg,by="rating_time")%>%mutate(p=mu_hat+b_i+b_u+b_t)%>%pull(p)
RMSE(prediction_v, validation$rating)
####### tuned prediction
prediction_tuned<-validation%>%left_join(b_i_t, by="movieId")%>%left_join(b_u_t, by="userId")%>%left_join(b_t_t, by="rating_time")%>%
  mutate(p=mu_hat+b_i+b_u+b_t)%>%pull(p)
RMSE_mutr<-RMSE(prediction_tuned, validation$rating)
round(RMSE_mutr, 5)


