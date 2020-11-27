##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(patchwork)

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
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))
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

#  save variable as R file
saveRDS(edx, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/edx.Rds")
saveRDS(validation, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/validation.Rds")
saveRDS(movielens, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/movielens.Rds")


rm(dl, ratings, movies, test_index, temp, removed)



# Description of data.


skimr::skim(movielens)
skimr::skim(edx)
skimr::skim(validation)





# first ten observation of movielens
movielens_head <- movielens %>% 
 head(n=5) 

# unique observation in each column

movielens_distinct <- movielens %>% 
  summarise(userId=n_distinct(userId),
            movieId = n_distinct(movieId),
            rating = n_distinct(rating),
            timestamp = n_distinct(timestamp),
            title = n_distinct(title),
            genres = n_distinct(genres),
            n = n()
            )


# save movieLens_head and movielens_distinct
saveRDS(movielens_head, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/movielens_head.Rds")
saveRDS(movielens_distinct, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/movielens_distinct.Rds")



movielens<- readRDS("C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/movielens.Rds")

rm(movielens)




# data wrangling

edx_df1 <- edx %>% 
  mutate(year_movie = sub("\\).*", "", sub(".*\\(", "", title)),  # extracting the year the movie premiered from title
         year_movie = as.numeric(year_movie)   # convert   year_movie into a numeric  
        ) %>%   
 mutate(date = lubridate::as_datetime(timestamp),  # create dates and time features 
       year_rated = year(date),    # year movie was rated
       month_rated = month(date),  # month movie was rated
       week_rated = week(date),    # week movie was rated
       day_rated = wday(date),     # day movie was rated
       ) %>%    
  mutate(year_gap = year_rated - year_movie) %>%    # calculate years between when a movie premiered and when it is rated
  mutate(genres = as.factor(genres),     # convert genres to a factor variable
         userId = as.factor(userId),     # convert userId to a factor variable
         movieId = as.factor(movieId)    # convert movieId to a factor variable
  )  



edx_df1_head <-  edx_df1 %>% 
  head(n=5)

saveRDS(edx_df1_head, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/edx_df1_head.Rds")




# SPLIT EDX_DATASET INTO EDX_TRAIN AND EDX_TEST


set.seed(1, sample.kind="Rounding")  # set seed of random numbers so results can be replicated.
test_index <- createDataPartition(y = edx_df1$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx_df1[-test_index,]
edx_test1 <- edx_df1[test_index,]


# Make sure userId and movieId in edx_test dataset are also in edx_train dataset
edx_test <- edx_test1 %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")


# Add rows removed from edx_test dataset back into edx_train data_set
removed_edx <- anti_join(edx_test1, edx_test)
edx_train <- rbind(edx_train, removed_edx)

# Make sure that edx_train has all the ditinct user and movie ids as in edx
n_distinct(edx_train$userId)==n_distinct(edx$userId)
n_distinct(edx_train$movieId)==n_distinct(edx$movieId)


rm(removed_edx, edx_test1, test_index)

saveRDS(edx_train, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/edx_train.Rds")
saveRDS(edx_test, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/edx_test.Rds")





#### Exploratory Data Analysis


# RATING DATASET

mu <- mean(edx_train$rating)  # mean ratings

FIG_RATING <- edx_train %>% 
  group_by(rating) %>% 
  summarise(n_rating = n(),
            average = mean(rating),
            n_rating = formattable::comma(n_rating, digits = 0)) %>% # format number to include commas
  ggplot(aes(x=rating, y=n_rating, fill= (rating)))+
  geom_bar(stat = "identity")+
  geom_text(aes(label=n_rating), vjust=-0.5)  +
  scale_x_continuous(breaks = seq(0.5,5,0.5)) +
  scale_fill_gradient(low = "yellow", high = "red", n.breaks=10)+
  geom_vline(xintercept = mu, colour="black")+
  theme_bw() +
  labs(y="Number",
       x="rating")+
    geom_text(aes(3.1, 2300000, label = paste(
        "Mean Rating = ", formattable::comma(mean(edx_train$rating), digits = 2)
        
      )))

# save figure
ggsave(FIG_RATING, filename = "graph/FIG_RATING.png")



# Basic Statistis

edx_train %>% 
   summarise(
     minimum = min(rating),
     firt_quartile = quantile(rating, 0.25),
     median = median(rating),
     mean = mean(rating),
     third_quartile = quantile(rating, 0.75),
     maximum = max(rating),
     standard_deviation = sd(rating)
        ) %>% 
  pivot_longer(1:7,names_to = "indicators", values_to = "estimtes") 


# movieID


edx_train %>% 
  group_by(movieId) %>% 
  summarise(n_movieId = n(), avg_rating = mean(rating)) %>% 
  ggplot(aes(x=n_movieId))+
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  labs(x="Number of Movie ratings (bin=30)")+
  theme_bw()
  

edx_train %>% 
  group_by(movieId, title) %>% 
  summarise(n_movieId = n(), avg_rating = mean(rating)) %>% 
  arrange(desc(n_movieId)) %>% 
  head()

edx_train %>% 
  group_by(movieId) %>% 
  summarise(n_movieId = n(), avg_rating = mean(rating)) %>% 
  arrange((n_movieId)) %>% 
  summarise(check=sum(n_movieId==1))  # number of movies that are rated only once.

edx_train %>% 
  group_by(movieId) %>% 
  summarise(n_movieId = n(), avg_rating = mean(rating)) %>% 
  arrange((n_movieId)) %>% 
  summarise(check=sum(n_movieId<10))  # number of movies that are rated less than 10 times




edx_train %>% 
  group_by(movieId, title) %>% 
  summarise(n_movieId = n(), avg_rating = mean(rating)) %>% 
  arrange(desc(avg_rating)) %>% 
  head()







edx_train %>% 
  group_by(movieId) %>% 
  summarise(n_movieId = n(), avg_rating = mean(rating)) %>% 
  #arrange((n_movieId)) %>% 
  ggplot(aes(x=n_movieId, y=avg_rating))+
  scale_x_log10()+
  #scale_x_sqrt()+
  geom_bin2d()+
  geom_smooth()+
  theme_bw()

edx_train %>% 
  group_by(movieId) %>% 
  summarise(n_movieId = n(), avg_rating = mean(rating)) %>% 
  arrange((n_movieId)) %>% 
  ggplot(aes(x=movieId, y=avg_rating))+
  #scale_x_log10()+
  #scale_x_sqrt()+
  geom_point()

edx_train %>% 
  #group_by(movieId) %>% 
  #summarise(n_movieId = n(), avg_rating = mean(rating)) %>% 
  #arrange((n_movieId)) %>% 
  ggplot(aes(x=movieId, y=rating))+
  #scale_x_log10()+
  #scale_x_sqrt()+
  geom_boxplot()+
  theme_bw()





 edx_train %>% 
  group_by(movieId) %>% 
  summarise(n_movieId = n(), avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  summarise(rank = c("min", "Q1", "median", "Q3", "max"),
            movieId_avg= quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)),
            movieId_count= quantile(n_movieId, c(0, 0.25, 0.5, 0.75,1))
              )
  
 edx_train %>% 
   group_by(movieId) %>% 
   summarise(count = n(), avg_rating = mean(rating)) %>% 
   ungroup() %>% 
   summarise(rank = c("min", "Q1", "median", "Q3", "max", "Inter-Quartile Range", "range"),
             movieId_avg= c(quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)), IQR(avg_rating), (max(avg_rating)-min(avg_rating))),
             movieId_count= c(quantile(count, c(0, 0.25, 0.5, 0.75,1)), IQR(count), (max(count)-min(count))))
 
 
 
 

 


# USerID



edx_train %>% 
  group_by(userId) %>% 
  summarise(n_userId = n(), avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  ggplot(aes((n_userId)))+
  scale_x_log10() + 
  geom_histogram(bins=30)+
  theme_bw()


edx_train %>% 
  group_by(userId) %>% 
  summarise(n_userId = n(), avg_rating = mean(rating)) %>% 
  arrange(desc(n_userId)) %>% 
  head()


edx_train %>% 
  group_by(userId) %>% 
  summarise(n_userId = n(), avg_rating = mean(rating)) %>% 
  arrange((n_userId)) %>% 
  head()


edx_train %>% 
  group_by(userId) %>% 
  summarise(n_user = n(), avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  summarise(userId= my_quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)))



# Genres


edx_genres <- edx_train %>% 
  mutate(genres = as.character(genres)) %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n(), avg_rating = mean(rating)) %>%
  arrange(desc(count))



# save movieLens_head and movielens_distinct
saveRDS(edx_genres, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/edx_train_genres.Rds")


edx_train %>% 
  group_by(genres) %>% 
  summarise(count = n(), avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  summarise(rank = c("min", "Q1", "median", "Q3", "max", "Inter-Quartile Range", "range"),
            genres_avg= c(quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)), IQR(avg_rating), (max(avg_rating)-min(avg_rating))),
            genres_count= c(quantile(count, c(0, 0.25, 0.5, 0.75,1)), IQR(count), (max(count)-min(count))))



edx_train %>% 
  group_by(genres) %>% 
  summarise(count=n(), avg_rating = mean(rating)) %>% 
  arrange((avg_rating))


# DATE EFFECTS

edx_train %>% 
  group_by(year_movie) %>% 
  mutate(avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year_movie, group=year_movie))+
  geom_boxplot(aes(y=rating))+
  geom_line(aes(y=avg_rating))+
  theme_bw()

edx_train %>% 
  group_by(year_movie) %>% 
  mutate(avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year_movie, group=year_movie))+
  geom_boxplot(aes(y=rating))+
  geom_line(aes(y=avg_rating, colour="red"))




fig_ym <- edx_train %>% 
  group_by(year_movie) %>% 
  summarise(n = n(),
            avg_rating = mean(rating),    # calculating the average rating by premiere year
            sd = sd(rating),              # calculating the standard deviation of rating by premiere year
            se = sd/sqrt(n),              # calculating the standard error of rating by premiere year
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),   # calculating a 95% confidence interval by premeir year
            min_rating = min(rating),    # calculating the minimum rating by premiere year
            max_rating=max(rating)) %>%    # calculating the maximum rating by premiere year
  ggplot(aes(x=year_movie, y=avg_rating))+
  geom_line(linetype = 4)+
  geom_errorbar(aes(ymin = avg_rating - ci, 
                    ymax = avg_rating + ci), 
                    colour = "red",
                width = 1.5)+
    scale_y_continuous(breaks = seq(0.5, 5, 0.5), 
                   limits=c(2, 5))+
  theme_bw()+
  geom_smooth()+
  labs(x = "premiere year",
       y= "average movie rating")


edx_train %>% 
  group_by(year_movie) %>% 
  summarise(n = n(),
            avg_rating = mean(rating),    # calculating the average rating by premiere year
            sd = sd(rating),              # calculating the standard deviation of rating by premiere year
            se = sd/sqrt(n),              # calculating the standard error of rating by premiere year
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),   # calculating a 95% confidence interval by premeir year
            min_rating = min(rating),    # calculating the minimum rating by premiere year
            max_rating=max(rating)) %>% 
  arrange((n))


fig_yr <- edx_train %>% 
  group_by(year_rated) %>% 
  summarise(n = n(),
            avg_rating = mean(rating),    # calculating the average rating by year rated
            sd = sd(rating),              # calculating the standard deviation of rating by year rated
            se = sd/sqrt(n),              # calculating the standard error of rating by year rated
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),   # calculating a 95% confidence interval by year rated
            min_rating = min(rating),    # calculating the minimum rating by year rated
            max_rating=max(rating)) %>%    # calculating the maximum rating by year rated
  #filter(year_rated>1995) %>% 
  ggplot(aes(x=year_rated, y=avg_rating))+
  geom_line()+
  geom_errorbar(aes(ymin = avg_rating - ci, 
                    ymax = avg_rating + ci), 
                colour = "red",
                width = 1.5)+
  scale_y_continuous(breaks = seq(0.5, 5, 0.5), 
                     limits=c(2, 5))+
  theme_bw()+
  geom_smooth()






edx_train %>% 
  group_by(year_rated) %>% 
  summarise(n = n(),
            avg_rating = mean(rating),    # calculating the average rating by premiere year
            sd = sd(rating),              # calculating the standard deviation of rating by premiere year
            se = sd/sqrt(n),              # calculating the standard error of rating by premiere year
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),   # calculating a 95% confidence interval by premeir year
            min_rating = min(rating),    # calculating the minimum rating by premiere year
            max_rating=max(rating)) %>% 
  arrange(desc(year_rated))





fig_yg <- edx_train %>% 
  group_by(year_gap) %>% 
  #mutate(year_gap = if_else(year_gap<0, 0, year_gap)) %>% 
  summarise(n = n(),
            avg_rating = mean(rating),    # calculating the average rating by year gap
            sd = sd(rating),              # calculating the standard deviation of rating by year gap
            se = sd/sqrt(n),              # calculating the standard error of rating by year gap
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),   # calculating a 95% confidence interval by year gap
            min_rating = min(rating),    # calculating the minimum rating by year gap
            max_rating=max(rating)) %>%    # calculating the maximum rating by year gap
  ggplot(aes(x=year_gap, y=avg_rating))+
  geom_line()+
  geom_errorbar(aes(ymin = avg_rating - ci, 
                    ymax = avg_rating + ci), 
                colour = "red",
                width = 1.5)+
  geom_smooth()+
# scale_x_continuous(breaks = seq(-2, 94, 4))+
  scale_y_continuous(breaks = seq(0.5, 5, 0.5), 
                     limits=c(2, 5))+
  theme_bw()



month <- seq(as.Date("2019-01-01"), as.Date("2019-12-01"), by = "1 month")
month_label <- lubridate::month(month, label=TRUE)

fig_mr <- edx_train %>% 
  group_by(month_rated) %>% 
  summarise(n = n(),
            avg_rating = mean(rating),    # calculating the average rating by month rate
            sd = sd(rating),              # calculating the standard deviation of rating by month rated
            se = sd/sqrt(n),              # calculating the standard error of rating by month rated
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),   # calculating a 95% confidence interval by month rated
            min_rating = min(rating),    # calculating the minimum rating by month rated
            max_rating=max(rating)) %>%    # calculating the maximum rating by month rated
  ggplot(aes(x= month_rated, y=avg_rating))+
  geom_line() +
  geom_errorbar(aes(ymin = avg_rating - ci, 
                    ymax = avg_rating + ci), 
                colour = "red",
                width = 1.5)+
  geom_smooth()+
  scale_x_continuous(breaks = 1:12,
                     labels = month_label)  +
  scale_y_continuous(breaks = seq(0.5, 5, 0.5), 
                     limits=c(2, 5))+
  theme_bw()+
  labs(x = "Month",
       y= "average movie rating")

# Week_rated


fig_wr <- edx_train %>% 
  group_by(week_rated) %>% 
  summarise(n = n(),
            avg_rating = mean(rating),    # calculating the average rating by week rate
            sd = sd(rating),              # calculating the standard deviation of rating by week  rated
            se = sd/sqrt(n),              # calculating the standard error of rating by week rated
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),   # calculating a 95% confidence interval by week rated
            min_rating = min(rating),    # calculating the minimum rating by week rated
            max_rating=max(rating)) %>%    # calculating the maximum rating by week rated
  ggplot(aes(x=  week_rated, y=avg_rating))+
  geom_line()+
  geom_errorbar(aes(ymin = avg_rating - se, 
                    ymax = avg_rating + se), 
                colour = "red",
                width = 1.5)+
  #scale_x_continuous(breaks = seq(1, 53, 1))+
  scale_y_continuous(breaks = seq(0.5, 5, 0.5), 
                     limits=c(2, 5))+
  theme_bw()+
  labs(x = "Week",
       y= "average movie rating")


# day_rated

week_days<- seq(as.Date("2020-11-01"), as.Date("2020-11-07"), by = "1 day")
week_days_label <- lubridate::wday(week_days, label=TRUE)


fig_dr <- edx_train %>% 
  group_by(day_rated) %>% 
  summarise(n = n(),
            avg_rating = mean(rating),    # calculating the average rating by weekday rate
            sd = sd(rating),              # calculating the standard deviation of rating by weekday  rated
            se = sd/sqrt(n),              # calculating the standard error of rating by weekday rated
            ci = qt(0.975, df = n - 1) * sd / sqrt(n),   # calculating a 95% confidence interval by weekday rated
            min_rating = min(rating),    # calculating the minimum rating by weekday rated
            max_rating=max(rating)) %>%    # calculating the maximum rating by weekday rated
  ggplot(aes(x=  day_rated, y=avg_rating))+
  geom_line()+
  geom_errorbar(aes(ymin = avg_rating - se, 
                    ymax = avg_rating + se), 
                colour = "red",
                width = 1.5)+
  scale_x_continuous(breaks = seq(1, 7, 1),
                     labels = week_days_label)  +
  scale_y_continuous(breaks = seq(0.5, 5, 0.5), 
                     limits=c(2, 5))+
  theme_bw()+
  labs(x = "WEEKDAY",
       y= "average movie rating")



library(patchwork)


fig_all_year <- (fig_ym / fig_yr) / (fig_yg)
fig_all_year1 <- fig_all_year + plot_annotation(tag_levels = list(c('Figure 5A', 'Figure 5B', 'Figure 5C')))

fig_mwd <- (fig_mr / fig_wr) / (fig_dr)
fig_mwd1 <- fig_mwd + plot_annotation(tag_levels = list(c('Figure 5D', 'Figure 5E', 'Figure 5F')))



# save figure
ggsave(fig_all_year1, filename = "graph/fig_all_year1.png")
ggsave(fig_mwd1, filename = "graph/fig_mwd1.png")







# adjust edx_train and edx_test for year gap that is negative

edx_train <- edx_train %>% 
  mutate(year_gap_adj = if_else(year_gap<0, 0, year_gap))  # if year gap is less than zero (that is there is an error in the data), place the value equal to zero



edx_test <- edx_test %>% 
  mutate(year_gap_adj = if_else(year_gap<0, 0, year_gap))  # if year gap is less than zero (that is there is an error in the data), place the value equal to zero

#check to see that everything is ok
sum(edx_train$year_gap_adj<0)
sum(edx_test$year_gap_adj<0)


# DATE FEATURE TABLES

stat_ym <- edx_train %>% 
  group_by(year_movie) %>% 
  summarise(count = n(), avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  summarise(rank = c("min", "Q1", "median", "Q3", "max", "Inter-Quartile Range", "range"),
            ayear_movie= c(quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)), IQR(avg_rating), (max(avg_rating)-min(avg_rating))),
            cyear_movie= c(quantile(count, c(0, 0.25, 0.5, 0.75,1)), IQR(count), (max(count)-min(count))))


stat_yr <- edx_train %>% 
  group_by(year_rated) %>% 
  summarise(count = n(), avg_rating = mean(rating)) %>% 
  #filter(year_rated>1995) %>% 
  ungroup() %>% 
  summarise(rank = c("min", "Q1", "median", "Q3", "max", "Inter-Quartile Range", "range"),
            ayear_rated= c(quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)), IQR(avg_rating), (max(avg_rating)-min(avg_rating))),
            cyear_rated= c(quantile(count, c(0, 0.25, 0.5, 0.75,1)), IQR(count), (max(count)-min(count))))



stat_yg <- edx_train %>% 
  group_by(year_gap_adj) %>% 
  summarise(count = n(), avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  summarise(rank = c("min", "Q1", "median", "Q3", "max", "Inter-Quartile Range", "range"),
            ayear_gap = c(quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)), IQR(avg_rating), (max(avg_rating)-min(avg_rating))),
            cyear_gap= c(quantile(count, c(0, 0.25, 0.5, 0.75,1)), IQR(count), (max(count)-min(count))))




stat_mr <- edx_train %>% 
  group_by(month_rated) %>% 
  summarise(count = n(), avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  summarise(rank = c("min", "Q1", "median", "Q3", "max", "Inter-Quartile Range", "range"),
            amonth_rated= c(quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)), IQR(avg_rating), (max(avg_rating)-min(avg_rating))),
            cmonth_rated= c(quantile(count, c(0, 0.25, 0.5, 0.75,1)), IQR(count), (max(count)-min(count))))

stat_wr <- edx_train %>% 
  group_by(week_rated) %>% 
  summarise(count = n(), avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  summarise(rank = c("min", "Q1", "median", "Q3", "max", "Inter-Quartile Range", "range"),
            aweek_rated= c(quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)), IQR(avg_rating), (max(avg_rating)-min(avg_rating))),
            cweek_rated= c(quantile(count, c(0, 0.25, 0.5, 0.75,1)), IQR(count), (max(count)-min(count))))

stat_dr <- edx_train %>% 
  group_by(day_rated) %>% 
  summarise(count = n(), avg_rating = mean(rating)) %>% 
  ungroup() %>% 
  summarise(rank = c("min", "Q1", "median", "Q3", "max", "Inter-Quartile Range", "range"),
            aday_rated = c(quantile(avg_rating, c(0, 0.25, 0.5, 0.75,1)), IQR(avg_rating), (max(avg_rating)-min(avg_rating))),
            cday_rated= c(quantile(count, c(0, 0.25, 0.5, 0.75,1)), IQR(count), (max(count)-min(count))))



date_effect <- stat_ym %>% 
  left_join(stat_yr, by="rank") %>% 
  left_join(stat_yg, by="rank") %>% 
  left_join(stat_mr, by="rank") %>% 
  left_join(stat_wr, by="rank") %>% 
  left_join(stat_dr, by="rank") %>% 
  select(rank,ayear_movie, ayear_rated, ayear_gap, amonth_rated, aweek_rated, aday_rated, everything())

# save movieLens_head and movielens_distinct
saveRDS(date_effect, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/date_effect.Rds")







####

# Regularized Model 1

mu <- mean(edx_train1$rating)





rmse_mu <- edx_test %>% 
  mutate(predict_rating = mu) %>% 
  summarise(yardstick::rmse(.,predict_rating,rating))

bind_rows(data.frame( method = "$\\beta_0$", 
                      RMSE = rmse_mu$.estimate)
         
)



lambdas_m_r1<- seq(0,3, 0.1)

rmse_m_r1 <- sapply(lambdas_m_r1, function(l){
  
b_m_r1 <- edx_train %>% 
    group_by(movieId) %>% 
    summarise(b_m = sum(rating - mu)/(n()+l))
  
  
  reg_predictm <- edx_test %>% 
    left_join(b_m_r1, by="movieId") %>% 
    mutate(predict_rating = mu+b_m)
  
  rmse <-  yardstick::rmse(reg_predictm, rating, predict_rating)
  rmse$.estimate
  
})

fig_lm <- qplot(lambdas_m_r1, rmse_m_r1)  +
    labs(title = expression(paste("Optimal Value of  ", hat(lambda)[m],
                                  " that minimizes the RMSE for ", "\n",
                                  hat(e)[m][u], " = ",  r[m][u]-bar(b)-hat(b)[m])),
       x = expression(hat(lambda)[m]),
       y = "RMSE"
      
        )+
  annotate("text", x = lambdas_m_r1[which.min(rmse_m_r1)], y = 0.94295, colour="red",
           label = paste("=", lambdas_m_r1[which.min(rmse_m_r1)]), size=5)+
      annotate("text", x = lambdas_m_r1[which.min(rmse_m_r1)]-0.2, y = 0.94295, colour="red",
           label = expression(hat(lambda)[m]), size=5)+
  theme_bw()+
  theme(plot.title = element_text(size = 11, 
                                  face = "bold"))+ 
  theme(axis.text = element_text(size = 9, 
         face = "bold", colour = "black"))

fig_lm  
           
lambdas_m_r1[which.min(rmse_m_r1)]
min(rmse_m_r1)

l_m_r1 <- lambdas_m_r1[which.min(rmse_m_r1)]

b_m_r1 <- edx_train %>% 
  group_by(movieId) %>% 
  summarise(b_m = sum(rating - mu)/(n()+l_m_r1))


rmse_movie_r1 <- edx_test %>% 
  left_join(b_m_r1, by="movieId") %>% 
  mutate(predict_rating = mu+b_m)%>% 
  summarise(yardstick::rmse(.,predict_rating,rating))



bind_rows(data.frame( method = "Average Rating", 
                    RMSE_R1 = rmse_mu$.estimate),
                      
          data.frame(method="Movie Effect",
                     RMSE_R1 = rmse_movie_r1$.estimate)
)


#### User Effect


lambdas_u_r1<- seq(3,7,0.1)

rmse_u_r1 <- sapply(lambdas_u_r1, function(l){
  
  b_u_r1 <- edx_train %>% 
    left_join(b_m_r1, by="movieId") %>% 
    group_by(userId) %>% 
    summarise(b_u = sum(rating - mu-b_m)/(n()+l))
  
  
  reg_predict <- edx_test %>% 
    left_join(b_mr1, by="movieId") %>% 
    left_join(b_u_r1, by="userId") %>% 
    mutate(predict_rating = mu+b_m+b_u)
  
  rmse <-  yardstick::rmse(reg_predict, rating, predict_rating)
  rmse$.estimate
  
})

fig_lu <- qplot(lambdas_u_r1, rmse_u_r1) +
    labs(title = expression(paste("Optimal Value of  ", "", hat(lambda)[u],
                                  " that minimizes the RMSE for ", "\n",
                                  hat(e)[m][u], " = ",  r[m][u]-bar(b)-hat(b)[m]-hat(b)[u])),
       x = expression(hat(lambda)[u]),
       y = "RMSE"
       
  )+
  annotate("text", x = lambdas_u_r1[which.min(rmse_u_r1)], y = 0.86422, colour="red",
           label = paste("=", lambdas_u_r1[which.min(rmse_u_r1)]), size=5)+
  annotate("text", x = lambdas_u_r1[which.min(rmse_u_r1)]-0.2, y = 0.86422, colour="red",
           label = expression(hat(lambda)[u]), size=5)+
  theme_bw() +
  theme(plot.title = element_text(size = 11, 
                                  face = "bold"))+ 
  theme(axis.text = element_text(size = 9, 
                                 face = "bold", colour = "black"))

fig_lu



lambdas_u_r1[which.min(rmse_u_r1)]
min(rmse_u_r1)

l_u_r1 <- lambdas_u_r1[which.min(rmse_u_r1)]

b_u_r1 <- edx_train %>% 
  left_join(b_m_r1, by="movieId") %>% 
  group_by(userId) %>% 
  summarise(b_u = sum(rating - mu-b_m)/(n()+l_u_r1))


rmse_user_r1 <- edx_test %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  mutate(predict_rating = mu+b_m+b_u)%>% 
  summarise(yardstick::rmse(.,predict_rating,rating))


bind_rows(data.frame( method = "Average Rating", 
                      RMSE_R1 = rmse_mu$.estimate),
          
          data.frame(method="Movie Effect",
                     RMSE_R1 = rmse_movie_r1$.estimate),
          
          data.frame(method="Movie and User Effects",
                     RMSE_R1 = rmse_user_r1$.estimate),
)

# genre effect

lambdas_g_r1<- seq(0,10,1)

rmse_gg_r1 <- sapply(lambdas_g_r1, function(l){
  
  b_g_r1 <- edx_train %>% 
    left_join(b_m_r1, by="movieId") %>% 
    left_join(b_u_r1, by="userId") %>% 
    group_by(genres) %>% 
    summarise(b_g = sum(rating - mu-b_m-b_u)/(n()+l))
  
  
  reg_predict <- edx_test %>% 
    left_join(b_m_r1, by="movieId") %>% 
    left_join(b_u_r1, by="userId") %>% 
    left_join(b_g_r1, by="genres") %>% 
    mutate(predict_rating = mu+b_m+b_u+b_g)
  
  rmse <-  yardstick::rmse(reg_predict, rating, predict_rating)
  rmse$.estimate
  
})

fig_lg <- qplot(lambdas_g_r1, rmse_gg_r1) +
  labs(title = expression(paste("Optimal Value of  ", "", hat(lambda)[g],
                                " that minimizes the RMSE for ",  "\n",
                                hat(e)[m][u], " = ",  r[m][u]-bar(b)-hat(b)[m]-hat(b)[u]-hat(b)[g])),
       x = expression(hat(lambda)[g]),
       y = "RMSE"
       
  )+
  annotate("text", x = lambdas_g_r1[which.min(rmse_gg_r1)]+0.2, y = 0.8638544, colour="red",
           label = paste(" =", lambdas_g_r1[which.min(rmse_gg_r1)]), size=5)+
  annotate("text", x = lambdas_g_r1[which.min(rmse_gg_r1)]-0.2, y = 0.8638544, colour="red",
           label = expression(hat(lambda)[g]), size=5)+
  theme_bw()  +
  theme(plot.title = element_text(size = 11, 
                                  face = "bold"))+ 
  theme(axis.text = element_text(size = 9, 
                                 face = "bold", colour = "black"))

fig_lg



lambdas_g_r1[which.min(rmse_g_r1)]
min(rmse_g_r1)

l_g_r1 <- lambdas_g_r1[which.min(rmse_g_r1)]


b_g_r1 <- edx_train %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  group_by(genres) %>% 
  summarise(b_g = sum(rating - mu-b_m-b_u)/(n()+l_g_r1))


rmse_genres_r1 <- edx_test %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  mutate(predict_rating = mu+b_m+b_u+b_g) %>% 
  summarise(yardstick::rmse(.,predict_rating,rating))




bind_rows(data.frame( method = "Average Rating", 
                      RMSE_R1 = rmse_mu$.estimate),
          
          data.frame(method="Movie Effect",
                     RMSE_R1 = rmse_movie_r1$.estimate),
          
          data.frame(method="Movie and User Effects",
                     RMSE_R1 = rmse_user_r1$.estimate),
          data.frame(method="Movie, User and Genres Effects",
                     RMSE_R1 = rmse_genres_r1$.estimate),
          
          
)
  

## Time series effect - 


# Movie Year

lambdas_p_r1<- seq(5.5,6.5,0.05)

rmse_p_r1 <- sapply(lambdas_p_r1, function(l){
  
  b_p_r1<- edx_train %>% 
    left_join(b_m_r1, by="movieId") %>% 
    left_join(b_u_r1, by="userId") %>% 
    left_join(b_g_r1, by="genres") %>% 
    group_by(year_movie) %>% 
    summarise(b_p = sum(rating - mu-b_m-b_u-b_g)/(n()+l))
  
  
  reg_predict <- edx_test %>% 
    left_join(b_m_r1, by="movieId") %>% 
    left_join(b_u_r1, by="userId") %>% 
    left_join(b_g_r1, by="genres") %>% 
    left_join(b_p_r1, by="year_movie") %>% 
    mutate(predict_rating = mu+b_m+b_u+b_g+b_p)
  
  rmse <-  yardstick::rmse(reg_predict, rating, predict_rating)
  rmse$.estimate
  
})

fig_lp <- qplot(lambdas_p_r1, rmse_p_r1)+  
    labs(title = expression(paste("Optimal Value of  ", "", hat(lambda)[p],
                                  " that minimizes the RMSE for ", "\n",
                                  hat(e)[m][u], " = ",  r[m][u]-bar(b)-hat(b)[m]-hat(b)[u]-hat(b)[g]-hat(b)[p])),
       x = expression(hat(lambda)[p]),
       y = "RMSE"
       
  )+
  annotate("text", x = lambdas_p_r1[which.min(rmse_p_r1)], y = 0.8636851114, colour="red",
           label = paste(" =", lambdas_p_r1[which.min(rmse_p_r1)]), size=5)+
  annotate("text", x = lambdas_p_r1[which.min(rmse_p_r1)]-0.05, y = 0.8636851114, colour="red",
           label = expression(hat(lambda)[p]), size=5)+
  theme_bw() +
  theme(plot.title = element_text(size = 11, 
                                  face = "bold"))+ 
  theme(axis.text = element_text(size = 9, 
                                 face = "bold", colour = "black"))

fig_lp









lambdas_p_r1[which.min(rmse_p_r1)]
min(rmse_p_r1)

l_p_r1 <- lambdas_p_r1[which.min(rmse_p_r1)]

b_p_r1 <- edx_train %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  group_by(year_movie) %>% 
  summarise(b_p = sum(rating - mu-b_m-b_u-b_g)/(n()+l_p_r1))


rmse_premiere_r1 <- edx_test %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  left_join(b_p_r1, by="year_movie") %>% 
  mutate(predict_rating = mu+b_m+b_u+b_g+b_p) %>% 
  summarise(yardstick::rmse(.,predict_rating,rating))




bind_rows(data.frame( method = "Average Rating", 
                      RMSE_R1 = rmse_mu$.estimate),
          
          data.frame(method="Movie Effect",
                     RMSE_R1 = rmse_movie_r1$.estimate),
          
          data.frame(method="Movie and User Effects",
                     RMSE_R1 = rmse_user_r1$.estimate),
          data.frame(method="Movie, User and Genres Effects",
                     RMSE_R1 = rmse_genres_r1$.estimate),
          
          data.frame(method="Movie, User, Genres and Premiere Year Effects",
                     RMSE_R1 = rmse_premiere_r1$.estimate),
)




# gap Year 

lambdas_gap_r1<- seq(2500,2800,25)

rmse_gap_r1 <- sapply(lambdas_gap_r1, function(l){
  
  b_gap_r1 <- edx_train %>% 
    left_join(b_m_r1, by="movieId") %>% 
    left_join(b_u_r1, by="userId") %>% 
    left_join(b_g_r1, by="genres") %>% 
    left_join(b_p_r1, by="year_movie") %>% 
    group_by(year_gap_adj) %>% 
    summarise(b_gap = sum(rating - mu-b_m-b_u-b_g-b_p)/(n()+l))
  
  
  reg_predict <- edx_test %>% 
    left_join(b_m_r1, by="movieId") %>% 
    left_join(b_u_r1, by="userId") %>% 
    left_join(b_g_r1, by="genres") %>% 
    left_join(b_p_r1, by="year_movie") %>% 
    left_join(b_gap_r1, by="year_gap_adj") %>% 
    mutate(predict_rating = mu+b_m+b_u+b_g+b_p+b_gap)
  
  rmse <-  yardstick::rmse(reg_predict, rating, predict_rating)
  rmse$.estimate
  
})

fig_lgap <- qplot(lambdas_gap_r1, rmse_gap_r1)  +
  labs(title = expression(paste("Optimal Value of  ", "", hat(lambda)[gap],
                                " that minimizes the RMSE for ", "\n",
                                hat(e)[m][u], " = ",  r[m][u]-bar(b)-hat(b)[m]-hat(b)[u]-hat(b)[g]-hat(b)[p]-hat(b)[gap])),
       x = expression(hat(lambda)[gap]),
       y = "RMSE"
       
  )+
  annotate("text", x = lambdas_gap_r1[which.min(rmse_gap_r1)], y = 0.863320917, colour="red",
           label = paste(" =", lambdas_gap_r1[which.min(rmse_gap_r1)]), size=5)+
  annotate("text", x = lambdas_gap_r1[which.min(rmse_gap_r1)]-20, y = 0.863320917, colour="red",
           label = expression(hat(lambda)[gap]), size=5)+
  theme_bw() +
  theme(plot.title = element_text(size = 11, 
                                  face = "bold"))+ 
  theme(axis.text = element_text(size = 9, 
                                 face = "bold", colour = "black"))


fig_lgap




lambdas_gap_r1[which.min(rmse_gap_r1)]
min(rmse_gap_r1)

l_gap_r1 <- lambdas_gap_r1[which.min(rmse_gap_r1)]

b_gap_r1 <- edx_train %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  left_join(b_p_r1, by="year_movie") %>% 
  group_by(year_gap_adj) %>% 
  summarise(b_gap = sum(rating - mu-b_m-b_u-b_g-b_p)/(n()+l_gap_r1))


rmse_year_gap_r1 <- edx_test %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  left_join(b_p_r1, by="year_movie") %>% 
  left_join(b_gap_r1, by="year_gap_adj") %>% 
  mutate(predict_rating = mu+b_m+b_u+b_g+b_p+b_gap) %>% 
  summarise(yardstick::rmse(.,predict_rating,rating))





bind_rows(data.frame( method = "Average Rating", 
                      RMSE_R1 = rmse_mu$.estimate),
          
          data.frame(method="Movie Effect",
                     RMSE_R1 = rmse_movie_r1$.estimate),
          
          data.frame(method="Movie and User Effects",
                     RMSE_R1 = rmse_user_r1$.estimate),
          data.frame(method="Movie, User and Genres Effects",
                     RMSE_R1 = rmse_genres_r1$.estimate),
          
          data.frame(method="Movie, User, Genres and Premiere Year Effects",
                     RMSE_R1 = rmse_premiere_r1$.estimate),
          
          data.frame(method="Movie, User, Genres, Premiere Year and Year Gap Effects",
                     RMSE_R1 = rmse_year_gap_r1$.estimate),
)




# year of rating

lambdas_r_r1<- seq(150000,170000,5000)

rmse_r_r1 <- sapply(lambdas_r_r1, function(l){
  
  b_r <- edx_train %>% 
    left_join(b_m_r1, by="movieId") %>% 
    left_join(b_u_r1, by="userId") %>% 
    left_join(b_g_r1, by="genres") %>% 
    left_join(b_p_r1, by="year_movie") %>% 
    left_join(b_gap_r1, by="year_gap_adj") %>%
    group_by(year_rated) %>% 
    summarise(b_r = sum(rating - mu-b_m-b_u-b_g-b_p-b_gap)/(n()+l))
  
  
  reg_predict <- edx_test %>% 
    left_join(b_m_r1, by="movieId") %>% 
    left_join(b_u_r1, by="userId") %>% 
    left_join(b_g_r1, by="genres") %>% 
    left_join(b_p_r1, by="year_movie") %>% 
    left_join(b_gap_r1, by="year_gap_adj") %>% 
    left_join(b_r, by="year_rated") %>% 
    mutate(predict_rating = mu+b_m+b_u+b_g+b_p+b_gap+b_r)
  
  rmse <-  yardstick::rmse(reg_predict, rating, predict_rating)
  rmse$.estimate
  
})

fig_lr <- qplot(lambdas_r_r1, rmse_r_r1)  +
  labs(title = expression(paste("Optimal Value of  ", "", hat(lambda)[r],
                                " that minimizes the RMSE for ", "\n",
                                hat(e)[m][u], " = ",  r[m][u]-bar(b)-hat(b)[m]-hat(b)[u]-hat(b)[g]-hat(b)[p]-hat(b)[gap]-hat(b)[r])),
       x = expression(hat(lambda)[r]),
       y = "RMSE"
       
  )+
  annotate("text", x = lambdas_r_r1[which.min(rmse_r_r1)], y = 0.8632777, colour="red",
           label = paste(" =", lambdas_r_r1[which.min(rmse_r_r1)]), size=5)+
  annotate("text", x = lambdas_r_r1[which.min(rmse_r_r1)]-1500, y = 0.8632777, colour="red",
           label = expression(hat(lambda)[r]), size=5)+
  theme_bw() +
  theme(plot.title = element_text(size = 11, 
                                  face = "bold"))+ 
  theme(axis.text = element_text(size = 9, 
                                 face = "bold", colour = "black"))


fig_lr



fig_lambda_mu <- (fig_lm / fig_lu)
fig_lambda_gp <- (fig_lg / fig_lp)
fig_lambda_gapr <- (fig_lgap / fig_lr)

# save figure
ggsave(fig_lambda_mu, filename = "graph/fig_lambda_mu.png",width = 7.1)
ggsave(fig_lambda_gp, filename = "graph/fig_lambda_gp.png", width = 7.1)
ggsave(fig_lambda_gapr, filename = "graph/fig_lambda_gapr.png", width = 7.1)

lambdas_r_r1[which.min(rmse_r_r1)]
min(rmse_r_r1)

l_r_r1 <- lambdas_r_r1[which.min(rmse_r_r1)]

b_r_r1 <- edx_train %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  left_join(b_p_r1, by="year_movie") %>% 
  left_join(b_gap_r1, by="year_gap_adj") %>%
  group_by(year_rated) %>% 
  summarise(b_r = sum(rating - mu-b_m-b_u-b_g-b_p-b_gap)/(n()+l_r_r1))


rmse_year_rated_r1 <- edx_test %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  left_join(b_p_r1, by="year_movie") %>% 
  left_join(b_gap_r1, by="year_gap_adj") %>% 
  left_join(b_r_r1, by="year_rated") %>% 
  mutate(predict_rating = mu+b_m+b_u+b_g+b_p+b_gap+b_r) %>% 
  summarise(yardstick::rmse(.,predict_rating,rating))



RMSE_edx_test <- bind_rows(data.frame( METHOD = "Average Effects", 
                      RMSE_R1 = rmse_mu$.estimate
                     ),
          
          data.frame(METHOD="Movie Effect",
                     RMSE_R1 = rmse_movie_r1$.estimate
                     ),
          
          data.frame(METHOD="Movie and User Effects",
                     RMSE_R1 = rmse_user_r1$.estimate
                     ),
          data.frame(METHOD="Movie, User and Genres Effects",
                     RMSE_R1 = rmse_genres_r1$.estimate
                     ),
          
          data.frame(METHOD="Movie, User, Genres and Premiere Year Effects",
                     RMSE_R1 = rmse_premiere_r1$.estimate
                     ),
          
          data.frame(METHOD="Movie, User, Genres, Premiere Year and Year Gap Effects",
                     RMSE_R1 = rmse_year_gap_r1$.estimate
                     ),
          
          data.frame(METHOD="Movie, User, Genres, Premiere Year, Year Gap and Year Rated Effects",
                     RMSE_R1 = rmse_year_rated_r1$.estimate
                     )
          
)











saveRDS(RMSE_edx_test, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/RMSE_edx_test.Rds")


### test validation set

rmse_validation <- validation %>% 
   mutate(year_movie = sub("\\).*", "", sub(".*\\(", "", title)),  # extracting the year the movie premiered from title
         year_movie = as.numeric(year_movie)   # convert   year_movie into a numeric  
  ) %>%   
  mutate(date = lubridate::as_datetime(timestamp),  # create dates and time features 
         year_rated = year(date),    # year movie was rated
         month_rated = month(date),  # month movie was rated
         week_rated = week(date),    # week movie was rated
         day_rated = wday(date),     # day movie was rated
  ) %>%    
  mutate(year_gap = year_rated - year_movie) %>%    # calculate years between when a movie premiered and when it is rated
  mutate(genres = as.factor(genres),     # convert genres to a factor variable
         userId = as.factor(userId),     # convert userId to a factor variable
         movieId = as.factor(movieId)    # convert movieId to a factor variable
  )  %>% 
  mutate(year_gap_adj = if_else(year_gap<0, 0, year_gap)) %>%   # if year gap is less than zero (that is there is an error in the data), place the value equal to zero

  mutate(year_gap = if_else(year_gap<0,0,year_gap)) %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  left_join(b_p_r1, by="year_movie") %>% 
  left_join(b_gap_r1, by="year_gap_adj") %>% 
  left_join(b_r_r1, by="year_rated") %>% 
  mutate(predict_rating = mu+b_m+b_u+b_g+b_p+b_gap+b_r) %>% 
  summarise(yardstick::rmse(.,predict_rating,rating))

saveRDS(rmse_validation, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/rmse_validation.Rds")



rmse_validation_nodate <-  validation %>%    # calculating the rmse for validation set excluding all date effects
  mutate(year_movie = sub("\\).*", "", sub(".*\\(", "", title)),  # extracting the year the movie premiered from title
         year_movie = as.numeric(year_movie)   # convert   year_movie into a numeric  
  ) %>%   
  mutate(date = lubridate::as_datetime(timestamp),  # create dates and time features 
         year_rated = year(date),    # year movie was rated
         month_rated = month(date),  # month movie was rated
         week_rated = week(date),    # week movie was rated
         day_rated = wday(date),     # day movie was rated
  ) %>%    
  mutate(year_gap = year_rated - year_movie) %>%    # calculate years between when a movie premiered and when it is rated
  mutate(genres = as.factor(genres),     # convert genres to a factor variable
         userId = as.factor(userId),     # convert userId to a factor variable
         movieId = as.factor(movieId)    # convert movieId to a factor variable
  )  %>% 
  mutate(year_gap_adj = if_else(year_gap<0, 0, year_gap)) %>%   # if year gap is less than zero (that is there is an error in the data), place the value equal to zero
  
  mutate(year_gap = if_else(year_gap<0,0,year_gap)) %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  mutate(predict_rating = mu+b_m+b_u+b_g) %>% 
  summarise(yardstick::rmse(.,predict_rating,rating))

saveRDS(rmse_validation_nodate, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/rmse_validation_nodate.Rds")

rmse_validation_exclude_yeargap_yearrated <- validation %>%   # calcualting rmse for validation dataset with only one date feature included
  mutate(year_movie = sub("\\).*", "", sub(".*\\(", "", title)),  # extracting the year the movie premiered from title
         year_movie = as.numeric(year_movie)   # convert   year_movie into a numeric  
  ) %>%   
  mutate(date = lubridate::as_datetime(timestamp),  # create dates and time features 
         year_rated = year(date),    # year movie was rated
         month_rated = month(date),  # month movie was rated
         week_rated = week(date),    # week movie was rated
         day_rated = wday(date),     # day movie was rated
  ) %>%    
  mutate(year_gap = year_rated - year_movie) %>%    # calculate years between when a movie premiered and when it is rated
  mutate(genres = as.factor(genres),     # convert genres to a factor variable
         userId = as.factor(userId),     # convert userId to a factor variable
         movieId = as.factor(movieId)    # convert movieId to a factor variable
  )  %>% 
  mutate(year_gap_adj = if_else(year_gap<0, 0, year_gap)) %>%   # if year gap is less than zero (that is there is an error in the data), place the value equal to zero
  
  mutate(year_gap = if_else(year_gap<0,0,year_gap)) %>% 
  left_join(b_m_r1, by="movieId") %>% 
  left_join(b_u_r1, by="userId") %>% 
  left_join(b_g_r1, by="genres") %>% 
  left_join(b_p_r1, by="year_movie") %>% 
  mutate(predict_rating = mu+b_m+b_u+b_g+b_p) %>% 
  summarise(yardstick::rmse(.,predict_rating,rating))


saveRDS(rmse_validation_exclude_yeargap_yearrated, file = "C:/Users/Hugh Morris/Desktop/Capstone/Capstone Project/dataset/rmse_validation_exclude_yeargap_yearrated.Rds")





