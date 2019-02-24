
#Loading data in R
beer <- read.csv('beer_data.csv')

library(dplyr)
library(ggplot2)
library(recommenderlab)

#Cursory review of the data
View(beer)
summary(beer)
dim(beer) 
#Appears that review_profilename has some missing values

nrow(beer)
#475984
#Checking missing values
nrow(beer[is.na(beer$review_profilename) | beer$review_profilename == "", ])
#There are 100 missing values
#Updating the dataset by removing missing values

beer <- beer[!(beer$review_profilename == ""), ]
nrow(beer)
#475884
#Removing duplicates in terms of same user giving multiple ratings to the same beer

beer<-distinct(beer,beer_beerid,review_profilename,.keep_all = TRUE)
nrow(beer)
#474462
#1422 duplicate rows with same user and same beer removed.

# 1.Data preparation
# 1.Choose only those beers that have at least N number of reviews

# Finding all unique beerid and total reviews received by each
beer_review_count <- beer %>% group_by(beer_beerid) %>% summarise(total_beer_reviews = n())
dim(beer_review_count)
# There are a total of 40304 unique beer brands/types (assuming each beer id refers to brand/type)
summary(beer_review_count)
#There seems to be huge variation in terms of number of reviews received by each beerid as median is 2 and max 
#value is 977

#Finding unique users rating different beers
beer_reviwer_count <- beer %>% group_by(review_profilename) %>% summarise(total_user_reviews = n())
dim(beer_reviwer_count)
# There are a total of 22491 unique beer reviewers
summary(beer_reviwer_count)
beer %>% group_by(review_profilename) %>% summarise(total_user_reviews = n()) %>% top_n(1)
#There is also huge variation in terms of number of reviews done by each user as median is 3 and max 
#value is 1842 by northyorksammy

#Visualise the data to assist in determining N
qplot(beer_review_count$total_beer_reviews, geom = "histogram")
#Most of the beers have only 1 review

beer_review_count %>% subset(total_beer_reviews==1) %>% dim()
#18080 beers i.e., 44.86% of beers have only 1 review

#creating dataframe with reiew frequency
review_frequency <- beer_review_count %>% group_by(total_beer_reviews) %>% summarise(review_occurance = n())
review_frequency
#Number of reviews drop exponentially between 1-4 reviews
#Visualise the data to get better idea
ggplot(review_frequency, aes(x = total_beer_reviews, y = review_occurance)) + geom_point()

#Removing beers with 1 review and checking the data
review_frequency_subset <- review_frequency %>% subset(total_beer_reviews != 1)
ggplot(review_frequency_subset, aes(x = total_beer_reviews, y = review_occurance)) + geom_point()
#Similar pattern of the drop in number of reviews per beer

#Removing beers with less than 2 review and checking the data
review_frequency_subset <- review_frequency %>% subset(total_beer_reviews > 2)
ggplot(review_frequency_subset, aes(x = total_beer_reviews, y = review_occurance)) + geom_point()
#exponential drop in this case as well

#it appears that the rate of drop in number of reviews is happening at the fixed rate

review_frequency$review_ratio<-(review_frequency$review_occurance*100)/40304
ggplot(review_frequency,aes(x=total_beer_reviews,y=review_ratio)) + geom_point() 
#Large ratios are keeping the graph similar

#Plotting records with review rations less than 1
ggplot(review_frequency[-c(1:10),], aes(x = total_beer_reviews, y = review_ratio)) + geom_point() +
  geom_jitter(width = .5, height = .25)

#It appears that rate of drop stabalises after from the ratio 0.003
#Filtering the ration less than 0.003 and visualising the data

review_frequency_subset <- subset(review_frequency, review_frequency$review_ratio < .003)
ggplot(review_frequency_subset, aes(x = total_beer_reviews, y = review_ratio)) + geom_point() +geom_jitter(width = 5000, height = .01)

View(review_frequency_subset)
#From N = 175 review ratio becomes constant. But that seems a bit high. Lets check for something else.

# Further analysis to check for value of N
View(beer)
sum(is.na(beer))
beerall_rrmatrix <- as(beer[,c(2,1,3)], "realRatingMatrix")
summary(rowCounts(beerall_rrmatrix, method = "Z-score"))
#mean = 21.09 median = 3
summary(colCounts(beerall_rrmatrix, method = "Z-score"))
#mean = 11.77 median = 2

#N could be 12 but as per our EDA ideal value should be more than this.
#Hence filtering the data by taking beers with 50 or above reviews and users having 30 or more reviews made
#Filtering data for beer with >=50 reviews
beer_review_count_suset <- subset(beer_review_count, beer_review_count$total_beer_reviews >= 50)
#visualising our subset
ggplot(beer_review_count_suset, aes(x = total_beer_reviews)) + geom_bar()

#Filtering data for users with >=30 reviews made
user_reviews_count<- beer %>% group_by(review_profilename) %>% summarise(total_user_reviews=n()) 
user_reviews_count_subset<-subset(user_reviews_count,user_reviews_count$total_user_reviews>=30)

#Identifying important beers based on our above criteria

important_beers <- merge(beer, beer_review_count_suset, by.x = "beer_beerid", by.y = "beer_beerid")
important_beers <- merge(important_beers, user_reviews_count_subset, by.x = "review_profilename", by.y = "review_profilename")
summary(important_beers)

#Converting dataframe to realRatingMatrix
sum(is.na(important_beers))
beers_rrmatrix <- as(important_beers[,c(1,2,3)], "realRatingMatrix")
class(beers_rrmatrix)
#Checking realRatingMatrix

dimnames(beers_rrmatrix)
rowCounts(beers_rrmatrix)
colCounts(beers_rrmatrix)
rowMeans(beers_rrmatrix)

#coercing matrix to dataframe
beers_df <- as(beers_rrmatrix, "data.frame")
str(beers_df)
summary(beers_df)

#2. Data Exploration
#1. Determine how similar the first ten users are with each other and visualise it

similar_users <- similarity(beers_rrmatrix[1:10,], method = "cosine", which = "users")
as.matrix(similar_users)
#visualising similartiy matrix
image(as.matrix(similar_users), main = "User Similarity")

#2. Compute and visualise the similarity between the first 10 beers

similar_beers <- similarity(beers_rrmatrix[,1:10], method = "cosine", which = "items")
as.matrix(similar_beers)
#visualising similartiy matrix
image(as.matrix(similar_beers), main = "Beer Similarity")

#3. What are the unique values of ratings?

beers_df %>% group_by(rating) %>% summarise(rating_frequency = n()) %>% nrow()
# 9 unique values of rating
#Checking frequency of these 9 unique ratings
beers_df %>% group_by(rating) %>% summarise(rating_frequency = n()) %>% View()
#Rating 4.0 and 4.5 are most common. 1.0 and 1.5 least common

#4. Visualise the rating values and notice:
#(i) The average beer ratings
avg_beer_rating <- beers_df %>% group_by(item) %>% summarise(average_rating = mean(rating))
qplot(avg_beer_rating$average_rating, geom = "histogram") + stat_bin(binwidth = .1) + labs(x = "Ratings", y = "# of Beers")
summary(avg_beer_rating$average_rating)
#average beer rating (mean) = 3.875 / median = 3.901, Distribution is almost normal. Slightly left skewed

# checking average rating on complete dataset
avg_beer_ratings_all<- beer %>% group_by(beer_beerid) %>% summarise(average_rating = mean(review_overall))
qplot(avg_beer_ratings_all$average_rating, geom = "histogram") + stat_bin(binwidth = .1) + labs(x = "Ratings", y = "# of Beers")
summary(avg_beer_ratings_all$average_rating)
#Average beer rating (mean) = 3.671 / median = 3.800

#(ii) The average user ratings
avg_user_ratings <- beers_df %>% group_by(user) %>% summarise(average_rating = mean(rating))
qplot(avg_user_ratings$average_rating, geom = "histogram") + stat_bin(binwidth = .1) + labs(x = "Ratings", y = "# of Users")
summary(avg_user_ratings$average_rating)
#Average User rating (Mean) = 3.806 / Median = 3.870

# checking average user rating on complete beer dataset
avg_user_ratings_all <- beer %>% group_by(review_profilename) %>% summarise(average_rating = mean(review_overall))
qplot(avg_user_ratings_all$average_rating, geom = "histogram") + stat_bin(binwidth = .1) + labs(x = "Ratings", y = "# of Users")
summary(avg_user_ratings_all$average_rating)
#Average User rating (Mean) = 3.910 / Median = 4.000

#(iii) The average number of ratings given to the beers
avg_beer_reviews <- important_beers %>% group_by(beer_beerid) %>% summarise(average_reviews = 
                                                                              mean(total_beer_reviews))
qplot(avg_beer_reviews$average_reviews, geom = "histogram") + stat_bin(binwidth = .1) + labs(x = "Ratings", y = "# of Beers")
summary(avg_beer_reviews$average_reviews)
#Average number of ratings given to beers is 143 from the chosen subset with derived value of N

#Checking average rating on the entire dataset
avg_beer_reviews_all <- beer_review_count %>% group_by(beer_beerid) %>% summarise(average_reviews = 
                                                                              mean(total_beer_reviews))
qplot(avg_beer_reviews_all$average_reviews, geom = "histogram") + stat_bin(binwidth = .1) + labs(x = "Ratings", y = "# of Beers")
summary(avg_beer_reviews_all$average_reviews)
#Average number of rating in this data set is 11.77. This is due to the huge variation in the primary dataset
View(important_beers)


#(iv) The average number of ratings given by the users
avg_user_reviews <- important_beers %>% group_by(review_profilename) %>% summarise(average_reviews = 
                                                                                     mean(total_user_reviews))
qplot(avg_user_reviews$average_reviews, geom = "histogram") + stat_bin(binwidth = .1) + 
  labs(x = "Ratings", y = "# of Users")
summary(avg_user_reviews$average_reviews)
#Average number of ratings given by users is 120.9 from the chosen subset with derived value of N

#Checking average rating by users on the entire dataset
avg_user_reviews_all <- user_reviews_count %>% group_by(review_profilename) %>% summarise(average_reviews = 
                                                                                     mean(total_user_reviews))
qplot(avg_user_reviews_all$average_reviews, geom = "histogram") + stat_bin(binwidth = .1) + 
  labs(x = "Ratings", y = "# of Users")
summary(avg_user_reviews_all$average_reviews)
#Average number of rating in this data set is 21.09.

#Visualising RealRating matrix of Beers

qplot(getRatings(beers_rrmatrix), binwidth = 1, main = "Histogram of ratings", xlab = "Rating")
summary(getRatings(beers_rrmatrix))
qplot(getRatings(normalize(beers_rrmatrix, method = "Z-score")),main = "Histogram of normalized ratings", xlab = "Rating") 
summary(getRatings(normalize(beers_rrmatrix, method = "Z-score")))
#Slightly right skewed

qplot(rowCounts(beers_rrmatrix), binwidth = 10, main = "Beers rated on average", xlab = "# of Users", 
      ylab = "# of Beers rated")
#Most users rate very less number of beers. It seems a bit obvious because people are genrally not very 
#experimentative when it comes to drinks. If they like something they will rate it and continue to consume
#if they do not then they might rate it and go back to their preferred brand.

#Same observations using hist function
hist(rowCounts(beers_rrmatrix), breaks=50)
hist(colCounts(beers_rrmatrix), breaks=20)
hist(rowMeans(beers_rrmatrix), breaks=20)

#3. Recommendation Models
#1. Divide your data into training and testing datasets
#   Experiment with 'split' and 'cross-validation' evaluation schemes

#scheme1 with 75/25 (train/test) data using split without cross validation and good rating as 4
model1 <- evaluationScheme(beers_rrmatrix, method = "split", train = .75, k=1, given = -1, goodRating = 4)
model1

#scheme2 using cross validation (5 fold) and good rating as 4
model2 <- evaluationScheme(beers_rrmatrix, method = "cross-validation", k=5, given = -1, goodRating = 4)
model2

#2. Build IBCF and UBCF models
algorithms <- list("user-based CF" = list(name = "UBCF", param = list(normalize = "Z-score", 
                                                                      method = "Cosine", nn=30)),
                   "item-based CF" = list(name = "IBCF", param = list(normalize = "Z-score")))
#Evaluating algorithms and predicting next n beers
results1 <- evaluate(scheme1, algorithms, n = c(1,3,5,10,15,20))
class(results1)

results2 <- evaluate(scheme2, algorithms, n = c(1,3,5,10,15,20))
class(results2)

#3. Compare the performance of the two models and suggest the one that should be deployed
#Drawing ROC Curve

plot(results1, annotate = 1:4, legend = "topleft")
plot(results2, annotate = 1:4, legend = "topleft")

#UBCF seems to get better then IBFC especially with higher values of n

#4. Give the names of the top 5 beers that you would recommend to the users "cokes", "genog" & "giblet"
#Making recommendations using UBCF

r <- Recommender(beers_rrmatrix, method = "UBCF")
r

recommend_cokes <- predict(r, beers_rrmatrix['cokes'], n=5)
as(recommend_cokes, "list")

#recommendation for cokes: "7971" "645"  "1346" "582"  "2041"

recommend_genog <- predict(r, beers_rrmatrix['genog'], n = 5)
as(recommend_genog, "list")

#Recommendation for geong: "57908" "1160"  "1093"  "1161"  "1445"

recommend_giblet <- predict(r, beers_rrmatrix['giblet'], n = 5)
as(recommend_giblet, "list")

#Recommendation for giblet: "19960" "4083"  "582"   "11757" "2041" 