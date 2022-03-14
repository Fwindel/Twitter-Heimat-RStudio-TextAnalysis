#11/20/2020 - 
# Scrape Heimat related tweets since 2018
# This script is to scrape tweets connected to Heimat
# add date to the .rdata files 

install.packages("devtools")
install_github("mkearney/rtweet")
install.packages("httpuv")
library(devtools)
library(httpuv)
library(rtweet)

#Heimat for time frame: 2018-01-01 to present
#https://github.com/cjbarrie/academictwitteR

install.packages("academictwitteR")
library(academictwitteR)

set_bearer()
#In .Renviron* run: 
#TWITTER_BEARER= 

#restart Rstudio: under session tab
library(academictwitteR)
#then run: 
heimat112321 <-
  get_all_tweets(
    query = "#heimat",
    start_tweets = "2018-01-01T00:00:00Z",
    end_tweets = "2021-11-22T00:00:00Z",
    file = "heimattweets",
    data_path = "data/",
    n = 10000000,
  )

save(heimat112321, file = "heimat112321.RData")

######get tweets for one week 
#consumer keys etc. last retrieved 11/19/21
app_name<-"#Heimat"
consumer_key<-""
consumer_secret<-""
access_token<-""
access_token_secret<-""
create_token(app=app_name,
             consumer_key=consumer_key, consumer_secret=consumer_secret,
             access_token = access_token, access_secret = access_token_secret)

#heimat 11/19/2021 for one week

heimat111921<-search_tweets("#heimat", n=100000000, include_rts = FALSE, 
                            retryonratelimit = TRUE)
save(heimat111921, file = "heimat111921.RData")

