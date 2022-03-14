####This is a work in progress
####I'm working on figuring out sentiment analysis for German-speaking dataset
heimatsentiment <- heimatclean

install.packages("textdata")
library(textdata)
library(udpipe)
model <- udpipe_download_model(language = "german")
udpipe_german <- udpipe_load_model(file = model$file_model)

udpipeannotation <- udpipe_annotate(udpipe_german, heimatsentiment$text)
x <- data.frame(s)

senti <- get_sentiments("nrc")
head(senti)

readAndflattenSentiWS <- function(heimatsentiment) { 
  words = readLines(heimatsentiment, encoding="UTF-8")
  words <- sub("\\|[A-Z]+\t[0-9.-]+\t?", ",", words)
  words <- unlist(strsplit(words, ","))
  words <- tolower(words)
  return(words)
}
pos.words <- c(scan("positive-words.txt",what='character', comment.char=';', quiet=T), 
               readAndflattenSentiWS("SentiWS_v1.8c_Positive.txt"))
neg.words <- c(scan("negative-words.txt",what='character', comment.char=';', quiet=T), 
               readAndflattenSentiWS("SentiWS_v1.8c_Negative.txt"))

score.sentiment = function(sentences, pos.words, neg.words, .progress='none') {
  # ... see OP ...
}

sample <- c("ich liebe dich. du bist wunderbar",
            "Ich hasse dich, geh sterben!", 
            "i love you. you are wonderful.",
            "i hate you, die.")
(test.sample <- score.sentiment(sample, 
                                pos.words, 
                                neg.words))
#   score                              text
# 1     2 ich liebe dich. du bist wunderbar
# 2    -2      ich hasse dich, geh sterben!
# 3     2    i love you. you are wonderful.
# 4    -2                  i hate you, die.
Share
Follow
edited Mar 1, 2014 at 21:39




################################


# load the wordlists
pos.words = scan("~/positive-words.txt",what='character', comment.char=';')
neg.words = scan("~/negative-words.txt",what='character', comment.char=';')

# bring in the sentiment analysis algorithm
# we got a vector of sentences. plyr will handle a list or a vector as an "l" 
# we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{ 
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) 
  {
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, 
  pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# and to see if it works, there should be a score...either in German or in English
sample = c("ich liebe dich. du bist wunderbar","I hate you. Die!");sample
test.sample = score.sentiment(sample, pos.words, neg.words);test.sample



############################ TIME ANALYSIS ############################
#https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967
#https://jtr13.github.io/cc21/twitter-sentiment-analysis-in-r.html

#load packages
library(twitteR)
library(ROAuth)
library(hms)
library(lubridate) 
library(tidytext)
library(tm)
library(wordcloud)
library(igraph)
library(glue)
library(networkD3)
library(rtweet)
library(plyr)
library(stringr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(dplyr)  
library(hms)
library(lubridate) 
library(magrittr)
library(tidyverse)
library(janeaustenr)
library(widyr)

#libraries, dictionaries and packages to evaluate the emotion prevalent in a text: e.g. Bing (will use this), AFINN, nrc)
#question do they work with german language?
get_sentiments("bing") %>% filter(sentiment=="positive")
get_sentiments("bing") %>% filter(sentiment=="negative")
#then run the sentiment analysis
heimatsentiments <- heimatclean %>%
  inner-join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


