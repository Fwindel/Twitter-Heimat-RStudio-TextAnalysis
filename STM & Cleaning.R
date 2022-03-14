#####STM and cleaning

#####load heimatcleanDE.RData in Dropbox folder --> Rstudio --> Data Cleaning & STM

####run STM 
install.packages("stm")
install.packages("streamR")
install.packages("RCurl")
install.packages("bitops")
install.packages("rjson")
install.packages("NLP")
install.packages('tm')
install.packages('geometry')
install.packages('Rtsne')
install.packages('rsvd')
library(tm)
library(bitops)
library(RCurl)
library(rjson)
library(ndjson)
library(streamR)
library(tidytext)
library(stminsights)
library(stringr)
library (Rtsne)
library(rsvd)
library(geometry)
library(igraph)
library(dplyr)
library(tm)
library(ggplot2)
library(RColorBrewer)
library(stm)

##this is the first run after general cleaning - on march 7
processed <- textProcessor(heimatclean$text,
                           removestopwords = TRUE, 
                           stem = TRUE,
                           language = "de",
                           metadata = heimatclean)

#Prepare
plotRemoved(processed$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 10)

#Removing 75878 of 82512 terms (152473 of 618143 tokens) due to frequency 
#Removing 14 Documents with No Words 
#Your corpus now has 50744 documents, 6634 terms and 465670 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta

#inspect to make sure pre-processing went ok 
head(docs)#this tells you how many words are in what position 
head(vocab)
head(meta)

#fit model 1 k=0
#March 7, 2022
set.seed(01234)

heimatstm1 <- stm(documents = out$documents, vocab = out$vocab, K= 0, 
                  data = out$meta, init.type = "Spectral")

labelTopics (heimatstm1)#66

plot(heimatstm1 , type = "summary", text.cex = 0.3)

save(heimatstm1, file = "heimatstm1.RData") #This is the STM outcome, not the dataset
#saved in Data cleaning & STM -> STM analysis
library(stminsights)
run_stminsights()

#exmamine the 66 topics on stm insights
#I examined the 50 tweets for each of the 66 topics and didn't find anything to clean. 
#There are some tweets that look very similar but are not duplicates
#I'll leave this in since it's a starting point to understand how Heimat is discussed and used for on Twitter
#I think that ads etc. are probably a big part of this and indicate the capitalist use of Heimat

#Print texts for the ten top topics: 12, 46, 47, 38, 13, 18, 16, 61, 45, 15
library(wordcloud)
library(stm)
topic12 <- findThoughts(heimatstm1, texts=meta$text, topics=12, n=50)
#put STM model in findThoughts(STM model, ....
#the meta df needs to come from the 1619 project
plotQuote(topic12$docs[[1]], main = "Topic 12")
sink("topic12_50.txt")
print("topic 12")
print(topic12[["docs"]][["Topic 12"]])
sink()



#######do this later!

#Look for best fit K here? (guess: between 65 and 80)
set.seed(831)
heimat.searchk <- searchK(documents = out$documents,
                      vocab = out$vocab,
                      K = 50:80,
                      init.type = "Spectral",
                      data = out$meta)
plot(heimat.searchk) #plot too large - because the window for the plot was too small - making Rstudio full screen on my big screen and making plot window bigger solved it
save(heimat.searchk, file = "heimat_searchk.R") #This is the searchK outcome
#diagnostic values by number of topics saved in folder


#to determine K, look at held-out likelihood, residulas,  and semantic coherence:
#held-out likelihood is highest 50 to 54 and 56, and much lower but still higher than others at 67. The residuals are lowest around 55 and from 57 on,
#67 is still also low. 
#high semantic coherence is at 55, and from 56 on, with 67 and 68, 71 being very high
#Based on the heimat.searchk plot , a choice of 67 may provide a good tradeoff.


#"Held-out likelihood refers to the log probability of topics in the test set correctly replicating topics in the training set. 
#The lower bound refers to the lower bound of the marginal log likelihood. Residuals refers to the difference between expected and predicted topic predictions.
#Semantic coherence refers to the co-occurrence of words in a given topic – where co-occurring words also comprise the most likely words of a given topic, 
#that topic has high semantic coherence. Figure 1 shows that between 80 and 90 topics produce relatively low residuals and a maximized lower bound, 
#though held-out likelihood and semantic coherence is not optimal. 85 topics were selected as the finalk for the current data set and model." (Gab paper)

run_stminsights()
#######################

####the following I didn't have to do. I needed to figure out the diagnostic value, and I have it now. 

#######################

##now I think this one would work as well - can do for other dataset or cleaner dataset
storage <- searchK(out$documents, out$vocab, K = c(3,45,55,60,65,70,75,80,85), data = meta)
plot(storage) #plot too large - because the window for the plot was too small - making Rstudio full screen on my big screen and making plot window bigger solved it
knitr::kable(storage$results)
#never did this: save(storagae, file = "storage.RData") 

#run actual stms with different Ks

##########STM with K= 80###############
set.seed(0389475)
temp <- textProcessor(heimatclean$text, metadata = heimatclean)

#Prepare
plotRemoved(temp$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(temp$documents, temp$vocab, temp$meta, lower.thresh = 10)


#Removing 75819 of 82567 terms (152271 of 856820 tokens) due to frequency 
#Removing 7 Documents with No Words 
#Your corpus now has 50753 documents, 6748 terms and 704549 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta
head(docs)
head(vocab)
head(meta)

heimatstm80 <- stm(documents = out$documents, vocab = out$vocab, K= 80, 
                  data = out$meta, init.type = "Spectral")

labelTopics (heimatstm80)#80

plot(heimatstm80 , type = "summary", text.cex = 0.3)

save(heimatstm80, file = "heimatstm80.RData") #This is the STM outcome, not the dataset
#move it to Rstudio folder --> Data cleaning & STM --> STM analysis

##########STM with K= 70###############
set.seed(0389475)
temp <- textProcessor(heimatclean$text, metadata = heimatclean)

#Prepare
plotRemoved(temp$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(temp$documents, temp$vocab, temp$meta, lower.thresh = 10)


#Removing 75819 of 82567 terms (152271 of 856820 tokens) due to frequency 
#Removing 7 Documents with No Words 
#Your corpus now has 50753 documents, 6748 terms and 704549 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta
head(docs)
head(vocab)
head(meta)

heimatstm70 <- stm(documents = out$documents, vocab = out$vocab, K= 70, 
                   data = out$meta, init.type = "Spectral")

labelTopics (heimatstm70)#70

plot(heimatstm70 , type = "summary", text.cex = 0.3)

save(heimatstm70, file = "heimatstm70.RData") #This is the STM outcome, not the dataset
#move it to Rstudio folder --> Data cleaning & STM --> STM analysis

##########STM with K= 65 ###############
set.seed(0389475)
temp <- textProcessor(heimatclean$text, metadata = heimatclean)

#Prepare
plotRemoved(temp$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(temp$documents, temp$vocab, temp$meta, lower.thresh = 10)


#Removing 75819 of 82567 terms (152271 of 856820 tokens) due to frequency 
#Removing 7 Documents with No Words 
#Your corpus now has 50753 documents, 6748 terms and 704549 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta
head(docs)
head(vocab)
head(meta)

heimatstm65 <- stm(documents = out$documents, vocab = out$vocab, K= 65, 
                   data = out$meta, init.type = "Spectral")

labelTopics (heimatstm65)#65

plot(heimatstm65 , type = "summary", text.cex = 0.3)

save(heimatstm65, file = "heimatstm65.RData") #This is the STM outcome, not the dataset
#move it to Rstudio folder --> Data cleaning & STM --> STM analysis

##########STM with K= 60###############
set.seed(0389475)
temp <- textProcessor(heimatclean$text, metadata = heimatclean)

#Prepare
plotRemoved(temp$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(temp$documents, temp$vocab, temp$meta, lower.thresh = 10)

#Removing 75819 of 82567 terms (152271 of 856820 tokens) due to frequency 
#Removing 7 Documents with No Words 
#Your corpus now has 50753 documents, 6748 terms and 704549 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta
head(docs)
head(vocab)
head(meta)

heimatstm60 <- stm(documents = out$documents, vocab = out$vocab, K= 60, 
                   data = out$meta, init.type = "Spectral")

labelTopics (heimatstm60)#60

plot(heimatstm60 , type = "summary", text.cex = 0.3)

save(heimatstm60, file = "heimatstm60.RData") #This is the STM outcome, not the dataset
#move it to Rstudio folder --> Data cleaning & STM --> STM analysis

##########STM with K= 55###############
set.seed(0389475)
temp <- textProcessor(heimatclean$text, metadata = heimatclean)

#Prepare
plotRemoved(temp$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(temp$documents, temp$vocab, temp$meta, lower.thresh = 10)


#Removing 75819 of 82567 terms (152271 of 856820 tokens) due to frequency 
#Removing 7 Documents with No Words 
#Your corpus now has 50753 documents, 6748 terms and 704549 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta
head(docs)
head(vocab)
head(meta)

heimatstm55<- stm(documents = out$documents, vocab = out$vocab, K= 55, 
                   data = out$meta, init.type = "Spectral")

labelTopics (heimatstm55)#55

plot(heimatstm55 , type = "summary", text.cex = 0.3)

save(heimatstm55, file = "heimatstm55.RData") #This is the STM outcome, not the dataset
#move it to Rstudio folder --> Data cleaning & STM --> STM analysis

##########STM with K= 50###############
set.seed(0389475)
temp <- textProcessor(heimatclean$text, metadata = heimatclean)

#Prepare
plotRemoved(temp$documents, lower.thresh = seq(1,100, by = 10))
out <- prepDocuments(temp$documents, temp$vocab, temp$meta, lower.thresh = 10)


#Removing 75819 of 82567 terms (152271 of 856820 tokens) due to frequency 
#Removing 7 Documents with No Words 
#Your corpus now has 50753 documents, 6748 terms and 704549 tokens.

docs<- out$documents
vocab<- out$vocab
meta <- out$meta
head(docs)
head(vocab)
head(meta)

heimatstm50<- stm(documents = out$documents, vocab = out$vocab, K= 50, 
                    data = out$meta, init.type = "Spectral")

labelTopics (heimatstm50)#50

plot(heimatstm50 , type = "summary", text.cex = 0.3)

save(heimatstm50, file = "heimatstm50.RData") #This is the STM outcome, not the dataset
#move it to Rstudio folder --> Data cleaning & STM --> STM analysis

run_stminsights()

