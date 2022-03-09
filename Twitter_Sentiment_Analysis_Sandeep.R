#Lexicon based sentiment analysis on twitter data
#libraries required for Lexicon based sentiment analysis and visualization

#loading the library
library(rtweet)
library(readr)
library(plyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(tm)

# Setting current working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Search Twitter for 1000 tweets
#twitter.data <- search_tweets("covid",n=1000,lang="en", include_rts=FALSE)

#write_csv(twitter.data,"data.csv")

#Load tweets data from existing data file.
twitter.data.df <- read_csv("data.csv")

#GetText
tweets.df <- as.data.frame(twitter.data.df$text)
colnames(tweets.df)[1]<-"text"

###
###Text Cleaning###
###

#for Mac
tweets.df <- sapply(tweets.df$text, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))

#for Windows based OS
#tweets.df <- sapply(tweets.df,function(row) iconv(row, "latin1", "ASCII", sub=""))

#common for any platform
tweets.df <- gsub("@\\w+", "", tweets.df)
tweets.df <- gsub("#\\w+", '', tweets.df)
tweets.df <- gsub("RT\\w+", "", tweets.df)
tweets.df <- gsub("http.*", "", tweets.df)
tweets.df <- gsub("RT", "", tweets.df)
tweets.df <- sub("([.-])|[[:punct:]]", "\\1", tweets.df)
tweets.df <- sub("(['])|[[:punct:]]", "\\1", tweets.df)
tweets.df <- gsub("&amp", "", tweets.df)
tweets.df <- gsub("\n","",tweets.df)

#View the cleaned Data
#View(tweets.df)

#Reading the Lexicon positive and negative words
pos <- readLines("positive_words.txt")
neg <- readLines("negative_words.txt")

#function to calculate sentiment score
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  # create simple array of scores with laply
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    # remove punctuation
                    sentence <- gsub("[[:punct:]]", "", sentence)
                    # remove control characters
                    sentence <- gsub("[[:cntrl:]]", "", sentence)
                    # remove digits
                    sentence <- gsub('\\d+', '', sentence)
                    # remove &amp
                    sentence <- gsub("&amp","",sentence)
                    #convert to lower
                    sentence <- tolower(sentence)
                    
                    # split sentence into words with str_split (stringr package)
                    word.list <- str_split(sentence, "\\s+")
                    words <- unlist(word.list)
                    
                    # compare words to the dictionaries of positive & negative terms
                    pos.matches <- match(words, pos)
                    neg.matches <- match(words, neg)
                    
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    
                    # final score
                    score <- sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}
#sentiment score
scores_twitter <- score.sentiment(tweets.df, pos, neg, .progress='text')

#Summary of the sentiment scores
summary(scores_twitter) %>% knitr::kable()

#Convert sentiment scores from numeric to character to enable the gsub function 
scores_twitter$score_chr <- as.character(scores_twitter$score)

#After looking at the summary(scores_twitter$score) decide on a threshold for the sentiment labels
scores_twitter$score_chr <- gsub("^0$", "Neutral", scores_twitter$score_chr)
scores_twitter$score_chr <- gsub("^1$|^2$|^3$|^4$", "Positive", scores_twitter$score_chr)
scores_twitter$score_chr <- gsub("^5$|^6$|^7$|^8$|^9$|^10$|^11$|^12$", "Very Positive", scores_twitter$score_chr)
scores_twitter$score_chr <- gsub("^-1$|^-2$|^-3$|^-4$", "Negative", scores_twitter$score_chr)
scores_twitter$score_chr <- gsub("^-5$|^-6$|^-7$|^-8$|^-9$|^-10$|^-11$|^-12$", "Very Negative", scores_twitter$score_chr)

#View(scores_twitter)

#Convert score_chr to factor for visualizations
scores_twitter$score_chr <- as.factor(scores_twitter$score_chr)

#plot to show number of negative, positive and neutral comments
ggplot(scores_twitter, aes(x=score_chr))+geom_bar()

# Create a Corpus
tweet_corpus <- Corpus(VectorSource(tweets.df))

# Remove stopwords
tweet_corpus <- tm_map(tweet_corpus,removeWords,stopwords("english"))
# Custom stopwords as a character vector
tweet_corpus <- tm_map(tweet_corpus,removeWords,c("the","will","can"))

# Text Stemming which reduces words to their root form
tweet_corpus <- tm_map(tweet_corpus,stemDocument)

tweet_corpus <- TermDocumentMatrix(tweet_corpus)
matrix <- as.matrix(tweet_corpus)
words <- sort(rowSums(matrix),decreasing = TRUE)
tweet_corpus <- data.frame(word = names(words),freq = words)

# Generating wordcloud
wordcloud(words = tweet_corpus$word,freq = tweet_corpus$freq,min.freq = 10,max.words = 80,random.order = FALSE,rot.per = 0.35,scale = c(3,0.5),colors = brewer.pal(8,"Dark2"))

#plot to show number of negative, positive and neutral comments
ggplot(scores_twitter, aes(x=score_chr))+geom_bar()

#writing to csv file
write_csv(scores_twitter, file = "sentiment_score.csv")
