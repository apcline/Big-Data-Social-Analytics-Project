library(tidytext)
library(sentimentr)
library(ggplot2)
library(tm)
library(stringr)
library(SentimentAnalysis)
library(textTinyR)
library(tm)
library(slam)
library(dplyr)
library(rlang)
library(wordcloud)
library(RColorBrewer)

# Load and inspect the dataset
hotel_reviews <- read.csv("Reviews_sample.csv")
head(hotel_reviews)

# Data cleaning and preparation
# Converting text to lower case
hotel_reviews$Positive_Review <- tolower(hotel_reviews$Positive_Review)
hotel_reviews$Negative_Review <- tolower(hotel_reviews$Negative_Review)
# Tokenize text and remove punctuation
hotel_reviews$Positive_Review <- sapply(hotel_reviews$Positive_Review, function(text) {
  words <- unlist(str_split(text, "\\s+"))
  words <- str_replace_all(words, "[[:punct:]]", "")
  words <- words[!sapply(words, function(word) any(grepl("[0-9]", word)))]
  words <- words[!tolower(words) %in% stopwords("en")]
  words <- words[nchar(words) > 1]
  words
}, USE.NAMES = FALSE)

hotel_reviews$Negative_Review <- sapply(hotel_reviews$Negative_Review, function(text) {
  words <- unlist(str_split(text, "\\s+"))
  words <- str_replace_all(words, "[[:punct:]]", "")
  words <- words[!sapply(words, function(word) any(grepl("[0-9]", word)))]
  words <- words[!tolower(words) %in% stopwords("en")]
  words <- words[nchar(words) > 1]
  words
}, USE.NAMES = FALSE)
# POS tagging and lemmatization
hotel_reviews$Positive_Review <- sapply(hotel_reviews$Positive_Review, function(text) {
  tokens <- removePunctuation(tolower(unlist(str_split(text, "\\s+"))))
  tokens <- tokens[!sapply(tokens, function(token) any(grepl("[0-9]", token)))]
  tokens <- tokens[!tokens %in% stopwords("en")]
  tokens <- tokens[nchar(tokens) > 1]
  # POS tagging
  pos_tags <- textTokenToTag(removePunctuation(tolower(unlist(str_split(text, "\\s+")))))
  
  # Lemmatization
  lemmas <- lemmatize_words(tokens, pos_tags)
  
  lemmas
}, USE.NAMES = FALSE)

hotel_reviews$Negative_Review <- sapply(hotel_reviews$Negative_Review, function(text) {
  tokens <- removePunctuation(tolower(unlist(str_split(text, "\\s+"))))
  tokens <- tokens[!sapply(tokens, function(token) any(grepl("[0-9]", token)))]
  tokens <- tokens[!tokens %in% stopwords("en")]
  tokens <- tokens[nchar(tokens) > 1]
  
  # POS tagging
  pos_tags <- textTokenToTag(removePunctuation(tolower(unlist(str_split(text, "\\s+")))))
  
  # Lemmatization
  lemmas <- lemmatize_words(tokens, pos_tags)
  
  lemmas
}, USE.NAMES = FALSE)

# -------------------------------------------
# Analysis and Modeling 
# -------------------------------------------

# Word Cloud -------------------

# Assuming 'reviews_df' is your data frame containing Positive_Review and Negative_Review columns
reviews_positive <- tolower(hotel_reviews$Positive_Review)
reviews_negative <- tolower(hotel_reviews$Negative_Review)

# Create a text corpus for positive reviews
corpus_positive <- Corpus(VectorSource(reviews_positive))

# Preprocess the text for positive reviews
corpus_positive <- tm_map(corpus_positive, removePunctuation)
corpus_positive <- tm_map(corpus_positive, removeWords, stopwords("english"))
corpus_positive <- tm_map(corpus_positive, stripWhitespace)

# Create a text corpus for negative reviews
corpus_negative <- Corpus(VectorSource(reviews_negative))

# Preprocess the text for negative reviews
corpus_negative <- tm_map(corpus_negative, removePunctuation)
corpus_negative <- tm_map(corpus_negative, removeWords, stopwords("english"))
corpus_negative <- tm_map(corpus_negative, stripWhitespace)

# Generate word cloud for positive reviews
set.seed(1234)  # for reproducibility
wordcloud(words = unlist(corpus_positive), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"),
          main = "Word Cloud for Positive Reviews")

# Generate word cloud for negative reviews
set.seed(1234)  # for reproducibility
wordcloud(words = unlist(corpus_negative), max.words = 100, random.order = FALSE, colors = brewer.pal(8, "Dark2"),
          main = "Word Cloud for Negative Reviews")


# Sentiment Analysis ------------------

reviews_positive <- hotel_reviews$Positive_Review
reviews_negative <- hotel_reviews$Negative_Review

# Function to calculate sentiment scores
calculate_sentiment <- function(reviews) {
  sentiment_scores <- sapply(reviews, function(review) analyzeSentiment(as.character(review))$Sentiment)
  return(sentiment_scores)
}

# Calculate sentiment scores for positive and negative reviews
sentiment_positive <- calculate_sentiment(reviews_positive)
sentiment_negative <- calculate_sentiment(reviews_negative)

# Add sentiment scores to the original data frame
hotel_reviews$Sentiment_Positive <- sentiment_positive
hotel_reviews$Sentiment_Negative <- sentiment_negative

# Display the head of the updated data frame
head(hotel_reviews)
