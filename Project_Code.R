library(tidytext)
library(sentimentr)
library(dplyr)
library(ggplot2)
library(tm)
library(stringr)

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
# POS tagging and lemmatization (using the 'textTinyR' package)
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

