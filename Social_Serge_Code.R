# Load necessary libraries
library(tidytext)
library(sentimentr)
library(dplyr)
library(ggplot2)
library(tm)
library(stringr)
library(SentimentAnalysis)
library(lubridate)
library(wordcloud)
library(tidyr)

# Load and inspect the dataset
hotel_reviews <- read.csv("Reviews_sample.csv")
head(hotel_reviews)

# Function to remove non-Latin characters
remove_non_latin <- function(text) {
  text <- gsub("[^\\p{Latin}\\d\\s\\p{P}]", "", text, perl=TRUE)
  return(text)
}

# Extend the stop words list
extended_stopwords <- c(stopwords("en"), "the", "was", "to", "as", "c", "t", "very")

# Data cleaning and preparation
# Function to clean text
clean_text <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- str_replace_all(text, "[[:punct:]]", "")
  text <- str_replace_all(text, "[[:digit:]]", "")
  text <- removeWords(text, extended_stopwords)
  text <- str_replace_all(text, "[[:space:]]+", " ")
  return(text)
}

# Apply the functions to remove non-Latin characters and clean text for review columns
hotel_reviews$Positive_Review <- vapply(hotel_reviews$Positive_Review, function(text) {
  text <- remove_non_latin(text)
  clean_text(text)
}, character(1))

hotel_reviews$Negative_Review <- vapply(hotel_reviews$Negative_Review, function(text) {
  text <- remove_non_latin(text)
  clean_text(text)
}, character(1))

# To see the cleaning process is working or not
hotel_reviews %>%
  select(Negative_Review) %>%
  head()

# Parse the Review_Date column
hotel_reviews$Review_Date <- mdy(hotel_reviews$Review_Date)

# Verify the structure of the data to confirm the changes
str(hotel_reviews)

####################
# Overall Analysis
####################

#################################
# EDA (Explanatory Data Analysis)
#################################

# Time Series of the Hotel Rating
# Aggregate average scores by month
monthly_ratings <- hotel_reviews %>%
  mutate(Month = floor_date(Review_Date, "month")) %>%
  group_by(Month) %>%
  summarise(Average_Score = mean(Reviewer_Score, na.rm = TRUE))

# Plot the time series
ggplot(monthly_ratings, aes(x = Month, y = Average_Score)) +
  geom_line() +
  labs(title = "Monthly Average Hotel Ratings",
       x = "Month",
       y = "Average Rating") +
  theme_minimal()

# Word Frequency Analysis
# Unnest tokens
positive_words <- hotel_reviews %>%
  unnest_tokens(word, Positive_Review)

negative_words <- hotel_reviews %>%
  unnest_tokens(word, Negative_Review)

# Count words and plot
positive_word_count <- positive_words %>%
  count(word, sort = TRUE)

negative_word_count <- negative_words %>%
  count(word, sort = TRUE)

# Plotting top 10 words from positive reviews
top_n(positive_word_count, 10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 words in Positive Reviews")

# Plotting top 10 words from negative reviews
top_n(negative_word_count, 10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 words in Negative Reviews")

# Creating wordcloud
# Filter out the top 100 words for each sentiment for the word cloud
top_positive_words <- head(positive_word_count, 50)
top_negative_words <- head(negative_word_count, 50)

# Create word clouds
# For positive reviews
set.seed(116) # for reproducibility
wordcloud(words = top_positive_words$word, freq = top_positive_words$n, random.order = FALSE, 
          scale = c(3, 0.4), max.words = 50, colors = brewer.pal(6, "Dark2"))

# For negative reviews
set.seed(120) # for reproducibility
wordcloud(words = top_negative_words$word, freq = top_negative_words$n, random.order = FALSE,
          scale = c(3, 0.5), max.words = 50, colors = brewer.pal(4, "Dark2"))

####################
# Sentiment Analysis
####################

# Assuming you have cleaned your reviews and they are in a column named `Positive_Review` and `Negative_Review`
# Sentiment analysis using sentimentr
hotel_reviews$Positive_Sentiment <- sentiment(hotel_reviews$Positive_Review)
hotel_reviews$Negative_Sentiment <- sentiment(hotel_reviews$Negative_Review)

# Prepare the data for plotting
positive_sentiments <- hotel_reviews %>%
  select(Review_Date, Positive_Sentiment) %>%
  unnest(Positive_Sentiment) %>%
  mutate(Review_Type = 'Positive')

negative_sentiments <- hotel_reviews %>%
  select(Review_Date, Negative_Sentiment) %>%
  unnest(Negative_Sentiment) %>%
  mutate(Review_Type = 'Negative')

combined_sentiments <- rbind(positive_sentiments, negative_sentiments)

# Plot the overall sentiment distribution
ggplot(combined_sentiments, aes(x = sentiment, fill = Review_Type)) +
  geom_histogram(binwidth = 0.1, position = "identity", alpha = 0.6) +
  labs(title = "Sentiment Distribution in Hotel Reviews",
       x = "Sentiment Score",
       y = "Frequency") +
  scale_fill_manual(values = c("Positive" = "blue", "Negative" = "red")) +
  theme_minimal()

# Ensure that the Review_Date is in the Date format
combined_sentiments$Review_Date <- as.Date(combined_sentiments$Review_Date)

# Plot sentiments over time if you have date data
# (Make sure to group by Review_Date and Review_Type if you have multiple reviews per day)
sentiments_over_time <- combined_sentiments %>%
  group_by(Review_Date, Review_Type) %>%
  summarise(Average_Sentiment = mean(sentiment, na.rm = TRUE))

ggplot(sentiments_over_time, aes(x = Review_Date, y = Average_Sentiment, color = Review_Type)) +
  geom_line() +
  labs(title = "Sentiment Over Time",
       x = "Date",
       y = "Sentiment Score") +
  scale_color_manual(values = c("Positive" = "blue", "Negative" = "red")) +
  theme_minimal()
