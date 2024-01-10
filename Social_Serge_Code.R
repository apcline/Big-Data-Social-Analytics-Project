# Load necessary libraries
library(tidytext)
library(sentimentr)
library(dplyr)
library(ggplot2)
library(tm)
library(stringr)
library(SentimentAnalysis)
library(lubridate)

# Load and inspect the dataset
hotel_reviews <- read.csv("Reviews_sample.csv")
head(hotel_reviews)

# Function to remove non-Latin characters
remove_non_latin <- function(text) {
  text <- gsub("[^\\p{Latin}\\d\\s\\p{P}]", "", text, perl=TRUE)
  return(text)
}

# Data cleaning and preparation
# Converting text to lower case and removing non-Latin characters
hotel_reviews$Positive_Review <- tolower(hotel_reviews$Positive_Review)
hotel_reviews$Positive_Review <- sapply(hotel_reviews$Positive_Review, remove_non_latin)

hotel_reviews$Negative_Review <- tolower(hotel_reviews$Negative_Review)
hotel_reviews$Negative_Review <- sapply(hotel_reviews$Negative_Review, remove_non_latin)

# Additional text preprocessing
# Tokenize text, remove punctuation, numbers, stopwords, and short words
process_text <- function(text) {
  words <- unlist(str_split(text, "\\s+"))
  words <- str_replace_all(words, "[[:punct:]]", "")
  words <- words[!sapply(words, function(word) any(grepl("[0-9]", word)))]
  words <- words[!tolower(words) %in% stopwords("en")]
  words <- words[nchar(words) > 1]
  return(words)
}

hotel_reviews$Positive_Review <- sapply(hotel_reviews$Positive_Review, process_text, USE.NAMES = FALSE)
hotel_reviews$Negative_Review <- sapply(hotel_reviews$Negative_Review, process_text, USE.NAMES = FALSE)

# Assuming your Review_Date is in the format "m/d/Y" (e.g., "8/3/2017")
# Adjust the format accordingly if it's different
hotel_reviews$Review_Date <- mdy(hotel_reviews$Review_Date)
str(hotel_reviews)

# ... (additional POS tagging and lemmatization code if necessary)

# Continue with your analysis...

# Universal Analysis
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
