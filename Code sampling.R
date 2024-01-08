
excluded_columns <- c("Hotel_Address","Additional_Number_of_Scoring","Tags","days_since_review","lat","lng")  # replace with your column names

# Drop the excluded columns from the dataset
Hotel_Reviews <- Hotel_Reviews[, !(names(Hotel_Reviews) %in% excluded_columns)]


# Ensure the dataset has more than 10,000 rows
if (nrow(Hotel_Reviews) > 10000) {
  # Sample 10,000 random rows
  set.seed(123)  # for reproducibility
  sampled_rows <- sample(nrow(Hotel_Reviews), 10000)
  sampled_data <- Hotel_Reviews[1:10000,]
  
  # Save the sampled data (optional)
  write.csv(sampled_data, "Reviews_sample.csv", row.names = FALSE)
} else {
  print("The dataset has less than 10,000 rows.")
}
