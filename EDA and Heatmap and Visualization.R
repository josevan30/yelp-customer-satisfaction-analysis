# ============================================================
# DATA SCIENCE AND MARKETING ANALYTICS
# Yelp Customer Satisfaction Analysis
# Author: Jose
# Purpose: Load prepared Yelp dataset and run initial analysis
# ============================================================


# ============================================================
# 1. LOAD DATA
# ============================================================

data <- read.csv("yelp_v5_holiday_sentiment.csv")


# ============================================================
# 2. FIRST DATA CHECK
# ============================================================

# Show first rows
head(data)

# Show structure: variables, data types, number of observations
str(data)

# Summary statistics
summary(data)

# ============================================================
# 3. SUMMARY STATISTICS TABLE
# ============================================================

summary_table <- data.frame(
  Variable = names(data),
  Mean = sapply(data, mean),
  SD = sapply(data, sd),
  Min = sapply(data, min),
  Max = sapply(data, max)
)

summary_table

# ============================================================
# 4. CHECK DEPENDENT VARIABLE: CUSTOMER SATISFACTION
# satisfied = 1 if review_stars >= 4
# satisfied = 0 if review_stars < 4
# ============================================================

# Count satisfied vs not satisfied
table(data$satisfied)

# Percentage satisfied vs not satisfied
prop.table(table(data$satisfied))


# ============================================================
# 5. CLEAN / FIX VARIABLE TYPES
# ============================================================

# Convert business_price from character to numeric
# Warning about NA is okay because missing/None values become NA
data$business_price <- as.numeric(data$business_price)

# Check if conversion worked
str(data$business_price)


# ============================================================
# 6. CHECK MISSING VALUES
# ============================================================

# Count missing values in business_price
sum(is.na(data$business_price))

# Distribution of price levels, including missing values
table(data$business_price, useNA = "always")


# ============================================================
# 7. INITIAL BUSINESS INSIGHT: PRICE VS SATISFACTION
# ============================================================

# Average satisfaction rate by price level
aggregate(
  satisfied ~ business_price,
  data = data,
  mean
)

# ============================================================
# 8. INITIAL BUSINESS INSIGHT: RESTAURANT ATTRIBUTES
# ============================================================

# Satisfaction by parking availability
aggregate(
  satisfied ~ has_parking,
  data = data,
  mean
)

# Satisfaction by reservation availability
aggregate(
  satisfied ~ takes_reservations,
  data = data,
  mean
)

# Satisfaction by outdoor seating
aggregate(
  satisfied ~ outdoor_seating,
  data = data,
  mean
)

# Satisfaction by TV availability
aggregate(
  satisfied ~ has_tv,
  data = data,
  mean
)

# ============================================================
# 9. INITIAL BUSINESS INSIGHT: REVIEWER CREDIBILITY
# ============================================================

# Satisfaction by elite user status
aggregate(
  satisfied ~ user_is_elite,
  data = data,
  mean
)

# Average user review count by satisfaction
aggregate(
  user_review_count ~ satisfied,
  data = data,
  mean
)

# Average user fans by satisfaction
aggregate(
  user_fans ~ satisfied,
  data = data,
  mean
)


# ============================================================
# 10. INITIAL BUSINESS INSIGHT: WEEKEND EFFECT
# ============================================================

# Satisfaction by weekend vs weekday
aggregate(
  satisfied ~ weekend,
  data = data,
  mean
)

# ============================================================
# 11. EXTERNAL DATA INSIGHT: PUBLIC HOLIDAY
# ============================================================

aggregate(
  satisfied ~ is_holiday,
  data = data,
  mean
)

# ============================================================
# 12. EXTERNAL DATA INSIGHT: CONSUMER SENTIMENT
# ============================================================

cor(
  data$satisfied,
  data$consumer_sentiment,
  use = "complete.obs"
)
aggregate(
  consumer_sentiment ~ satisfied,
  data = data,
  mean
)

# ============================================================
# 13. DATA VISUALIZATION
# ============================================================

# ============================================================
# Customer Satisfaction Distribution
# ============================================================

satisfaction_count <- table(data$satisfied)

satisfaction_pct <- round(
  prop.table(satisfaction_count) * 100,
  1
)

bp <- barplot(
  satisfaction_pct,
  names.arg = c("Unsatisfied", "Satisfied"),
  main = "Customer Satisfaction Distribution",
  xlab = "Customer Satisfaction",
  ylab = "Percentage (%)",
  ylim = c(0, 90)
)

print(bp)

text(
  x = bp,
  y = satisfaction_pct + 3,
  labels = paste0(satisfaction_pct, "%"),
  cex = 1.3,
  font = 2
)

# ------------------------------------------------------------
# Price Level vs Satisfaction
# ------------------------------------------------------------

price_sat <- aggregate(
  satisfied ~ business_price,
  data = data,
  mean
)

price_sat$satisfied_pct <- price_sat$satisfied * 100

bp <- barplot(
  price_sat$satisfied_pct,
  names.arg = price_sat$business_price,
  main = "Price Level vs Satisfaction",
  xlab = "Price Level",
  ylab = "Satisfaction Rate (%)",
  ylim = c(0, 100),
  las = 1
)

text(
  x = bp,
  y = price_sat$satisfied_pct,
  labels = paste0(round(price_sat$satisfied_pct, 1), "%"),
  pos = 3
)


# ------------------------------------------------------------
# Restaurant Attributes vs Satisfaction
# Unified Grouped Bar Chart
# ------------------------------------------------------------

# Function to calculate satisfaction percentage for 0 and 1
get_sat_pct <- function(variable) {
  result <- tapply(data$satisfied, data[[variable]], mean, na.rm = TRUE) * 100
  
  # Make sure order is always 0 then 1
  result <- result[c("0", "1")]
  
  return(result)
}

# Calculate satisfaction rate for each restaurant attribute
parking_pct <- get_sat_pct("has_parking")
reservation_pct <- get_sat_pct("takes_reservations")
outdoor_pct <- get_sat_pct("outdoor_seating")
tv_pct <- get_sat_pct("has_tv")

# Combine into one matrix
attribute_matrix <- cbind(
  Parking = parking_pct,
  Reservation = reservation_pct,
  `Outdoor Seating` = outdoor_pct,
  TV = tv_pct
)

# Rename rows
rownames(attribute_matrix) <- c("Without", "With")

# Create grouped bar chart
bp <- barplot(
  attribute_matrix,
  beside = TRUE,
  main = "Restaurant Attributes and Customer Satisfaction",
  ylab = "Satisfaction Rate (%)",
  xlab = "Restaurant Attributes",
  ylim = c(0, 100),
  las = 1,
  legend.text = rownames(attribute_matrix),
  args.legend = list(
    x = "topright",
    title = "Attribute Status",
    bty = "n"
  )
)

# Add percentage labels above bars
text(
  x = bp,
  y = attribute_matrix,
  labels = paste0(round(attribute_matrix, 1), "%"),
  pos = 3,
  cex = 0.8
)



# ------------------------------------------------------------
# Temporal Factors vs Satisfaction
# Unified Grouped Bar Chart
# ------------------------------------------------------------

# Function to calculate satisfaction percentage for 0 and 1
get_sat_pct <- function(variable) {
  result <- tapply(data$satisfied, data[[variable]], mean, na.rm = TRUE) * 100
  
  # Make sure order is always 0 then 1
  result <- result[c("0", "1")]
  
  return(result)
}

# Calculate satisfaction rate for each temporal factor
weekend_pct <- get_sat_pct("weekend")
holiday_pct <- get_sat_pct("is_holiday")

# Combine into one matrix
temporal_matrix <- cbind(
  Weekend = weekend_pct,
  `Public Holiday` = holiday_pct
)

# Rename rows
rownames(temporal_matrix) <- c("No", "Yes")

# Create grouped bar chart
bp <- barplot(
  temporal_matrix,
  beside = TRUE,
  main = "Temporal Factors and Customer Satisfaction",
  ylab = "Satisfaction Rate (%)",
  xlab = "Temporal Factors",
  ylim = c(0, 100),
  las = 1,
  legend.text = rownames(temporal_matrix),
  args.legend = list(
    x = "topright",
    title = "Condition",
    bty = "n"
  )
)

# Add percentage labels above bars
text(
  x = bp,
  y = temporal_matrix,
  labels = paste0(round(temporal_matrix, 1), "%"),
  pos = 3,
  cex = 0.8
)

# ------------------------------------------------------------
# Consumer Sentiment by Satisfaction
# ------------------------------------------------------------

sentiment_sat <- aggregate(
  consumer_sentiment ~ satisfied,
  data = data,
  mean
)

bp <- barplot(
  sentiment_sat$consumer_sentiment,
  names.arg = c("Unsatisfied", "Satisfied"),
  main = "Average Consumer Sentiment by Satisfaction",
  ylab = "Consumer Sentiment Index",
  ylim = c(0, max(sentiment_sat$consumer_sentiment) * 1.2),
  las = 1
)

text(
  x = bp,
  y = sentiment_sat$consumer_sentiment,
  labels = round(sentiment_sat$consumer_sentiment, 1),
  pos = 3
)

# ------------------------------------------------------------
# Elite Reviewer vs Satisfaction
# ------------------------------------------------------------

elite_sat <- aggregate(
  satisfied ~ user_is_elite,
  data = data,
  mean
)

elite_sat$satisfied_pct <- elite_sat$satisfied * 100

bp <- barplot(
  elite_sat$satisfied_pct,
  names.arg = c("Non-Elite Reviewer", "Elite Reviewer"),
  main = "Elite Reviewer Status vs Satisfaction",
  ylab = "Satisfaction Rate (%)",
  ylim = c(0, 100),
  las = 1
)

text(
  x = bp,
  y = elite_sat$satisfied_pct,
  labels = paste0(round(elite_sat$satisfied_pct, 1), "%"),
  pos = 3
)
# ------------------------------------------------------------
# Satisfaction Status vs Average User Review Count
# ------------------------------------------------------------

review_count_sat <- aggregate(
  user_review_count ~ satisfied,
  data = data,
  mean
)

bp <- barplot(
  review_count_sat$user_review_count,
  names.arg = c("Dissatisfied", "Satisfied"),
  main = "Average User Review Count by Satisfaction Status",
  ylab = "Average User Review Count",
  ylim = c(0, max(review_count_sat$user_review_count) * 1.2),
  las = 1
)

text(
  x = bp,
  y = review_count_sat$user_review_count,
  labels = round(review_count_sat$user_review_count, 1),
  pos = 3
)

# ------------------------------------------------------------
# Satisfaction Status vs Average Number of Fans
# ------------------------------------------------------------

fans_sat <- aggregate(
  user_fans ~ satisfied,
  data = data,
  mean
)

bp <- barplot(
  fans_sat$user_fans,
  names.arg = c("Dissatisfied", "Satisfied"),
  main = "Average Number of Fans by Satisfaction Status",
  ylab = "Average Number of Fans",
  ylim = c(0, max(fans_sat$user_fans) * 1.2),
  las = 1
)

text(
  x = bp,
  y = fans_sat$user_fans,
  labels = round(fans_sat$user_fans, 1),
  pos = 3
)
# ============================================================
# 15. CORRELATION HEATMAP
# ============================================================

library(corrplot)

cor_data <- data[, c(
  "satisfied",
  "business_price",
  "business_review_count",
  "has_parking",
  "takes_reservations",
  "outdoor_seating",
  "has_tv",
  "user_review_count",
  "user_average_stars",
  "user_fans",
  "user_is_elite",
  "review_length",
  "useful",
  "funny",
  "cool",
  "weekend",
  "is_holiday",
  "consumer_sentiment"
)]

cor_matrix <- cor(
  cor_data,
  use = "pairwise.complete.obs"
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.5,
  tl.cex = 0.6,
  tl.col = "black",
  tl.srt = 45,
  title = "Correlation Matrix of Model Variables",
  mar = c(0,0,2,0)
)

# ============================================================
# APPENDIX VARIABLE DESCRIPTION TABLE - CSV / EXCEL EXPORT
# ============================================================

library(dplyr)
library(purrr)
library(writexl)

#make sure business_price is numeric
data$business_price <- as.numeric(as.character(data$business_price))

# Variables to include in appendix
appendix_vars <- c(
  "review_month",
  "review_id",
  "business_id",
  "user_id",
  "review_stars",
  "satisfied",
  "review_date",
  "review_length",
  "useful",
  "funny",
  "cool",
  "business_name",
  "city",
  "state",
  "latitude",
  "longitude",
  "business_review_count",
  "is_open",
  "categories",
  "business_price",
  "has_parking",
  "takes_reservations",
  "outdoor_seating",
  "has_tv",
  "user_review_count",
  "user_average_stars",
  "user_fans",
  "user_is_elite",
  "review_day_of_week",
  "weekend",
  "is_holiday",
  "consumer_sentiment"
)

# Variable descriptions
variable_description <- c(
  review_month = "Month of the review, used to merge monthly consumer sentiment data",
  review_id = "Unique identifier of each review",
  business_id = "Unique identifier of each restaurant/business",
  user_id = "Unique identifier of each Yelp user",
  review_stars = "Star rating given by the reviewer (1–5)",
  satisfied = "Customer satisfaction indicator (1 = review stars ≥ 4, 0 = otherwise)",
  review_date = "Date when the review was posted",
  review_length = "Length of the review text measured by number of characters",
  useful = "Number of useful votes received by the review",
  funny = "Number of funny votes received by the review",
  cool = "Number of cool votes received by the review",
  business_name = "Name of the restaurant/business",
  city = "City where the restaurant is located",
  state = "State where the restaurant is located",
  latitude = "Geographic latitude of the restaurant",
  longitude = "Geographic longitude of the restaurant",
  business_review_count = "Total number of reviews received by the restaurant",
  is_open = "Indicates whether the restaurant is currently operating",
  categories = "Restaurant category and cuisine classification",
  business_price = "Restaurant price level (1 = cheapest, 4 = most expensive)",
  has_parking = "Parking availability indicator (1 = yes, 0 = no)",
  takes_reservations = "Reservation availability indicator (1 = yes, 0 = no)",
  outdoor_seating = "Outdoor seating availability indicator (1 = yes, 0 = no)",
  has_tv = "Television availability indicator (1 = yes, 0 = no)",
  user_review_count = "Total number of reviews written by the user",
  user_average_stars = "Average rating given by the user across all reviews",
  user_fans = "Number of Yelp fans/followers of the user",
  user_is_elite = "Yelp Elite status indicator (1 = elite user, 0 = non-elite user)",
  review_day_of_week = "Day of the week when the review was posted",
  weekend = "Weekend indicator (1 = weekend, 0 = weekday)",
  is_holiday = "Public holiday indicator (1 = public holiday, 0 = non-holiday)",
  consumer_sentiment = "Monthly consumer sentiment score from the University of Michigan Consumer Sentiment Index"
)

# Keep only variables that exist in your dataset
appendix_vars <- appendix_vars[appendix_vars %in% names(data)]

# Function to safely calculate numeric statistics
safe_stat <- function(x, fun) {
  if (is.numeric(x) || is.integer(x)) {
    return(round(fun(x, na.rm = TRUE), 2))
  } else {
    return(NA)
  }
}

# Create appendix table
appendix_table <- map_dfr(appendix_vars, function(var) {
  
  x <- data[[var]]
  
  tibble(
    Variable = var,
    Data_Type = class(x)[1],
    Mean = safe_stat(x, mean),
    SD = safe_stat(x, sd),
    Min = safe_stat(x, min),
    Max = safe_stat(x, max),
    Description = variable_description[var]
  )
})

# Replace NA with blank for cleaner output
appendix_table_clean <- appendix_table %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))

# View table in R
View(appendix_table_clean)

# Export to CSV
write.csv(
  appendix_table_clean,
  "appendix_variable_table.csv",
  row.names = FALSE
)

# Export to Excel
write_xlsx(
  appendix_table_clean,
  "appendix_variable_table.xlsx"
)
