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
# 21. SUMMARY STATISTICS TABLE
# ============================================================

summary_table <- data.frame(
  Variable = names(model_data),
  Mean = sapply(model_data, mean),
  SD = sapply(model_data, sd),
  Min = sapply(model_data, min),
  Max = sapply(model_data, max)
)

summary_table

# ============================================================
# 3. CHECK DEPENDENT VARIABLE: CUSTOMER SATISFACTION
# satisfied = 1 if review_stars >= 4
# satisfied = 0 if review_stars < 4
# ============================================================

# Count satisfied vs not satisfied
table(data$satisfied)

# Percentage satisfied vs not satisfied
prop.table(table(data$satisfied))


# ============================================================
# 4. CLEAN / FIX VARIABLE TYPES
# ============================================================

# Convert business_price from character to numeric
# Warning about NA is okay because missing/None values become NA
data$business_price <- as.numeric(data$business_price)

# Check if conversion worked
str(data$business_price)


# ============================================================
# 5. CHECK MISSING VALUES
# ============================================================

# Count missing values in business_price
sum(is.na(data$business_price))

# Distribution of price levels, including missing values
table(data$business_price, useNA = "always")


# ============================================================
# 6. INITIAL BUSINESS INSIGHT: PRICE VS SATISFACTION
# ============================================================

# Average satisfaction rate by price level
aggregate(
  satisfied ~ business_price,
  data = data,
  mean
)

# ============================================================
# 7. INITIAL BUSINESS INSIGHT: RESTAURANT ATTRIBUTES
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
# 8. INITIAL BUSINESS INSIGHT: REVIEWER CREDIBILITY
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
# 9. INITIAL BUSINESS INSIGHT: WEEKEND EFFECT
# ============================================================

# Satisfaction by weekend vs weekday
aggregate(
  satisfied ~ weekend,
  data = data,
  mean
)

# ============================================================
# 10. EXTERNAL DATA INSIGHT: PUBLIC HOLIDAY
# ============================================================

aggregate(
  satisfied ~ is_holiday,
  data = data,
  mean
)

# ============================================================
# 11. EXTERNAL DATA INSIGHT: CONSUMER SENTIMENT
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
# 14. DATA VISUALIZATION
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
# Parking Availability vs Satisfaction
# ------------------------------------------------------------

parking_sat <- aggregate(
  satisfied ~ has_parking,
  data = data,
  mean
)

parking_sat$satisfied_pct <- parking_sat$satisfied * 100

bp <- barplot(
  parking_sat$satisfied_pct,
  names.arg = c("No Parking", "Parking"),
  main = "Parking Availability vs Satisfaction",
  ylab = "Satisfaction Rate (%)",
  ylim = c(0, 100),
  las = 1
)

text(
  x = bp,
  y = parking_sat$satisfied_pct,
  labels = paste0(round(parking_sat$satisfied_pct, 1), "%"),
  pos = 3
)


# ------------------------------------------------------------
# Reservations vs Satisfaction
# ------------------------------------------------------------

reservation_sat <- aggregate(
  satisfied ~ takes_reservations,
  data = data,
  mean
)

reservation_sat$satisfied_pct <- reservation_sat$satisfied * 100

bp <- barplot(
  reservation_sat$satisfied_pct,
  names.arg = c("No Reservation", "Reservation"),
  main = "Reservations vs Satisfaction",
  ylab = "Satisfaction Rate (%)",
  ylim = c(0, 100),
  las = 1
)

text(
  x = bp,
  y = reservation_sat$satisfied_pct,
  labels = paste0(round(reservation_sat$satisfied_pct, 1), "%"),
  pos = 3
)

# ------------------------------------------------------------
# Weekend vs Satisfaction
# ------------------------------------------------------------

weekend_sat <- aggregate(
  satisfied ~ weekend,
  data = data,
  mean
)

weekend_sat$satisfied_pct <- weekend_sat$satisfied * 100

bp <- barplot(
  weekend_sat$satisfied_pct,
  names.arg = c("Weekday", "Weekend"),
  main = "Weekend vs Satisfaction",
  ylab = "Satisfaction Rate (%)",
  ylim = c(0, 100),
  las = 1
)

text(
  x = bp,
  y = weekend_sat$satisfied_pct,
  labels = paste0(round(weekend_sat$satisfied_pct, 1), "%"),
  pos = 3
)

# ------------------------------------------------------------
# Public Holiday vs Satisfaction
# ------------------------------------------------------------

holiday_sat <- aggregate(
  satisfied ~ is_holiday,
  data = data,
  mean
)

holiday_sat$satisfied_pct <- holiday_sat$satisfied * 100

bp <- barplot(
  holiday_sat$satisfied_pct,
  names.arg = c("Non-Holiday", "Holiday"),
  main = "Public Holiday vs Satisfaction",
  ylab = "Satisfaction Rate (%)",
  ylim = c(0, 100),
  las = 1
)

text(
  x = bp,
  y = holiday_sat$satisfied_pct,
  labels = paste0(round(holiday_sat$satisfied_pct, 1), "%"),
  pos = 3
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
