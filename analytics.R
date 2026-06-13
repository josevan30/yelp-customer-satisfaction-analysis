# ============================================================
# DATA SCIENCE AND MARKETING ANALYTICS
# Yelp Customer Satisfaction Analysis
# Author: Jose
# Purpose: Load prepared Yelp dataset and run initial analysis
# ============================================================


# ============================================================
# 1. LOAD DATA
# ============================================================

if (!file.exists("data/yelp_v2.csv")) {
  stop("Dataset not found. Please place yelp_v2.csv inside the data folder.")
}

data <- read.csv("data/yelp_v2.csv")


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
# 10. CREATE CLEAN DATASET FOR MODELING
# ============================================================

model_data <- na.omit(data[, c(
  "satisfied",
  "business_price",
  "business_stars",
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
  "weekend"
)])

nrow(model_data)


# ============================================================
# 11. LOGISTIC REGRESSION MODEL
# ============================================================

model_logit <- glm(
  satisfied ~ business_price +
    business_stars +
    business_review_count +
    has_parking +
    takes_reservations +
    outdoor_seating +
    has_tv +
    user_review_count +
    user_average_stars +
    user_fans +
    user_is_elite +
    review_length +
    useful +
    funny +
    cool +
    weekend,
  data = model_data,
  family = binomial
)

summary(model_logit)


# ============================================================
# 12. MODEL PREDICTIONS
# ============================================================

model_data$predicted_probability <- predict(
  model_logit,
  newdata = model_data,
  type = "response"
)

model_data$predicted_class <- ifelse(
  model_data$predicted_probability >= 0.5,
  1,
  0
)


# ============================================================
# 13. MODEL PERFORMANCE
# ============================================================

table(
  Actual = model_data$satisfied,
  Predicted = model_data$predicted_class
)

mean(model_data$satisfied == model_data$predicted_class)

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
# Business Stars vs Satisfaction
# ------------------------------------------------------------

star_sat <- aggregate(
  satisfied ~ business_stars,
  data = data,
  mean
)

star_sat$satisfied_pct <- star_sat$satisfied * 100

plot(
  star_sat$business_stars,
  star_sat$satisfied_pct,
  type = "b",
  xlab = "Business Stars",
  ylab = "Satisfaction Rate (%)",
  main = "Business Stars vs Satisfaction",
  ylim = c(0, 100),
  las = 1
)

text(
  x = star_sat$business_stars,
  y = star_sat$satisfied_pct,
  labels = paste0(round(star_sat$satisfied_pct, 1), "%"),
  pos = 3
)

# ============================================================
# 15. CORRELATION HEATMAP
# ============================================================

# Install package once if needed:
# install.packages("corrplot")

install.packages(
  "corrplot",
  repos = "https://cloud.r-project.org"
)
library(corrplot)

cor_data <- model_data[, c(
  "satisfied",
  "business_price",
  "business_stars",
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
  "weekend"
)]

cor_matrix <- cor(cor_data)

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
# 15B. LOGISTIC REGRESSION IMPORTANCE - IMPROVED
# ============================================================

coef_data <- summary(model_logit)$coefficients

coef_df <- data.frame(
  Variable = rownames(coef_data),
  Estimate = coef_data[,1],
  P_Value = coef_data[,4]
)

coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]

coef_df <- coef_df[order(coef_df$Estimate), ]

# Increase left margin so variable names are not cropped
par(mar = c(5, 12, 4, 2))

barplot(
  coef_df$Estimate,
  horiz = TRUE,
  names.arg = coef_df$Variable,
  las = 1,
  main = "Logistic Regression Coefficient Estimates",
  xlab = "Coefficient Estimate",
  cex.names = 0.8
)

# Reset margin
par(mar = c(5, 4, 4, 2))

# ============================================================
# 15C. PREDICTED PROBABILITY DISTRIBUTION
# ============================================================

hist(
  model_data$predicted_probability,
  breaks = 20,
  main = "Predicted Probability of Satisfaction",
  xlab = "Predicted Probability",
  ylab = "Number of Reviews"
)

# ============================================================
# 16. TRAIN-TEST SPLIT
# ============================================================

set.seed(123)

train_index <- sample(
  1:nrow(model_data),
  size = 0.7 * nrow(model_data)
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]


# ============================================================
# 17. MODEL 1: LOGISTIC REGRESSION ON TRAINING DATA
# ============================================================

model_logit_train <- glm(
  satisfied ~ business_price +
    business_stars +
    business_review_count +
    has_parking +
    takes_reservations +
    outdoor_seating +
    has_tv +
    user_review_count +
    user_average_stars +
    user_fans +
    user_is_elite +
    review_length +
    useful +
    funny +
    cool +
    weekend,
  data = train_data,
  family = binomial
)

test_data$logit_prob <- predict(
  model_logit_train,
  newdata = test_data,
  type = "response"
)

test_data$logit_pred <- ifelse(test_data$logit_prob >= 0.5, 1, 0)

logit_accuracy <- mean(test_data$satisfied == test_data$logit_pred)

logit_accuracy


# ============================================================
# 18. MODEL 2: DECISION TREE
# ============================================================

# Install package once if needed:
# install.packages("rpart")
# install.packages("rpart.plot")

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

model_tree <- rpart(
  as.factor(satisfied) ~ business_price +
    business_stars +
    business_review_count +
    has_parking +
    takes_reservations +
    outdoor_seating +
    has_tv +
    user_review_count +
    user_average_stars +
    user_fans +
    user_is_elite +
    review_length +
    useful +
    funny +
    cool +
    weekend,
  data = train_data,
  method = "class"
)

tree_pred <- predict(
  model_tree,
  newdata = test_data,
  type = "class"
)

tree_accuracy <- mean(test_data$satisfied == as.numeric(as.character(tree_pred)))

tree_accuracy

rpart.plot(
  model_tree,
  main = "Decision Tree for Customer Satisfaction"
)

# Decision Tree Accuracy 
test_data$tree_pred <- predict(
  model_tree,
  newdata = test_data,
  type = "class"
)

mean(
  test_data$satisfied ==
    as.numeric(as.character(test_data$tree_pred))
)
# ============================================================
# 19. MODEL 3: RANDOM FOREST
# ============================================================

# Install package once if needed:
# install.packages("randomForest")

install.packages("randomForest")
library(randomForest)

model_rf <- randomForest(
  as.factor(satisfied) ~ business_price +
    business_stars +
    business_review_count +
    has_parking +
    takes_reservations +
    outdoor_seating +
    has_tv +
    user_review_count +
    user_average_stars +
    user_fans +
    user_is_elite +
    review_length +
    useful +
    funny +
    cool +
    weekend,
  data = train_data,
  ntree = 100,
  importance = TRUE
)

rf_pred <- predict(
  model_rf,
  newdata = test_data
)

rf_accuracy <- mean(test_data$satisfied == as.numeric(as.character(rf_pred)))

rf_accuracy

importance_df <- data.frame(
  Variable = rownames(importance(model_rf)),
  Importance = importance(model_rf)[, "MeanDecreaseAccuracy"]
)

importance_df <- importance_df[
  order(importance_df$Importance, decreasing = TRUE),
]

# Increase left margin
par(mar = c(5, 15, 4, 2))

bp <- barplot(
  rev(importance_df$Importance),
  names.arg = rev(importance_df$Variable),
  horiz = TRUE,
  las = 1,
  col = "steelblue",
  main = "Random Forest Variable Importance",
  xlab = "Mean Decrease Accuracy",
  cex.names = 0.9,
  xlim = c(0, max(importance_df$Importance) * 1.25)
)

text(
  x = rev(importance_df$Importance),
  y = bp,
  labels = round(rev(importance_df$Importance), 1),
  pos = 4
)

# ============================================================
# 20. MODEL COMPARISON
# ============================================================

model_comparison <- data.frame(
  Model = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  ),
  Accuracy = c(
    logit_accuracy,
    tree_accuracy,
    rf_accuracy
  )
)

bp <- barplot(
  model_comparison$Accuracy * 100,
  names.arg = model_comparison$Model,
  main = "Comparison of Machine Learning Models",
  ylab = "Prediction Accuracy (%)",
  ylim = c(0, 100),
  las = 1
)

text(
  x = bp,
  y = model_comparison$Accuracy * 100 + 2,
  labels = paste0(
    round(model_comparison$Accuracy * 100, 1),
    "%"
  ),
  cex = 1.1,
  font = 2
)

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
# 22. TEST CONFUSION MATRICES
# ============================================================

table(
  Actual = test_data$satisfied,
  Logistic_Regression = test_data$logit_pred
)

table(
  Actual = test_data$satisfied,
  Decision_Tree = test_data$tree_pred
)

table(
  Actual = test_data$satisfied,
  Random_Forest = rf_pred
)

# ============================================================
# 23. SAVE FINAL MODEL COMPARISON TABLE
# ============================================================

write.csv(
  model_comparison,
  "model_comparison_results.csv",
  row.names = FALSE
)

write.csv(
  summary_table,
  "summary_statistics.csv",
  row.names = FALSE
)