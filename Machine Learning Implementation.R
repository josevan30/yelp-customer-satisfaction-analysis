# ============================================================
# DATA SCIENCE AND MARKETING ANALYTICS
# Yelp Customer Satisfaction Analysis
# Author: Jose
# Purpose: Machine Learning Implementation
# ============================================================

# ============================================================
# 1. LOAD DATA
# ============================================================

data <- read.csv("yelp_v5_holiday_sentiment.csv")


# ============================================================
# 12. CREATE CLEAN MODELING DATASET
# ============================================================

library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(class)
library(nnet)
library(gbm)
library(ipred)
library(pROC)

predictor_vars <- c(
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
)

model_data <- data[, c("satisfied", predictor_vars)]
model_data <- na.omit(model_data)
model_data$satisfied <- as.numeric(model_data$satisfied)

nrow(model_data)

# ============================================================
# 13. TRAIN-TEST SPLIT
# ============================================================

set.seed(123)

train_index <- sample(
  1:nrow(model_data),
  size = 0.7 * nrow(model_data)
)

train_data <- model_data[train_index, ]
test_data  <- model_data[-train_index, ]

train_class <- train_data
test_class  <- test_data

train_class$satisfied <- as.factor(train_class$satisfied)
test_class$satisfied  <- as.factor(test_class$satisfied)

test_actual <- test_data$satisfied

model_formula <- as.formula(
  paste("satisfied ~", paste(predictor_vars, collapse = " + "))
)

# ============================================================
# 14. HELPER FUNCTIONS
# ============================================================

get_accuracy <- function(actual, predicted) {
  mean(as.character(actual) == as.character(predicted))
}

get_gini <- function(actual, probability) {
  roc_obj <- roc(actual, probability, quiet = TRUE)
  auc_value <- as.numeric(auc(roc_obj))
  gini_value <- 2 * auc_value - 1
  return(gini_value)
}

get_tdl <- function(actual, probability) {
  actual <- as.numeric(as.character(actual))
  top_n <- ceiling(0.1 * length(actual))
  top_index <- order(probability, decreasing = TRUE)[1:top_n]
  
  top_response_rate <- mean(actual[top_index])
  overall_response_rate <- mean(actual)
  
  tdl <- top_response_rate / overall_response_rate
  return(tdl)
}

model_results <- data.frame()


# ============================================================
# 15. MODEL 1: LOGISTIC REGRESSION
# ============================================================

time_logit <- system.time({
  
  model_logit <- glm(
    model_formula,
    data = train_data,
    family = binomial
  )
  
  logit_prob <- predict(
    model_logit,
    newdata = test_data,
    type = "response"
  )
  
  logit_pred <- ifelse(logit_prob >= 0.5, 1, 0)
  
})

summary(model_logit)

logit_confusion <- table(
  Actual = test_actual,
  Predicted = logit_pred
)

logit_confusion

logit_accuracy <- get_accuracy(test_actual, logit_pred)
logit_gini <- get_gini(test_actual, logit_prob)
logit_tdl <- get_tdl(test_actual, logit_prob)


# ============================================================
# 16. MODEL 2: NAIVE BAYES
# ============================================================

time_nb <- system.time({
  
  model_nb <- naiveBayes(
    model_formula,
    data = train_class
  )
  
  nb_prob_raw <- predict(
    model_nb,
    newdata = test_class,
    type = "raw"
  )
  
  nb_prob <- nb_prob_raw[, "1"]
  
  nb_pred <- predict(
    model_nb,
    newdata = test_class
  )
  
})

nb_confusion <- table(
  Actual = test_class$satisfied,
  Predicted = nb_pred
)

nb_confusion

nb_accuracy <- get_accuracy(test_class$satisfied, nb_pred)
nb_gini <- get_gini(test_actual, nb_prob)
nb_tdl <- get_tdl(test_actual, nb_prob)


# ============================================================
# 17. MODEL 3: DECISION TREE
# ============================================================

time_tree <- system.time({
  
  model_tree <- rpart(
    model_formula,
    data = train_class,
    method = "class",
    control = rpart.control(
      cp = 0.001,
      minsplit = 50,
      maxdepth = 4
    )
  )
  
  tree_prob_raw <- predict(
    model_tree,
    newdata = test_class,
    type = "prob"
  )
  
  tree_prob <- tree_prob_raw[, "1"]
  
  tree_pred <- predict(
    model_tree,
    newdata = test_class,
    type = "class"
  )
  
})

rpart.plot(
  model_tree,
  main = "Decision Tree for Customer Satisfaction"
)

tree_confusion <- table(
  Actual = test_class$satisfied,
  Predicted = tree_pred
)

tree_confusion

tree_accuracy <- get_accuracy(test_class$satisfied, tree_pred)
tree_gini <- get_gini(test_actual, tree_prob)
tree_tdl <- get_tdl(test_actual, tree_prob)


# ============================================================
# 18. MODEL 4: RANDOM FOREST
# ============================================================

set.seed(123)

time_rf <- system.time({
  
  model_rf <- randomForest(
    model_formula,
    data = train_class,
    ntree = 500,
    importance = TRUE
  )
  
  rf_prob_raw <- predict(
    model_rf,
    newdata = test_class,
    type = "prob"
  )
  
  rf_prob <- rf_prob_raw[, "1"]
  
  rf_pred <- predict(
    model_rf,
    newdata = test_class
  )
  
})

rf_confusion <- table(
  Actual = test_class$satisfied,
  Predicted = rf_pred
)

rf_confusion

rf_accuracy <- get_accuracy(test_class$satisfied, rf_pred)
rf_gini <- get_gini(test_actual, rf_prob)
rf_tdl <- get_tdl(test_actual, rf_prob)

varImpPlot(
  model_rf,
  main = "Random Forest Variable Importance"
)


# ============================================================
# 19. SCALE DATA FOR KNN, SVM, AND NEURAL NETWORK
# ============================================================

knn_train <- train_data[, predictor_vars]
knn_test  <- test_data[, predictor_vars]

train_scaled <- scale(knn_train)

test_scaled <- scale(
  knn_test,
  center = attr(train_scaled, "scaled:center"),
  scale = attr(train_scaled, "scaled:scale")
)

train_labels <- as.factor(train_data$satisfied)
test_labels  <- as.factor(test_data$satisfied)


# ============================================================
# 20. MODEL 5: KNN
# ============================================================

time_knn <- system.time({
  
  knn_pred <- knn(
    train = train_scaled,
    test = test_scaled,
    cl = train_labels,
    k = 5,
    prob = TRUE
  )
  
  knn_vote_prob <- attr(knn_pred, "prob")
  
  knn_prob <- ifelse(
    as.character(knn_pred) == "1",
    knn_vote_prob,
    1 - knn_vote_prob
  )
  
})

knn_confusion <- table(
  Actual = test_labels,
  Predicted = knn_pred
)

knn_confusion

knn_accuracy <- get_accuracy(test_labels, knn_pred)
knn_gini <- get_gini(test_actual, knn_prob)
knn_tdl <- get_tdl(test_actual, knn_prob)

# Optional K comparison plot
k_values <- c(1, 3, 5, 7, 9, 11, 13, 15)
knn_acc <- c()

for (k in k_values) {
  pred_k <- knn(
    train = train_scaled,
    test = test_scaled,
    cl = train_labels,
    k = k
  )
  
  knn_acc <- c(knn_acc, get_accuracy(test_labels, pred_k))
}

plot(
  k_values,
  knn_acc,
  type = "b",
  xlab = "K Value",
  ylab = "Accuracy",
  main = "KNN Accuracy by K"
)


# ============================================================
# 21. MODEL 6: SUPPORT VECTOR MACHINE
# ============================================================

time_svm <- system.time({
  
  model_svm <- svm(
    x = train_scaled,
    y = train_labels,
    kernel = "radial",
    probability = TRUE
  )
  
  svm_pred <- predict(
    model_svm,
    newdata = test_scaled,
    probability = TRUE
  )
  
  svm_prob_raw <- attr(svm_pred, "probabilities")
  svm_prob <- svm_prob_raw[, "1"]
  
})

svm_confusion <- table(
  Actual = test_labels,
  Predicted = svm_pred
)

svm_confusion

svm_accuracy <- get_accuracy(test_labels, svm_pred)
svm_gini <- get_gini(test_actual, svm_prob)
svm_tdl <- get_tdl(test_actual, svm_prob)



# ============================================================
# 22. MODEL 7: NEURAL NETWORK
# ============================================================

set.seed(123)

time_nn <- system.time({
  
  model_nn <- nnet(
    x = train_scaled,
    y = class.ind(train_labels),
    size = 8,
    decay = 0.01,
    maxit = 1000,
    softmax = TRUE,
    trace = FALSE
  )
  
  nn_prob_raw <- predict(
    model_nn,
    newdata = test_scaled,
    type = "raw"
  )
  
  # Probability of class 1 = satisfied
  nn_prob <- nn_prob_raw[, "1"]
  
  nn_pred <- ifelse(nn_prob >= 0.5, 1, 0)
  
})

nn_confusion <- table(
  Actual = test_actual,
  Predicted = nn_pred
)

nn_confusion

nn_accuracy <- get_accuracy(test_actual, nn_pred)
nn_gini <- get_gini(test_actual, nn_prob)
nn_tdl <- get_tdl(test_actual, nn_prob)

nn_accuracy
nn_gini
nn_tdl

summary(nn_prob)
length(unique(round(nn_prob, 4)))


# ============================================================
# 23. MODEL 8: GRADIENT BOOSTING
# ============================================================

set.seed(123)

time_gbm <- system.time({
  
  model_gbm <- gbm(
    formula = model_formula,
    data = train_data,
    distribution = "bernoulli",
    n.trees = 500,
    interaction.depth = 3,
    shrinkage = 0.01,
    n.minobsinnode = 10,
    verbose = FALSE
  )
  
  gbm_prob <- predict(
    model_gbm,
    newdata = test_data,
    n.trees = 500,
    type = "response"
  )
  
  gbm_pred <- ifelse(gbm_prob >= 0.5, 1, 0)
  
})

summary(model_gbm)

gbm_confusion <- table(
  Actual = test_actual,
  Predicted = gbm_pred
)

gbm_confusion

gbm_accuracy <- get_accuracy(test_actual, gbm_pred)
gbm_gini <- get_gini(test_actual, gbm_prob)
gbm_tdl <- get_tdl(test_actual, gbm_prob)


# ============================================================
# 24. MODEL 9: BAGGING
# ============================================================

set.seed(123)

time_bag <- system.time({
  
  model_bag <- bagging(
    model_formula,
    data = train_class,
    nbagg = 50
  )
  
  bag_prob_raw <- predict(
    model_bag,
    newdata = test_class,
    type = "prob"
  )
  
  bag_prob <- bag_prob_raw[, "1"]
  
  bag_pred <- predict(
    model_bag,
    newdata = test_class,
    type = "class"
  )
  
})

bag_confusion <- table(
  Actual = test_class$satisfied,
  Predicted = bag_pred
)

bag_confusion

bag_accuracy <- get_accuracy(test_class$satisfied, bag_pred)
bag_gini <- get_gini(test_actual, bag_prob)
bag_tdl <- get_tdl(test_actual, bag_prob)


# ============================================================
# 25. FINAL MODEL COMPARISON TABLE
# ============================================================

model_results <- data.frame(
  Model = c(
    "Logistic Regression",
    "Naive Bayes",
    "Decision Tree",
    "Random Forest",
    "KNN",
    "SVM",
    "Neural Network",
    "Gradient Boosting",
    "Bagging"
  ),
  
  Hit_Rate_Percent = round(c(
    logit_accuracy,
    nb_accuracy,
    tree_accuracy,
    rf_accuracy,
    knn_accuracy,
    svm_accuracy,
    nn_accuracy,
    gbm_accuracy,
    bag_accuracy
  ) * 100, 2),
  
  GINI = round(c(
    logit_gini,
    nb_gini,
    tree_gini,
    rf_gini,
    knn_gini,
    svm_gini,
    nn_gini,
    gbm_gini,
    bag_gini
  ), 3),
  
  Top_Decile_Lift = round(c(
    logit_tdl,
    nb_tdl,
    tree_tdl,
    rf_tdl,
    knn_tdl,
    svm_tdl,
    nn_tdl,
    gbm_tdl,
    bag_tdl
  ), 3),
  
  Runtime_Seconds = round(c(
    time_logit["elapsed"],
    time_nb["elapsed"],
    time_tree["elapsed"],
    time_rf["elapsed"],
    time_knn["elapsed"],
    time_svm["elapsed"],
    time_nn["elapsed"],
    time_gbm["elapsed"],
    time_bag["elapsed"]
  ), 3)
)

model_results <- model_results[
  order(model_results$Hit_Rate_Percent, decreasing = TRUE),
]

model_results
# ============================================================
# PERFORMANCE OF MACHINE LEARNING ALGORITHMS
# ============================================================

par(mar = c(8, 4, 4, 2))

bar_data <- t(
  as.matrix(
    model_results[, c("Top_Decile_Lift", "GINI")]
  )
)

bp <- barplot(
  bar_data,
  beside = TRUE,
  names.arg = model_results$Model,
  las = 2,
  legend.text = c("TDL", "GINI"),
  args.legend = list(
    x = "topright",
    inset = 0.02
  ),
  main = "Performance of Machine Learning Algorithms",
  ylab = "Metric Value",
  ylim = c(0, max(bar_data) * 1.20)
)

# Add values above bars
text(
  x = bp,
  y = c(bar_data) + 0.03,
  labels = round(c(bar_data), 2),
  cex = 0.8
)

par(mar = c(5, 4, 4, 2))

# ============================================================
# 26. MODEL ACCURACY COMPARISON PLOT
# ============================================================

bp <- barplot(
  model_results$Hit_Rate_Percent,
  names.arg = model_results$Model,
  las = 2,
  ylim = c(0, max(model_results$Hit_Rate_Percent) * 1.15),
  main = "Machine Learning Model Accuracy Comparison",
  ylab = "Hit Rate / Accuracy (%)"
)

text(
  x = bp,
  y = model_results$Hit_Rate_Percent,
  labels = paste0(model_results$Hit_Rate_Percent, "%"),
  pos = 3,
  cex = 0.8
)


# ============================================================
# 27. GINI COMPARISON PLOT
# ============================================================

bp <- barplot(
  model_results$GINI,
  names.arg = model_results$Model,
  las = 2,
  ylim = c(0, max(model_results$GINI) * 1.15),
  main = "GINI Comparison Across Machine Learning Models",
  ylab = "GINI Coefficient"
)

text(
  x = bp,
  y = model_results$GINI,
  labels = model_results$GINI,
  pos = 3,
  cex = 0.8
)


# ============================================================
# 28. TOP DECILE LIFT COMPARISON PLOT
# ============================================================

bp <- barplot(
  model_results$Top_Decile_Lift,
  names.arg = model_results$Model,
  las = 2,
  ylim = c(0, max(model_results$Top_Decile_Lift) * 1.15),
  main = "Top Decile Lift Comparison",
  ylab = "Top Decile Lift"
)

text(
  x = bp,
  y = model_results$Top_Decile_Lift,
  labels = model_results$Top_Decile_Lift,
  pos = 3,
  cex = 0.8
)


# ============================================================
# 29. LIFT CURVE FOR BEST MODEL
# ============================================================

prob_list <- list(
  "Logistic Regression" = logit_prob,
  "Naive Bayes" = nb_prob,
  "Decision Tree" = tree_prob,
  "Random Forest" = rf_prob,
  "KNN" = knn_prob,
  "SVM" = svm_prob,
  "Neural Network" = nn_prob,
  "Gradient Boosting" = gbm_prob,
  "Bagging" = bag_prob
)

best_model_name <- model_results$Model[1]
best_prob <- prob_list[[best_model_name]]

ordered_index <- order(best_prob, decreasing = TRUE)
actual_ordered <- test_actual[ordered_index]

population_share <- seq_along(actual_ordered) / length(actual_ordered)

cumulative_lift <- (
  cumsum(actual_ordered) / seq_along(actual_ordered)
) / mean(test_actual)

plot(
  population_share,
  cumulative_lift,
  type = "l",
  lwd = 2,
  xlab = "Share of Test Sample Ranked by Predicted Probability",
  ylab = "Cumulative Lift",
  main = paste("Lift Curve -", best_model_name)
)

abline(
  h = 1,
  lty = 2
)
