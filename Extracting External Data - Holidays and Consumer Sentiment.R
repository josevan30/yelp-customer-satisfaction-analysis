# ============================================================
# DATA SCIENCE AND MARKETING ANALYTICS
# Yelp Customer Satisfaction Analysis
# Author: Jose
# Purpose: Extracting External Data
# ============================================================


# ============================================================
# 1. Load required packages
# ============================================================
library(jsonlite)
library(fredr)


# ============================================================
# 2. Load Yelp dataset
# ============================================================

df <- read.csv("yelp_v3.csv")
df$review_date <- as.Date(df$review_date)


# ============================================================
# 3. Extract external holiday data from Nager.Date API
# ============================================================

years <- 2005:2022
holiday_list <- list()

for (y in years) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", y, "/US")
  temp <- jsonlite::fromJSON(url)
  temp$year <- y
  holiday_list[[as.character(y)]] <- temp
}

holidays <- do.call(rbind, holiday_list)
holidays$holiday_date <- as.Date(holidays$date)


# ============================================================
# 4. Merge holiday variable with Yelp data
# ============================================================

df$is_holiday <- ifelse(df$review_date %in% holidays$holiday_date, 1, 0)

table(df$is_holiday)


# ============================================================
# 5. Extract external consumer sentiment data from FRED API
# ============================================================
# FRED API KEY
# Insert your personal FRED API key
fredr_set_key("YOUR_PERSONAL_API_KEY")

consumer_sentiment <- fredr(
  series_id = "UMCSENT",
  observation_start = as.Date("2005-01-01"),
  observation_end = as.Date("2022-01-31")
)

head(consumer_sentiment)


# ============================================================
# 6. Merge consumer sentiment with Yelp data by month
# ============================================================

df$review_month <- format(df$review_date, "%Y-%m")
consumer_sentiment$review_month <- format(consumer_sentiment$date, "%Y-%m")

sentiment_clean <- consumer_sentiment[, c("review_month", "value")]
names(sentiment_clean)[2] <- "consumer_sentiment"

df <- merge(
  df,
  sentiment_clean,
  by = "review_month",
  all.x = TRUE
)

summary(df$consumer_sentiment)
sum(is.na(df$consumer_sentiment))


# ============================================================
# 7. Save final enriched dataset
# ============================================================

write.csv(df, "yelp_v5_holiday_sentiment.csv", row.names = FALSE)