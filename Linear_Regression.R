# 4. Linear Regression except reserve data
head(date_info)
head(store_id_relation)

# 4-1. merge air data
head(air_store_info)
str(air_store_info)


head(air_visit_data)
str(air_visit_data)

air_data <- merge(air_visit_data, air_store_info, by = "air_store_id", all.x = T)

# 4-2. merge hpg & air using store_id_relation (hold)
head(hpg_store_info)
str(hpg_store_info)

head(store_id_relation)
str(store_id_relation)

relation_data <- merge(hpg_store_info, store_id_relation, by = "hpg_store_id", all.x = T) # Occured NA

# 4-3. using only air data
head(date_info)
colnames(date_info)[1] <- "visit_date"

only_air_data <- merge(air_data, date_info, by = "visit_date", all.x = T)
only_air_data$holiday_flg <- as.factor(only_air_data$holiday_flg)

length(unique(air_visit_data$air_store_id))

str(only_air_data)

# divide train / test and Regression
set.seed(1337)
air_idx <- sample(1 : nrow(only_air_data), size = floor((nrow(only_air_data) * 0.7)))
air_train <- only_air_data[air_idx, ]
air_test <- only_air_data[-air_idx, ]

linear_air_model <- lm(visitors ~ air_genre_name + day_of_week + holiday_flg, data = air_train)
summary(linear_air_model)

pred_air <- predict(linear_air_model, newdata = air_test)

rmsle(pred_air, air_test$visitors) # 0.8177436


# RMSLE(Root Mean Square Log Error) Function
rmsle <- function(pred, actual){
  return (sqrt(mean(
    (log(pred + 1) - log(actual + 1))^2
  )))
}
