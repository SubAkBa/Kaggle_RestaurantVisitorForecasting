library(data.table)

restaurant <- fread("restaurant_data.csv", data.table = F, stringsAsFactors = T, header = T)
sample_submission <- fread("sample_submission.csv", header = T, data.table = F, stringsAsFactors = T)

restaurant$hpg_exist <- as.factor(restaurant$hpg_exist)

# divide train / test
set.seed(1337)
rest_idx <- sample(1 : nrow(restaurant), size = floor((nrow(restaurant) * 0.7)))
rest_train <- restaurant[rest_idx, ]
rest_test <- restaurant[-rest_idx, ]

# Linear Regression
linear_model <- lm(visitors ~ . - id, data = rest_train)
summary(linear_model)

pred_1 <- predict(linear_model, data = rest_test)

rmsle(pred_1, rest_test$visitors) # 0.8916791

# RMSLE(Root Mean Square Log Error) Function
rmsle <- function(pred, actual){
  return (sqrt(mean(
    (log(pred + 1) - log(actual + 1))^2
  )))
}
