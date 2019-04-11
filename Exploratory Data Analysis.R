library(data.table)
library(dplyr)
library(ggplot2)

air_reserve <- fread("air_reserve.csv", header = T, data.table = F)
air_store_info <- fread("air_store_info.csv", header = T, data.table = F)
air_visit_data <- fread("air_visit_data.csv", header = T, data.table = F)
date_info <- fread("date_info.csv", header = T, data.table = F)
hpg_reserve <- fread("hpg_reserve.csv", header = T, data.table = F)
hpg_store_info <- fread("hpg_store_info.csv", header = T, data.table = F)
store_id_relation <- fread("store_id_relation.csv", header = T, data.table = F)

# 1. Total Visitors per holiday_flag, a week
head(air_visit_data)
head(date_info)

ch_date_info <- date_info
colnames(ch_date_info)[1] <- "visit_date"

merge_date <- merge(air_visit_data, ch_date_info, by = "visit_date", all.x = T)
head(merge_date)

merge_date$day_of_week <- factor(merge_date$day_of_week, levels = c("Sunday", "Monday", "Tuesday", 
                                                                    "Wednesday", "Thursday", "Friday", "Saturday"))
merge_date$holiday_flg <- as.factor(merge_date$holiday_flg)

visit_per_day <- merge_date %>% group_by(day_of_week, holiday_flg) %>% summarise(total_visitors = sum(visitors),
                                                                mean_visitors = mean(visitors))

ggplot(data = visit_per_day, aes(x = day_of_week, y = total_visitors, fill = holiday_flg)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(. ~ holiday_flg, scales = "free") + coord_flip() # flag = 1 일때 합계 visitor는 차이가 있다.
ggplot(data = visit_per_day, aes(x = day_of_week, y = mean_visitors, fill = holiday_flg)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(. ~ holiday_flg, scales = "free") + coord_flip() # flag = 1 일때 평균 visitor는 차이가 없다.
                                                              # -> 요일에 따라 flag = 1 인 갯 수의 차이가 있을 것이다.

# the number of weekday per holiday_flg
head(merge_date)

weekday_per_holiflg <- merge_date %>% filter(holiday_flg == 1) %>% group_by(day_of_week) %>% summarise(n = n())
ggplot(weekday_per_holiflg, aes(x = day_of_week, y = n)) + geom_bar(stat = "identity") # flag = 1 일때 요일별로 갯수의 차이가 있다.
