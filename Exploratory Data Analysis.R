install.packages("forecast")

library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)

rm(list = ls()); gc(reset = T)

air_reserve <- fread("air_reserve.csv", header = T, data.table = F, stringsAsFactors = T)
air_store_info <- fread("air_store_info.csv", header = T, data.table = F, stringsAsFactors = T)
air_visit_data <- fread("air_visit_data.csv", header = T, data.table = F, stringsAsFactors = T)
date_info <- fread("date_info.csv", header = T, data.table = F, stringsAsFactors = T)
hpg_reserve <- fread("hpg_reserve.csv", header = T, data.table = F, stringsAsFactors = T)
hpg_store_info <- fread("hpg_store_info.csv", header = T, data.table = F, stringsAsFactors = T)
store_id_relation <- fread("store_id_relation.csv", header = T, data.table = F, stringsAsFactors = T)

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


# 2. holiday_flag, air_area_name, air_store_id
head(air_store_info)
str(air_store_info)

head(hpg_store_info)
str(hpg_store_info)

head(date_info)
str(date_info)

head(air_visit_data)
str(air_visit_data)

holi_date <- date_info %>% filter(holiday_flg == 1)
holi_date$day_of_week <- NULL
colnames(holi_date)[1] <- "visit_date"

air_date_area <- merge(air_visit_data, air_store_info, by = "air_store_id", all.x = T)
str(air_date_area)

date_area_flg <- merge(air_date_area, holi_date, by = "visit_date", all.x = T)

str(date_area_flg)
table(date_area_flg$air_area_name)

date_area_flg %>% group_by(air_genre_name) %>% summarise(genre_sum = n()) %>% arrange(desc(genre_sum)) %>% 
  ggplot(aes(x = reorder(air_genre_name, genre_sum), y = genre_sum)) + geom_bar(stat = "identity") + coord_flip()
                                                     # 1. Izakaya, 2. Cafe / Sweets, 3. Dining bar

# 3. time-series for Total visitors
head(air_visit_data)
str(air_visit_data)
air_visit_data$air_store_id <- NULL
air_visit_data$visit_date <- NULL

ts_visit_data <- ts(air_visit_data, frequency = 12, start = c(2016, 1), end = c(2017, 4))

plot(ts_visit_data)
auto.arima(ts_visit_data)
forecast(ts_visit_data)

# external Data
air_station_distances <- fread("air_station_distances.csv", header = T, data.table = F, stringsAsFactors = T)
air_store_near_active_station <- fread("air_store_info_with_nearest_active_station.csv", header = T, 
                                       data.table= F, stringsAsFactors = T)
feature_manifest <- fread("feature_manifest.csv", header = T, data.table = F, stringsAsFactors = T)
hpg_station_distances <- fread("hpg_station_distances.csv", header = T, data.table= F, stringsAsFactors = T)
hpg_store_near_active_station <- fread("hpg_store_info_with_nearest_active_station.csv", header = T,
                                       data.table = F, stringsAsFactors = T)
near_active_station <- fread("nearby_active_stations.csv", header = T, data.table = F, stringsAsFactors = T)
weather_stations <- fread("weather_stations.csv", header = T, data.table = F, stringsAsFactors = T)
head(air_station_distances)
head(air_store_near_active_station)
head(feature_manifest)
head(hpg_station_distances)
head(hpg_store_near_active_station)
head(near_active_station)
head(weather_stations)

