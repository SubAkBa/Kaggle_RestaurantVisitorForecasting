install.packages("forecast")

library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(stringr)

rm(list = ls()); gc(reset = T)

air_reserve <- fread("air_reserve.csv", header = T, data.table = F, stringsAsFactors = T)
air_store_info <- fread("air_store_info.csv", header = T, data.table = F, stringsAsFactors = T)
air_visit_data <- fread("air_visit_data.csv", header = T, data.table = F, stringsAsFactors = T)
date_info <- fread("date_info.csv", header = T, data.table = F, stringsAsFactors = T)
hpg_reserve <- fread("hpg_reserve.csv", header = T, data.table = F, stringsAsFactors = T)
hpg_store_info <- fread("hpg_store_info.csv", header = T, data.table = F, stringsAsFactors = T)
store_id_relation <- fread("store_id_relation.csv", header = T, data.table = F, stringsAsFactors = T)
sample_submission <- fread("sample_submission.csv", header = T, data.table = F, stringsAsFactors = T)

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

# 3. make column that means whether hpg system is existed / delete latitude, longitude & split area_name
head(hpg_store_info)
str(hpg_store_info)

head(store_id_relation)
str(store_id_relation)

head(air_store_info)
str(air_store_info)

# 3-1. Make split area name function
SplitAreaName <- function(data_name, area_name){
  split_area_data <- sapply(as.character(area_name), str_split, " ")
  first_area_name <- c(); second_area_name <- c()
  
  for(i in 1 : NROW(area_name)){
    first_area_name <- c(first_area_name, split_area_data[[i]][1])
    second_area_name <- c(second_area_name, split_area_data[[i]][2])
  }
  
  result <- data.frame(one = first_area_name, two = second_area_name)
  colnames(result)[c(1, 2)] <- c(paste0(data_name, "_first_area_name"), paste0(data_name, "_second_area_name"))
  
  return (result)
}
hpg_store_info <- cbind(hpg_store_info, SplitAreaName("hpg", hpg_store_info$hpg_area_name))
hpg_store_info$hpg_area_name <- NULL
str(hpg_store_info)

air_store_info <- cbind(air_store_info, SplitAreaName("air", air_store_info$air_area_name))
air_store_info$air_area_name <- NULL
str(air_store_info)

# 3-2. delete latitude, longitude
hpg_store_info %>% ggplot(aes(x = latitude, y = longitude, colour = first_area_name)) + geom_jitter()
hpg_store_info %>% ggplot(aes(x = latitude, y = longitude, colour = second_area_name)) + geom_jitter()

air_store_info %>% ggplot(aes(x = latitude, y = longitude, colour = first_area_name)) + geom_jitter()
air_store_info %>% ggplot(aes(x = latitude, y = longitude, colour = second_area_name)) + geom_jitter()

hpg_store_info[, c("longitude", "latitude")] <- NULL
air_store_info[, c("longitude", "latitude")] <- NULL

merge_store <- merge(store_id_relation, air_store_info, by = "air_store_id", all.x = T)
merge_store <- merge(merge_store, hpg_store_info, by = "hpg_store_id", all.x = T)

all_store_data <- merge(air_store_info, merge_store, by = c("air_store_id", "air_genre_name",
                                                            "air_first_area_name", "air_second_area_name"), all.x = T)

# 3-3. whether hpg system is existed
all_store_data$hpg_exist <- ifelse(is.na(all_store_data$hpg_store_id), 0, 1)
all_store_data[, c(5 : 8)] <- NULL
str(all_store_data)
head(all_store_data)

# 4. add columns that the number of hpg_reserve, total number of hpg_reserve_visitors per date
head(hpg_reserve)

# 4-1. how many reserve count per datetime
hpg_reserve$visit_datetime <- as_date(hpg_reserve$visit_datetime)
hpg_reserve$reserve_datetime <- as_date(hpg_reserve$reserve_datetime)
table(hpg_reserve$reserve_datetime == hpg_reserve$visit_datetime) # 예약날짜와 방문날짜가 다른 row가 있다.

# 4-2. merge data & diff calculate visit_datetime, reserve_datetime
store_data <- merge(all_store_data, hpg_reserve, by = "hpg_store_id", all.x = T)
store_data$hpg_store_id <- NULL

store_data$hpg_diffday_visit_reserve <- as.numeric(store_data$visit_datetime - store_data$reserve_datetime)
str(store_data)
t <- store_data %>% group_by(air_store_id, visit_datetime) %>% summarise(n = n()) # 가게 별 예약날짜는 다르고 방문날짜는 같은경우가 있는지
t %>% filter(n >= 2) # 있다.

# 4-3. the total number of reserve visitors, min / max diff day per visit_datetime
t <- store_data %>% group_by(air_store_id, visit_datetime) %>% summarise(hpg_mindiff = min(hpg_diffday_visit_reserve),
                                                                         hpg_maxdiff = max(hpg_diffday_visit_reserve),
                                                                         hpg_total_reserve_visitores = sum(reserve_visitors))
merge_store1 <- merge(store_data, )


# hpg의 area와 air의 area가 다르다.
tail(store_data[which(as.character(store_data$air_first_area_name) != as.character(store_data$hpg_first_area_name)), ], 20)



# external data
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

