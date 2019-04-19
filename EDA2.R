library(lubridate)
library(dplyr)

rm(list = ls()); gc(reset = T)

# Data load
hpg_store_info <- fread("hpg_store_info.csv", header = T, data.table = F, stringsAsFactors = T)
air_store_info <- fread("air_store_info.csv", header = T, data.table = F, stringsAsFactors = T)
hpg_reserve <- fread("hpg_reserve.csv", header = T, data.table = F, stringsAsFactors = T)
air_reserve <- fread("air_reserve.csv", header = T, data.table = F, stringsAsFactors = T)
date_info <- fread("date_info.csv", header = T, data.table = F, stringsAsFactors = T)
air_visit_data <- fread("air_visit_data.csv", header = T, data.table = F, stringsAsFactors = T)
store_id_relation <- fread("store_id_relation.csv", header = T, data.table = F, stringsAsFactors = T)

# Split Area Name
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

air_store_info <- cbind(air_store_info, SplitAreaName("air", air_store_info$air_area_name))
air_store_info$air_area_name <- NULL

# delete latitude, longitude
hpg_store_info %>% ggplot(aes(x = latitude, y = longitude, colour = first_area_name)) + geom_jitter()
hpg_store_info %>% ggplot(aes(x = latitude, y = longitude, colour = second_area_name)) + geom_jitter()

air_store_info %>% ggplot(aes(x = latitude, y = longitude, colour = first_area_name)) + geom_jitter()
air_store_info %>% ggplot(aes(x = latitude, y = longitude, colour = second_area_name)) + geom_jitter()

hpg_store_info[, c("longitude", "latitude")] <- NULL
air_store_info[, c("longitude", "latitude")] <- NULL

# convert column of reserve data
air_reserve$visit_datetime <- as_date(air_reserve$visit_datetime)
air_reserve$reserve_datetime <- as_date(air_reserve$reserve_datetime)
colnames(air_reserve)[2] <- "visit_date"
colnames(air_reserve)[3] <- "reserve_date"

hpg_reserve$visit_datetime <- as_date(hpg_reserve$visit_datetime)
hpg_reserve$reserve_datetime <- as_date(hpg_reserve$reserve_datetime)
colnames(hpg_reserve)[2] <- "visit_date"
colnames(hpg_reserve)[3] <- "reserve_date"

air_reserve <- air_reserve %>% group_by(air_store_id, visit_date) %>% summarise(air_total_reserve_visitors = sum(reserve_visitors),
                                                                                air_total_reserve_count = n())
hpg_reserve <- hpg_reserve %>% group_by(hpg_store_id, visit_date) %>% summarise(hpg_total_reserve_visitors = sum(reserve_visitors),
                                                                                hpg_total_reserve_count = n())

# merge store_info & reserve and insert NA
air_store_reserve <- merge(air_store_info, air_reserve, by = "air_store_id", all.x = T)
hpg_store_reserve <- merge(hpg_store_info, hpg_reserve, by = "hpg_store_id", all.x = T)

air_store_reserve$air_total_reserve_count <- ifelse(is.na(air_store_reserve$air_total_reserve_count), 0, 
                                                    air_store_reserve$air_total_reserve_count)
air_store_reserve$air_total_reserve_visitors <- ifelse(is.na(air_store_reserve$air_total_reserve_visitors), 0, 
                                                       air_store_reserve$air_total_reserve_visitors)
hpg_store_reserve$hpg_total_reserve_count <- ifelse(is.na(hpg_store_reserve$hpg_total_reserve_count), 0, 
                                                    hpg_store_reserve$hpg_total_reserve_count)
hpg_store_reserve$hpg_total_reserve_visitors <- ifelse(is.na(hpg_store_reserve$hpg_total_reserve_visitors), 0, 
                                                       hpg_store_reserve$hpg_total_reserve_visitors)
air_store_reserve <- air_store_reserve %>% filter(!is.na(visit_date))

# merge date_info & air_visit_data
colnames(date_info)[1] <- "visit_date"
air_visit_data <- merge(air_visit_data, date_info, by = "visit_date", all.x = T)

# merge air & hpg using store_id_relation
air_data <- merge(air_visit_data, air_store_reserve, by = c("air_store_id", "visit_date"), all = T)

air_data$air_total_reserve_count <- ifelse(is.na(air_data$air_total_reserve_count), 0, air_data$air_total_reserve_count)
air_data$air_total_reserve_visitors <- ifelse(is.na(air_data$air_total_reserve_visitors), 0, air_data$air_total_reserve_visitors)
air_data[, c(6 : 8)] <- NULL
air_data <- merge(air_data, air_store_info, by = "air_store_id", all.x = T)
air_data <- air_data %>% filter(!is.na(visit_date))

id_relation <- merge(air_data, store_id_relation, by = "air_store_id", all.x = T)
store_data <- merge(id_relation, hpg_store_reserve, by = c("hpg_store_id", "visit_date"), all.x = T)

# column working
store_data$hpg_store_id <- NULL
store_data$id <- paste(store_data$air_store_id, as.character(store_data$visit_date), sep = "_")
store_data[, c(1, 2)] <- NULL
store_data <- store_data[, c(14, 1 : 13)]

table(is.na(store_data))
table(is.na(store_data$hpg_genre_name)) # perform hpg genre / first_area / second_area NA Processing
