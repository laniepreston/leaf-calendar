library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(car)
library(purrr)
library(stats)
library(ggplot2)

feb_arr <- fread("data/working_copies/split_datasets/Feb_PostArr.csv")
feb_dep <- fread("data/working_copies/split_datasets/Feb_PostDep.csv")
feb_prearr<- fread("data/working_copies/split_datasets/Feb_PreArr.csv")
feb_predep <- fread("data/working_copies/split_datasets/Feb_PreDep.csv")
mar_arr<- fread("data/working_copies/split_datasets/Mar_Arrivals.csv")
mar_dep <- fread("data/working_copies/split_datasets/Mar_Departures.csv")
apr_arr <- fread("data/working_copies/split_datasets/Apr_Arr.csv")
apr_dep <- fread("data/working_copies/split_datasets/Apr_Dep.csv")
con1_arr <- fread("data/working_copies/split_datasets/Con1_Arr.csv")
con1_dep <- fread("data/working_copies/split_datasets/Con1_Dep.csv")
con2_arr <- fread("data/working_copies/split_datasets/Con2_Arr.csv")
con2_dep <- fread("data/working_copies/split_datasets/Con2_Dep.csv")

#fix column names 
feb_arr <- feb_arr%>% 
  rename(date_sound = post_arr.date_sound,
         rel_diff_5 = post_arr.rel_diff_5,
         rel_diff_6 = post_arr.rel_diff_6,
         rel_diff_7 = post_arr.rel_diff_7,
         rel_diff_8 = post_arr.rel_diff_8,
         wind_speed = post_arr.Wind_Speed_2,
         bearing_FL = post_arr.bearing_FL,
         shield_angle = post_arr.shield_angle,
         ground_distance_FL = post_arr.ground_distance_FL,
         ground_Speed_ms = post_arr.Ground_Speed_ms)

feb_prearr <- feb_prearr %>% 
  rename(date_sound = pre_arr.date_sound,
         rel_diff_5 = pre_arr.rel_diff_5,
         rel_diff_6 = pre_arr.rel_diff_6,
         rel_diff_7 = pre_arr.rel_diff_7,
         rel_diff_8 = pre_arr.rel_diff_8,
         wind_speed = pre_arr.Wind_Speed_2,
         bearing_FL = pre_arr.bearing_FL,
         shield_angle = pre_arr.shield_angle,
         ground_distance_FL = pre_arr.ground_distance_FL,
         ground_Speed_ms = pre_arr.Ground_Speed_ms)
         

feb_dep <- feb_dep %>% 
  rename(date_sound = post_dep.date_sound,
         rel_diff_5 = post_dep.rel_diff_5,
         rel_diff_6 = post_dep.rel_diff_6,
         rel_diff_7 = post_dep.rel_diff_7,
         rel_diff_8 = post_dep.rel_diff_8,
         wind_speed = post_dep.Wind_Speed_2,
         bearing_FL = post_dep.bearing_FL,
         shield_angle = post_dep.shield_angle,
         ground_distance_FL = post_dep.ground_distance_FL,
         ground_Speed_ms = post_dep.Ground_Speed_ms)

feb_predep <- feb_predep %>% 
  rename(date_sound = pre_dep.date_sound,
         rel_diff_5 = pre_dep.rel_diff_5,
         rel_diff_6 = pre_dep.rel_diff_6,
         rel_diff_7 = pre_dep.rel_diff_7,
         rel_diff_8 = pre_dep.rel_diff_8,
         wind_speed = pre_dep.Wind_Speed_2,
         bearing_FL = pre_dep.bearing_FL,
         shield_angle = pre_dep.shield_angle,
         ground_distance_FL = pre_dep.ground_distance_FL,
         ground_Speed_ms = pre_dep.Ground_Speed_ms)

mar_arr <- mar_arr %>% 
  rename(date_sound = dat_arr.date_sound,
         rel_diff_5 = dat_arr.rel_diff_5,
         rel_diff_6 = dat_arr.rel_diff_6,
         rel_diff_7 = dat_arr.rel_diff_7,
         rel_diff_8 = dat_arr.rel_diff_8,
         wind_speed = dat_arr.Wind_Speed_2,
         bearing_FL = dat_arr.bearing_FL,
         shield_angle = dat_arr.shield_angle,
         ground_distance_FL = dat_arr.ground_distance_FL,
         ground_Speed_ms = dat_arr.Ground_Speed_ms)

mar_dep <- mar_dep %>% 
  rename(date_sound = dat_dep.date_sound,
         rel_diff_5 = dat_dep.rel_diff_5,
         rel_diff_6 = dat_dep.rel_diff_6,
         rel_diff_7 = dat_dep.rel_diff_7,
         rel_diff_8 = dat_dep.rel_diff_8,
         wind_speed = dat_dep.Wind_Speed_2,
         bearing_FL = dat_dep.bearing_FL,
         shield_angle = dat_dep.shield_angle,
         ground_distance_FL = dat_dep.ground_distance_FL,
         ground_Speed_ms = dat_dep.Ground_Speed_ms)

apr_arr <- apr_arr %>% 
  rename(date_sound = dat_arr.date_sound,
         rel_diff_5 = dat_arr.rel_diff_5,
         rel_diff_6 = dat_arr.rel_diff_6,
         rel_diff_7 = dat_arr.rel_diff_7,
         rel_diff_8 = dat_arr.rel_diff_8,
         wind_speed = dat_arr.Wind_Speed_2,
         bearing_FL = dat_arr.bearing_FL,
         shield_angle = dat_arr.shield_angle,
         ground_distance_FL = dat_arr.ground_distance_FL,
         ground_Speed_ms = dat_arr.Ground_Speed_ms)

apr_dep <- apr_dep %>% 
  rename(date_sound = dat_dep.date_sound,
         rel_diff_5 = dat_dep.rel_diff_5,
         rel_diff_6 = dat_dep.rel_diff_6,
         rel_diff_7 = dat_dep.rel_diff_7,
         rel_diff_8 = dat_dep.rel_diff_8,
         wind_speed = dat_dep.Wind_Speed_2,
         bearing_FL = dat_dep.bearing_FL,
         shield_angle = dat_dep.shield_angle,
         ground_distance_FL = dat_dep.ground_distance_FL,
         ground_Speed_ms = dat_dep.Ground_Speed_ms)

con1_arr <- con1_arr %>% 
  rename(date_sound = config_1_arr.date_sound,
         rel_diff_5 = config_1_arr.rel_diff_5,
         rel_diff_6 = config_1_arr.rel_diff_6,
         rel_diff_7 = config_1_arr.rel_diff_7,
         rel_diff_8 = config_1_arr.rel_diff_8,
         wind_speed = config_1_arr.Wind_Speed_2,
         bearing_FL = config_1_arr.bearing_FL,
         shield_angle = config_1_arr.shield_angle,
         ground_distance_FL = config_1_arr.ground_distance_FL,
         ground_Speed_ms = config_1_arr.Ground_Speed_ms)

con1_dep <- con1_dep %>% 
  rename(date_sound = config_1_dep.date_sound,
         rel_diff_5 = config_1_dep.rel_diff_5,
         rel_diff_6 = config_1_dep.rel_diff_6,
         rel_diff_7 = config_1_dep.rel_diff_7,
         rel_diff_8 = config_1_dep.rel_diff_8,
         wind_speed = config_1_dep.Wind_Speed_2,
         bearing_FL = config_1_dep.bearing_FL,
         shield_angle = config_1_dep.shield_angle,
         ground_distance_FL = config_1_dep.ground_distance_FL,
         ground_Speed_ms = config_1_dep.Ground_Speed_ms)

con2_arr <- con2_arr %>% 
  rename(date_sound = config_2_arr.date_sound,
         rel_diff_5 = config_2_arr.rel_diff_5,
         rel_diff_6 = config_2_arr.rel_diff_6,
         rel_diff_7 = config_2_arr.rel_diff_7,
         rel_diff_8 = config_2_arr.rel_diff_8,
         wind_speed = config_2_arr.Wind_Speed_2,
         bearing_FL = config_2_arr.bearing_FL,
         shield_angle = config_2_arr.shield_angle,
         ground_distance_FL = config_2_arr.ground_distance_FL,
         ground_Speed_ms = config_2_arr.Ground_Speed_ms)


con2_dep <- con2_dep %>% 
  rename(date_sound = config_2_dep.date_sound,
         rel_diff_5 = config_2_dep.rel_diff_5,
         rel_diff_6 = config_2_dep.rel_diff_6,
         rel_diff_7 = config_2_dep.rel_diff_7,
         rel_diff_8 = config_2_dep.rel_diff_8,
         wind_speed = config_2_dep.Wind_Speed_2,
         bearing_FL = config_2_dep.bearing_FL,
         shield_angle = config_2_dep.shield_angle,
         ground_distance_FL = config_2_dep.ground_distance_FL,
         ground_Speed_ms = config_2_dep.Ground_Speed_ms)

arrivals <- rbind(feb_prearr, feb_arr, mar_arr, apr_arr, con1_arr, con2_arr)
departures <-rbind(feb_predep,feb_dep, mar_dep, apr_dep, con1_arr, con2_dep)

#daily difference relative difference variables 

arr_daily <- arrivals %>% group_by(day_month = format(date_sound, "%d-%m")) %>%
  summarise(rel_5 = mean(rel_diff_5), rel_6 = mean(rel_diff_6), 
            rel_7 = mean(rel_diff_7), rel_8 = mean(rel_diff_8))

arr_daily$day_month <- as.POSIXct(arr_daily$day_month, format="%d-%m")
arr_daily <- arr_daily %>% arrange(day_month)



dep_daily <- departures %>% group_by(day_month = format(date_sound, "%d-%m")) %>%
  summarise(rel_5 = mean(rel_diff_5), rel_6 = mean(rel_diff_6), 
            rel_7 = mean(rel_diff_7), rel_8 = mean(rel_diff_8)) 

dep_daily$day_month <- as.POSIXct(dep_daily$day_month, format="%d-%m")
dep_daily <- dep_daily %>% arrange(day_month)


ggplot(arr_daily, aes(x = day_month, y = rel_5, color = "Mic 5")) +
  geom_line() +
  geom_line(aes(y = rel_6, color = "Mic 6")) +
  geom_line(aes(y = rel_7, color = "Mic 7")) +
  geom_line(aes(y= rel_8, color="Mic 8")) +
  labs(title = "Daily Relative LAeq,1s Differences for Arrivals",
       x = "Day of the Experiment",
       y = "Average Relative Difference with Reference Mic (dB)",
       color = "Microphones") +
  theme_minimal() +
  scale_color_manual(values = c("dodgerblue2", "firebrick1", "firebrick4", "dodgerblue4"))


ggplot(dep_daily, aes(x = day_month, y = rel_5, color = "Mic 5")) +
  geom_line() +
  geom_line(aes(y = rel_6, color = "Mic 6")) +
  geom_line(aes(y = rel_7, color = "Mic 7")) +
  geom_line(aes(y= rel_8, color="Mic 8")) +
  labs(title = "Daily Relative  LAeq,1s Differences for Departures",
       x = "Day of the Experiment",
       y = "Average Relative Difference with Reference Mic (dB)",
       color = "Microphones") +
  theme_minimal() +
  scale_color_manual(values = c("dodgerblue2", "firebrick1", "firebrick4", "dodgerblue4"))

