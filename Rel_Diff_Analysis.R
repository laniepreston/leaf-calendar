library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(car)
library(purrr)
library(stats)
library(ggplot2)

feb_shielded <- fread("data/working_copies/Feb_Shielded.csv")
feb_unshielded <- fread("data/working_copies/Feb_Unshielded.csv")
feb_preshielded <- fread("data/working_copies/Feb_PreShielded.csv")
feb_preunshielded <- fread("data/working_copies/Feb_PreUnshielded.csv")
mar_shielded <- fread("data/working_copies/Mar_Shielded.csv")
mar_unshielded <- fread("data/working_copies/Mar_Unshielded.csv")
apr_shielded <- fread("data/working_copies/Apr_Shielded.csv")
apr_unshielded <- fread("data/working_copies/Apr_Unshielded.csv")

#fix column names 
feb_shielded <- feb_shielded %>% 
  rename(date_sound = post_shielded.date_sound,
         rel_diff_5 = post_shielded.rel_diff_5,
         rel_diff_6 = post_shielded.rel_diff_6,
         rel_diff_7 = post_shielded.rel_diff_7,
         rel_diff_8 = post_shielded.rel_diff_8,
         wind_speed = post_shielded.Wind_Speed_2,
         bearing_FL = post_shielded.bearing_FL,
         shield_angle = post_shielded.shield_angle,
         ground_distance_FL = post_shielded.ground_distance_FL,
         ground_Speed_ms = post_shielded.Ground_Speed_ms)

feb_preshielded <- feb_preshielded %>% 
  rename(date_sound = pre_shielded.date_sound,
         rel_diff_5 = pre_shielded.rel_diff_5,
         rel_diff_6 = pre_shielded.rel_diff_6,
         rel_diff_7 = pre_shielded.rel_diff_7,
         rel_diff_8 = pre_shielded.rel_diff_8,
         wind_speed = pre_shielded.Wind_Speed_2,
         bearing_FL = pre_shielded.bearing_FL,
         shield_angle = pre_shielded.shield_angle,
         ground_distance_FL = pre_shielded.ground_distance_FL,
         ground_Speed_ms = pre_shielded.Ground_Speed_ms)
         

feb_unshielded <- feb_unshielded %>% 
  rename(date_sound = post_unshielded.date_sound,
         rel_diff_5 = post_unshielded.rel_diff_5,
         rel_diff_6 = post_unshielded.rel_diff_6,
         rel_diff_7 = post_unshielded.rel_diff_7,
         rel_diff_8 = post_unshielded.rel_diff_8,
         wind_speed = post_unshielded.Wind_Speed_2,
         bearing_FL = post_unshielded.bearing_FL,
         shield_angle = post_unshielded.shield_angle,
         ground_distance_FL = post_unshielded.ground_distance_FL,
         ground_Speed_ms = post_unshielded.Ground_Speed_ms)

feb_preunshielded <- feb_preunshielded %>% 
  rename(date_sound = pre_unshielded.date_sound,
         rel_diff_5 = pre_unshielded.rel_diff_5,
         rel_diff_6 = pre_unshielded.rel_diff_6,
         rel_diff_7 = pre_unshielded.rel_diff_7,
         rel_diff_8 = pre_unshielded.rel_diff_8,
         wind_speed = pre_unshielded.Wind_Speed_2,
         bearing_FL = pre_unshielded.bearing_FL,
         shield_angle = pre_unshielded.shield_angle,
         ground_distance_FL = pre_unshielded.ground_distance_FL,
         ground_Speed_ms = pre_unshielded.Ground_Speed_ms)

mar_shielded <- mar_shielded %>% 
  rename(date_sound = shielded.date_sound,
         rel_diff_5 = shielded.rel_diff_5,
         rel_diff_6 = shielded.rel_diff_6,
         rel_diff_7 = shielded.rel_diff_7,
         rel_diff_8 = shielded.rel_diff_8,
         wind_speed = shielded.Wind_Speed_1,
         bearing_FL = shielded.bearing_FL,
         shield_angle = shielded.shield_angle,
         ground_distance_FL = shielded.ground_distance_FL,
         ground_Speed_ms = shielded.Ground_Speed_ms)

mar_unshielded <- mar_unshielded %>% 
  rename(date_sound = unshielded.date_sound,
         rel_diff_5 = unshielded.rel_diff_5,
         rel_diff_6 = unshielded.rel_diff_6,
         rel_diff_7 = unshielded.rel_diff_7,
         rel_diff_8 = unshielded.rel_diff_8,
         wind_speed = unshielded.Wind_Speed_1,
         bearing_FL = unshielded.bearing_FL,
         shield_angle = unshielded.shield_angle,
         ground_distance_FL = unshielded.ground_distance_FL,
         ground_Speed_ms = unshielded.Ground_Speed_ms)

apr_shielded <- apr_shielded %>% 
  rename(date_sound = shielded.date_sound,
         rel_diff_5 = shielded.rel_diff_5,
         rel_diff_6 = shielded.rel_diff_6,
         rel_diff_7 = shielded.rel_diff_7,
         rel_diff_8 = shielded.rel_diff_8,
         wind_speed = shielded.Wind_Speed_1,
         bearing_FL = shielded.bearing_FL,
         shield_angle = shielded.shield_angle,
         ground_distance_FL = shielded.ground_distance_FL,
         ground_Speed_ms = shielded.Ground_Speed_ms)

apr_unshielded <- apr_unshielded %>% 
  rename(date_sound = unshielded.date_sound,
         rel_diff_5 = unshielded.rel_diff_5,
         rel_diff_6 = unshielded.rel_diff_6,
         rel_diff_7 = unshielded.rel_diff_7,
         rel_diff_8 = unshielded.rel_diff_8,
         wind_speed = unshielded.Wind_Speed_1,
         bearing_FL = unshielded.bearing_FL,
         shield_angle = unshielded.shield_angle,
         ground_distance_FL = unshielded.ground_distance_FL,
         ground_Speed_ms = unshielded.Ground_Speed_ms)

shielded <- rbind(feb_preshielded, feb_shielded, mar_shielded, apr_shielded)
unshielded <-rbind(feb_preunshielded,feb_unshielded, mar_unshielded, apr_unshielded)

#daily difference relative difference variables 

sh_daily <- shielded %>% group_by(day_month = format(date_sound, "%d-%m")) %>%
  summarise(rel_5 = mean(rel_diff_5), rel_6 = mean(rel_diff_6), 
            rel_7 = mean(rel_diff_7), rel_8 = mean(rel_diff_8))

sh_daily$day_month <- as.POSIXct(sh_daily$day_month, format="%d-%m")
sh_daily <- sh_daily %>% arrange(day_month)



unsh_daily <- unshielded %>% group_by(day_month = format(date_sound, "%d-%m")) %>%
  summarise(rel_5 = mean(rel_diff_5), rel_6 = mean(rel_diff_6), 
            rel_7 = mean(rel_diff_7), rel_8 = mean(rel_diff_8)) 

unsh_daily$day_month <- as.POSIXct(unsh_daily$day_month, format="%d-%m")
unsh_daily <- unsh_daily %>% arrange(day_month)


ggplot(sh_daily, aes(x = day_month, y = rel_5, color = "Mic 5")) +
  geom_line() +
  geom_line(aes(y = rel_6, color = "Mic 6")) +
  geom_line(aes(y = rel_7, color = "Mic 7")) +
  geom_line(aes(y= rel_8, color="Mic 8")) +
  labs(title = "Shielded Relative Differences",
       x = "Day of the Experiment",
       y = "Relative Difference with Reference Mic (dB)",
       color = "Microphones") +
  theme_minimal()


ggplot(unsh_daily, aes(x = day_month, y = rel_5, color = "Mic 5")) +
  geom_line() +
  geom_line(aes(y = rel_6, color = "Mic 6")) +
  geom_line(aes(y = rel_7, color = "Mic 7")) +
  geom_line(aes(y= rel_8, color="Mic 8")) +
  labs(title = "UnShielded Relative Differences",
       x = "Day of the Experiment",
       y = "Relative Difference with Reference Mic (dB)",
       color = "Microphones") +
  theme_minimal()

#ANOVA Analysis 
#looking for factors that influence the relative difference
model <- aov(rel_diff_11 ~ slant_angle + ground_distance_FL + Wind_Speed_1, data = shielded)
anova_results <- Anova(model, type = 2)
summary(anova_results)

