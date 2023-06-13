library(data.table)
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(car)
library(purrr)
library(stats)
library(ggplot2)
library(sjstats)

dat_may <- fread("data/working_copies/MayMUNISENSE_CASPER_DAVIS_COMBINED_2023_05copy.csv")
dat_apr <-  fread("data/working_copies/AprilMUNISENSE_CASPER_DAVIS_COMBINED_2023_04copy.csv")

# DATA PREPROCESSING 
#convert chr to timestamp
dat_may$date_sound <- as.POSIXct(dat_may$date_sound, format="%d-%m-%Y %H:%M")
dat_apr$date_sound <- as.POSIXct(dat_apr$date_sound, format="%d-%m-%Y %H:%M")

#confine dates to configuration experiment 
dat_apr <- dat_apr %>% filter(date_sound >= '2023-04-25')
dat_may <- dat_may %>% filter(date_sound <= '2023-05-15')

dat_may <- dat_may %>% drop_na(Temp_Out_2)

#filter by closest point
dat_apr <- dat_apr %>%
  mutate(prev_point = lag(closest_point),
         next_point = lead(closest_point)) %>%
  filter(closest_point == TRUE |
           prev_point == TRUE |
           next_point == TRUE |
           lag(closest_point, 2) == TRUE |
           lead(closest_point, 2) == TRUE)

dat_may <- dat_may %>%
  mutate(prev_point = lag(closest_point),
         next_point = lead(closest_point)) %>%
  filter(closest_point == TRUE |
           prev_point == TRUE |
           next_point == TRUE |
           lag(closest_point, 2) == TRUE |
           lead(closest_point, 2) == TRUE)


#merge dataframes 
dat_apr <- subset(dat_apr, select = -c(distance, angle, Wind_Dir_deg_2))
dat <- rbind(dat_apr, dat_may)


#remove unnecessary variables
dat <- dat %>% select(-contains(c('spectrum_z_16Hz', 'spectrum_z_20Hz', 'spectrum_z_10Hz', 
                                  "spectrum_z_50Hz", "spectrum_z_40Hz", "spectrum_z_63Hz",    
                                  "spectrum_z_80Hz", "spectrum_z_160Hz", "spectrum_z_200Hz", 
                                  "spectrum_z_400Hz", "spectrum_z_630Hz",   "spectrum_z_800Hz", 
                                  "spectrum_z_1250Hz",  "spectrum_z_1600Hz",  "spectrum_z_2500Hz",  
                                  "spectrum_z_3150Hz",  "spectrum_z_4000Hz", 
                                  "spectrum_z_5000Hz",  "spectrum_z_6300Hz",  "spectrum_z_8000Hz", 
                                  "spectrum_z_10000Hz", "spectrum_z_12500Hz", "spectrum_z_16000Hz",
                                  "spectrum_z_20000Hz")))

#label configurations 
date_change <- as.Date("2023-05-05")
dat$config <- ifelse(dat$date_sound < date_change, 1, 2)

#add variable counting each day from April 25th to somewhat account for leaf growth (in the future, the LAI value can be used here)
dat$leaf_growth <- with(rle(as.character(as.Date(dat$date_sound), format = "%Y-%m-%d  %H:%M")), rep(1:length(values), lengths))

#filter out high winds
dat <- dat %>% subset(!Wind_Speed_2 > 12 || !Wind_Speed_4 >12)

#filter data when planes are too far away from field lab (in line w/
#other months)
dat <- dat %>% filter(ground_distance_FL < 1000) 

#split up into configurations 
config_1 <- dat %>% filter(config == 1)
config_2 <- dat %>% filter(config == 2)

#further split into departures versus arrivals 
dat_split <- split(config_1, config_1$OP)
config_1_arr <- dat_split$A
config_1_dep <- dat_split$D


dat_split <- split(config_2, config_2$OP)
config_2_arr <- dat_split$A
config_2_dep <- dat_split$D

# Create histogram with the selected variables
selected_vars <- config_1_arr[, c("bearing_FL", "slant_angle", "Wind_Speed_2")]
hist_dat <- stack(selected_vars)
hist(hist_dat$values, col = c("dodgerblue3", "firebrick1", "lightskyblue3"), border = "white",
     xlab = "Value", ylab = "Frequency", main = "Varation of Selected Variables")
legend("topright", legend = names(selected_vars), fill = c("dodgerblue3", "firebrick1", "lightskyblue3"))

#repeat for other dataframes 
selected_vars <- config_1_dep[, c("bearing_FL", "slant_angle", "Wind_Speed_2")]
hist_dat <- stack(selected_vars)
hist(hist_dat$values, col = c("dodgerblue3", "firebrick1", "lightskyblue3"), border = "white",
     xlab = "Value", ylab = "Frequency", main = "Varation of Selected Variables")
legend("topright", legend = names(selected_vars), fill = c("dodgerblue3", "firebrick1", "lightskyblue3"))

selected_vars <- config_2_arr[, c("bearing_FL", "slant_angle", "Wind_Speed_2")]
hist_dat <- stack(selected_vars)
hist(hist_dat$values, col = c("dodgerblue3", "firebrick1", "lightskyblue3"), border = "white",
     xlab = "Value", ylab = "Frequency", main = "Varation of Selected Variables")
legend("topright", legend = names(selected_vars), fill = c("dodgerblue3", "firebrick1", "lightskyblue3"))

selected_vars <- config_2_dep[, c("bearing_FL", "slant_angle", "Wind_Speed_2")]
hist_dat <- stack(selected_vars)
hist(hist_dat$values, col = c("dodgerblue3", "firebrick1", "lightskyblue3"), border = "white",
     xlab = "Value", ylab = "Frequency", main = "Varation of Selected Variables")
legend("topright", legend = names(selected_vars), fill = c("dodgerblue3", "firebrick1", "lightskyblue3"))



#DATA ANALYSIS 

#calculate the overall stats for each config for data table
#find the average laeq across all courtyard microphones 
config_1_arr <- mutate(config_1_arr, avg_exp = rowMeans(cbind(laeq_7, laeq_6, laeq_5, laeq_8)))
config_1_dep <- mutate(config_1_dep, avg_exp = rowMeans(cbind(laeq_7, laeq_6, laeq_5, laeq_8)))
config_2_arr <- mutate(config_2_arr, avg_exp = rowMeans(cbind(laeq_7, laeq_6, laeq_5, laeq_8)))
config_2_dep <- mutate(config_2_dep, avg_exp = rowMeans(cbind(laeq_7, laeq_6, laeq_5, laeq_8)))

#add column for relative difference with the rooftop microphone 
config_1_arr<- mutate(config_1_arr, rel_diff_11 = laeq_11 - avg_exp)
mean(config_1_arr$rel_diff_11)
sd(config_1_arr$rel_diff_11)
min(config_1_arr$rel_diff_11)
max(config_1_arr$rel_diff_11)

config_1_dep<- mutate(config_1_dep, rel_diff_11 = laeq_11 - avg_exp)
mean(config_1_dep$rel_diff_11)
sd(config_1_dep$rel_diff_11)
min(config_1_dep$rel_diff_11)
max(config_1_dep$rel_diff_11)

config_2_arr<- mutate(config_2_arr, rel_diff_11 = laeq_11 - avg_exp)
mean(config_2_arr$rel_diff_11)
sd(config_2_arr$rel_diff_11)
min(config_2_arr$rel_diff_11)
max(config_2_arr$rel_diff_11)

config_2_dep<- mutate(config_2_dep, rel_diff_11 = laeq_11 - avg_exp)
mean(config_2_dep$rel_diff_11)
sd(config_2_dep$rel_diff_11)
min(config_2_dep$rel_diff_11)
max(config_2_dep$rel_diff_11)


#individual microphone analysis 

#for configuration 1
#Mic 5 
config_1_arr<- mutate(config_1_arr, rel_diff_5 = laeq_11 - laeq_5)
mean(config_1_arr$rel_diff_5)
sd(config_1_arr$rel_diff_5)
min(config_1_arr$rel_diff_5)
max(config_1_arr$rel_diff_5)

config_1_dep <- mutate(config_1_dep, rel_diff_5 = laeq_11 - laeq_5)
mean(config_1_dep$rel_diff_5)
sd(config_1_dep$rel_diff_5)
min(config_1_dep$rel_diff_5)
max(config_1_dep$rel_diff_5)

#Mic 6 
config_1_arr <- mutate(config_1_arr, rel_diff_6 = laeq_11 - laeq_6)
mean(config_1_arr$rel_diff_6)
sd(config_1_arr$rel_diff_6)
min(config_1_arr$rel_diff_6)
max(config_1_arr$rel_diff_6)

config_1_dep <- mutate(config_1_dep, rel_diff_6 = laeq_11 - laeq_6)
mean(config_1_dep$rel_diff_6)
sd(config_1_dep$rel_diff_6)
min(config_1_dep$rel_diff_6)
max(config_1_dep$rel_diff_6)

#Mic 7
config_1_arr<- mutate(config_1_arr, rel_diff_7 = laeq_11 - laeq_7)
mean(config_1_arr$rel_diff_7)
sd(config_1_arr$rel_diff_7)
min(config_1_arr$rel_diff_7)
max(config_1_arr$rel_diff_7)

config_1_dep <- mutate(config_1_dep, rel_diff_7 = laeq_11 - laeq_7)
mean(config_1_dep$rel_diff_7)
sd(config_1_dep$rel_diff_7)
min(config_1_dep$rel_diff_7)
max(config_1_dep$rel_diff_7)

#Mic 8
config_1_arr<- mutate(config_1_arr, rel_diff_8 = laeq_11 - laeq_8)
mean(config_1_arr$rel_diff_8)
sd(config_1_arr$rel_diff_8)
min(config_1_arr$rel_diff_8)
max(config_1_arr$rel_diff_8)

config_1_dep <- mutate(config_1_dep, rel_diff_8 = laeq_11 - laeq_8)
mean(config_1_dep$rel_diff_8)
sd(config_1_dep$rel_diff_8)
min(config_1_dep$rel_diff_8)
max(config_1_dep$rel_diff_8)


#for configuration 2
config_2_arr<- mutate(config_2_arr, rel_diff_5 = laeq_11 - laeq_5)
mean(config_2_arr$rel_diff_5)
sd(config_2_arr$rel_diff_5)
min(config_2_arr$rel_diff_5)
max(config_2_arr$rel_diff_5)

config_2_dep <- mutate(config_2_dep, rel_diff_5 = laeq_11 - laeq_5)
mean(config_2_dep$rel_diff_5)
sd(config_2_dep$rel_diff_5)
min(config_2_dep$rel_diff_5)
max(config_2_dep$rel_diff_5)

#Mic 6 
config_2_arr <- mutate(config_2_arr, rel_diff_6 = laeq_11 - laeq_6)
mean(config_2_arr$rel_diff_6)
sd(config_2_arr$rel_diff_6)
min(config_2_arr$rel_diff_6)
max(config_2_arr$rel_diff_6)

config_2_dep <- mutate(config_2_dep, rel_diff_6 = laeq_11 - laeq_6)
mean(config_2_dep$rel_diff_6)
sd(config_2_dep$rel_diff_6)
min(config_2_dep$rel_diff_6)
max(config_2_dep$rel_diff_6)

#Mic 7
config_2_arr<- mutate(config_2_arr, rel_diff_7 = laeq_11 - laeq_7)
mean(config_2_arr$rel_diff_7)
sd(config_2_arr$rel_diff_7)
min(config_2_arr$rel_diff_7)
max(config_2_arr$rel_diff_7)

config_2_dep <- mutate(config_2_dep, rel_diff_7 = laeq_11 - laeq_7)
mean(config_2_dep$rel_diff_7)
sd(config_2_dep$rel_diff_7)
min(config_2_dep$rel_diff_7)
max(config_2_dep$rel_diff_7)

#Mic 8
config_2_arr<- mutate(config_2_arr, rel_diff_8 = laeq_11 - laeq_8)
mean(config_2_arr$rel_diff_8)
sd(config_1_arr$rel_diff_8)
min(config_1_arr$rel_diff_8)
max(config_1_arr$rel_diff_8)

config_2_dep <- mutate(config_2_dep, rel_diff_8 = laeq_11 - laeq_8)
mean(config_2_dep$rel_diff_8)
sd(config_2_dep$rel_diff_8)
min(config_2_dep$rel_diff_8)
max(config_2_dep$rel_diff_8)

#visualizations 
arr_daily <- config_1_arr %>% group_by(day_month = format(date_sound, "%d-%m")) %>%
  summarise(rel_5 = mean(rel_diff_5), rel_6 = mean(rel_diff_6), 
            rel_7 = mean(rel_diff_7), rel_8 = mean(rel_diff_8))

arr_daily$day_month <- as.POSIXct(arr_daily$day_month, format="%d-%m")
arr_daily <- arr_daily %>% arrange(day_month)



dep_daily <- config_1_dep %>% group_by(day_month = format(date_sound, "%d-%m")) %>%
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

con1_arr <- data.frame(config_1_arr$date_sound, config_1_arr$rel_diff_5, 
                      config_1_arr$rel_diff_6, config_1_arr$rel_diff_7, 
                      config_1_arr$rel_diff_8, config_1_arr$Wind_Speed_2, 
                      config_1_arr$bearing_FL, config_1_arr$shield_angle,
                      config_1_arr$ground_distance_FL, config_1_arr$Ground_Speed_ms)

con1_dep <- data.frame(config_1_dep$date_sound, config_1_dep$rel_diff_5, 
                       config_1_dep$rel_diff_6, config_1_dep$rel_diff_7, 
                       config_1_dep$rel_diff_8, config_1_dep$Wind_Speed_2, 
                       config_1_dep$bearing_FL, config_1_dep$shield_angle,
                       config_1_dep$ground_distance_FL, config_1_dep$Ground_Speed_ms)

con2_arr <- data.frame(config_2_arr$date_sound, config_2_arr$rel_diff_5, 
                       config_2_arr$rel_diff_6, config_2_arr$rel_diff_7, 
                       config_2_arr$rel_diff_8, config_2_arr$Wind_Speed_2, 
                       config_2_arr$bearing_FL, config_2_arr$shield_angle,
                       config_2_arr$ground_distance_FL, config_2_arr$Ground_Speed_ms)

con2_dep <- data.frame(config_2_dep$date_sound, config_2_dep$rel_diff_5, 
                       config_2_dep$rel_diff_6, config_2_dep$rel_diff_7, 
                       config_2_dep$rel_diff_8, config_2_dep$Wind_Speed_2, 
                       config_2_dep$bearing_FL, config_2_dep$shield_angle,
                       config_2_dep$ground_distance_FL, config_2_dep$Ground_Speed_ms)
#write the dataframes to be used later for data visualizations 
write.csv(con1_arr, "data/working_copies/split_datasets/Con1_Arr.csv", row.names=FALSE)
write.csv(con1_dep, "data/working_copies/split_datasets/Con1_Dep.csv", row.names=FALSE)
write.csv(con2_arr, "data/working_copies/split_datasets/Con2_Arr.csv", row.names=FALSE)
write.csv(con2_dep, "data/working_copies/split_datasets/Con2_Dep.csv", row.names=FALSE)


#T tests
#arrivals versus departures 
t.test(config_1_arr$avg_exp, config_1_arr$laeq_11, paired = TRUE) 
t.test(config_1_dep$avg_exp, config_1_dep$laeq_11, paired=TRUE)
t.test(config_2_arr$avg_exp, config_2_arr$laeq_11, paired=TRUE)
t.test(config_2_dep$avg_exp, config_2_dep$laeq_11, paired=TRUE)



#examining significance of other variables 
config_1_arr$ACTYPE <- factor(config_1_arr$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) +  scale(Temp_Out_2) + scale(leaf_growth) + ACTYPE, data = config_1_arr)
summary(model)
coefficients(model)
summary(model)$r.squared

#repeat process for the other variables 
config_1_dep$ACTYPE <- factor(config_1_dep$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) +  scale(Temp_Out_2) + scale(leaf_growth) + ACTYPE, data = config_1_dep)
summary(model)
coefficients(model)
summary(model)$r.squared 

config_2_arr$ACTYPE <- factor(config_2_arr$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) +  scale(Temp_Out_2) + scale(leaf_growth) + ACTYPE, data = config_2_arr)
summary(model)
coefficients(model)
summary(model)$r.squared 

config_2_dep$ACTYPE <- factor(config_2_dep$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) +  scale(Temp_Out_2) + scale(leaf_growth) + ACTYPE, data = config_2_dep)
summary(model)
coefficients(model)
summary(model)$r.squared #substantially higher r squared value here - bearing and slant_angle playing a major role 
