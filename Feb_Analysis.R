library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(car)
library(purrr)
library(stats)
library(ggplot2)

dat <- fread("data/working_copies/FEB_MUNISENSE_CASPER_DAVIS_COMBINED_CY2_2023_02copy.csv")
dat2 <- fread("data/working_copies/MUNISENSE_CASPER_DAVIS_COMBINED_CY1_2023_02copy.csv")

# DATA PREPROCESSING 
#convert chr to timestamp
dat$date_sound <- as.POSIXct(dat$date_sound, format="%d/%m/%Y %H:%M")
dat2$date_sound <- as.POSIXct(dat2$date_sound, format="%d/%m/%Y %H:%M")

dat <- merge(dat, select(dat2, date_sound, laeq_avg_4, spectrum_z_avg_100Hz_4, spectrum_z_avg_250Hz_4, spectrum_z_avg_500Hz_4,
                  laeq_avg_3, spectrum_z_avg_100Hz_3, spectrum_z_avg_250Hz_3, spectrum_z_avg_500Hz_3,
                  laeq_avg_2, spectrum_z_avg_100Hz_2, spectrum_z_avg_250Hz_2, spectrum_z_avg_500Hz_2,
                  laeq_avg_1, spectrum_z_avg_100Hz_1, spectrum_z_avg_250Hz_1, spectrum_z_avg_500Hz_1), by = "date_sound", all.x = FALSE)

#reduce sound measurements to decibels and 5 characteristic frequencies  
dat <- dat %>% select(-contains(c('spectrum_z_avg_16Hz', 'spectrum_z_avg_20Hz', 'spectrum_z_avg_10Hz', 
                                  "spectrum_z_avg_50Hz", "spectrum_z_avg_40Hz", "spectrum_z_avg_63Hz",    
                                  "spectrum_z_avg_80Hz", "spectrum_z_avg_160Hz", "spectrum_z_avg_200Hz", 
                                  "spectrum_z_avg_400Hz", "spectrum_z_avg_630Hz",   "spectrum_z_avg_800Hz", 
                                  "spectrum_z_avg_1250Hz",  "spectrum_z_avg_1600Hz",  "spectrum_z_avg_2500Hz",  
                                  "spectrum_z_avg_3150Hz",  "spectrum_z_avg_4000Hz", 
                                  "spectrum_z_avg_5000Hz",  "spectrum_z_avg_6300Hz",  "spectrum_z_avg_8000Hz", 
                                  "spectrum_z_avg_10000Hz", "spectrum_z_avg_12500Hz", "spectrum_z_avg_16000Hz",
                                  "spectrum_z_avg_20000Hz")))

#filter out windspeeds of more than 12 m/s
dat <- dat %>% subset(!Wind_Speed_2 > 12 || !Wind_Speed_4 >12)

#only look at points when plane is closest to FL to remove some variance
dat <- dat %>% subset(closest_point == TRUE)
#Filter outliers in the bearing 
dat <- dat %>% filter(bearing_FL >= 100 & bearing_FL <= 250)

#2 clusters of bearings, around -125 and 50 degrees. Use for cross-analysis later 
hist(dat$Bearing)

boxplot(dat$spectrum_z_avg_100Hz_1)

#create data frames for pre and post tree placement, filter outliers  
dat_pre <- dat %>% filter(date_sound < '2023-02-20' & laeq_avg_5 > 30)
dat_post <- dat %>% filter(date_sound >= '2023-02-20') 


#Split microphones by shielded or not shielded 
dat_split <- split(dat_pre, dat_pre$shielded)
pre_shielded <-  dat_split$'TRUE'
pre_unshielded <- dat_split$'FALSE'

dat_split <- split(dat_post, dat_post$shielded)
post_shielded <-  dat_split$'TRUE'
post_unshielded <- dat_split$'FALSE'


# DATA ANALYSIS 
#summary stats all mics 
pre_shielded %>% 
  select(laeq_avg_11, laeq_avg_8, laeq_avg_7, laeq_avg_6, laeq_avg_5, laeq_avg_4, laeq_avg_3, laeq_avg_2, laeq_avg_1) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

pre_unshielded %>% 
  select(laeq_avg_11, laeq_avg_8, laeq_avg_7, laeq_avg_6, laeq_avg_5, laeq_avg_4, laeq_avg_3, laeq_avg_2, laeq_avg_1) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

post_shielded %>% 
  select(laeq_avg_11, laeq_avg_8, laeq_avg_7, laeq_avg_6, laeq_avg_5,laeq_avg_4, laeq_avg_3, laeq_avg_2, laeq_avg_1) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

post_unshielded %>% 
  select(laeq_avg_11, laeq_avg_8, laeq_avg_7, laeq_avg_6, laeq_avg_5, laeq_avg_4, laeq_avg_3, laeq_avg_2, laeq_avg_1) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

#use KS test to check for a normal distribution amongst shielded mics 

#WITH ROOFTOP MIC 
pre_shielded <- mutate(pre_shielded, avg_exp = rowMeans(cbind(laeq_avg_7,laeq_avg_6, laeq_avg_5, laeq_avg_8)))
ks.test(pre_shielded$avg_exp, pre_shielded$laeq_avg_11)

post_shielded <- mutate(post_shielded, avg_exp = rowMeans(cbind(laeq_avg_7, laeq_avg_6, laeq_avg_5, laeq_avg_8)))
ks.test(post_shielded$avg_exp, post_shielded$laeq_avg_11)

pre_unshielded <- mutate(pre_unshielded, avg_exp = rowMeans(cbind(laeq_avg_7, laeq_avg_6, laeq_avg_5, laeq_avg_8)))
ks.test(pre_unshielded$avg_exp, pre_unshielded$laeq_avg_11)

post_unshielded <- mutate(post_unshielded, avg_exp = rowMeans(cbind(laeq_avg_7, laeq_avg_6, laeq_avg_5, laeq_avg_8)))
ks.test(post_unshielded$avg_exp, post_unshielded$laeq_avg_11)


#WITH SLANTED COURTYARD MICS 
pre_shielded <- mutate(pre_shielded, avg_ctrl = rowMeans(cbind(laeq_avg_1, laeq_avg_2, laeq_avg_3, laeq_avg_4)))
ks.test(pre_shielded$avg_exp, pre_shielded$avg_ctrl)

post_shielded <- mutate(post_shielded, avg_ctrl = rowMeans(cbind(laeq_avg_1, laeq_avg_2, laeq_avg_3, laeq_avg_4)))
ks.test(post_shielded$avg_exp, post_shielded$avg_ctrl)

pre_unshielded <- mutate(pre_unshielded, avg_ctrl = rowMeans(cbind(laeq_avg_1, laeq_avg_2, laeq_avg_3, laeq_avg_4)))
ks.test(pre_unshielded$avg_exp, pre_unshielded$avg_ctrl)

post_unshielded <- mutate(post_unshielded, avg_ctrl = rowMeans(cbind(laeq_avg_1, laeq_avg_2, laeq_avg_3, post_unshielded$laeq_avg_4)))
ks.test(post_unshielded$avg_exp, post_unshielded$avg_ctrl)


#T tests 
#shielded  
#rooftop mics
t.test(pre_shielded$avg_exp, pre_shielded$laeq_avg_11, paired = TRUE)
t.test(post_shielded$avg_exp, post_shielded$laeq_avg_11, paired=TRUE)

#slanted courtyard mics 
t.test(pre_shielded$avg_exp, pre_shielded$avg_ctrl)
t.test(post_shielded$avg_exp, post_shielded$avg_ctrl)

#unshielded 
#rooftop mics 
t.test(pre_unshielded$avg_exp, pre_unshielded$laeq_avg_11, paired = TRUE)
t.test(post_unshielded$avg_exp, post_unshielded$laeq_avg_11, paired=TRUE)

#slanted courtyard mics
t.test(pre_unshielded$avg_exp, pre_unshielded$avg_ctrl)
t.test(post_unshielded$avg_exp, post_unshielded$avg_ctrl)


#decided to also check low-frequency sounds to test if the trunks impact 
post_unshielded <- mutate(post_unshielded, avg_exp_100hz = rowMeans(cbind(spectrum_z_avg_100Hz_8, spectrum_z_avg_100Hz_7, spectrum_z_avg_100Hz_6, spectrum_z_avg_100Hz_5)))
post_unshielded <- mutate(post_unshielded, avg_ctrl_100hz = rowMeans(cbind(spectrum_z_avg_100Hz_1, spectrum_z_avg_100Hz_2, spectrum_z_avg_100Hz_3, spectrum_z_avg_100Hz_4)))
t.test(post_unshielded$avg_exp_100hz, post_unshielded$avg_ctrl_100hz)

post_shielded <- mutate(post_shielded, avg_exp_100hz = rowMeans(cbind(spectrum_z_avg_100Hz_8, spectrum_z_avg_100Hz_7, spectrum_z_avg_100Hz_6, spectrum_z_avg_100Hz_5)))
post_shielded <- mutate(post_shielded, avg_ctrl_100hz = rowMeans(cbind(spectrum_z_avg_100Hz_1, spectrum_z_avg_100Hz_2, spectrum_z_avg_100Hz_3, spectrum_z_avg_100Hz_4)))
t.test(post_shielded$avg_exp_100hz, post_shielded$avg_ctrl_100hz)


#mic to mic comparison graph 

dat_post <- mutate(dat_post, rel_diff_5 = laeq_avg_5 - laeq_avg_11)
dat_post <- mutate(dat_post, rel_diff_6 = laeq_avg_6 - laeq_avg_11)
dat_post <- mutate(dat_post, rel_diff_7 = laeq_avg_7 - laeq_avg_11)
dat_post <- mutate(dat_post, rel_diff_8 = laeq_avg_8 - laeq_avg_11)

dat_post$hour <- hour(dat_post$date_sound)
dat_post$date <- date(dat_post$date_sound)

dat_post_grouped <- dat_post %>%
  group_by(date, hour)

dat_hourly_mean <- dat_post_grouped %>%
  summarize(hourly_mic5 = mean(rel_diff_5),
            hourly_mic6 = mean(rel_diff_6),
            hourly_mic7 = mean(rel_diff_7),
            hourly_mic8 = mean(rel_diff_8))

ggplot(dat_hourly_mean, aes(x = date, y = hourly_mic5, color = "Mic 5")) +
  geom_line() +
  geom_line(aes(y = hourly_mic6, color = "Mic 6")) +
  geom_line(aes(y = hourly_mic7, color = "Mic 7")) +
  geom_line(aes(y=hourly_mic8, color="Mic 8")) +
  labs(title = "Hourly Mean Differences with Reference Mic",
       x = "Date",
       y = "Relative Difference with Reference Mic",
       color = "Microphones") +
  theme_minimal()


ggplot(dat_post, aes(x = date_sound)) + # Specify data and aesthetics
  geom_line(aes(y = rel_diff_5, color = "Mic. 5")) +
  geom_line(aes(y = rel_diff_6, color = "Mic. 6")) +
  geom_line(aes(y = rel_diff_7, color = "Mic. 7")) +
  geom_line(aes(y = rel_diff_8, color = "Mic. 8")) + 
  labs(x = "Time", y = "Db Level", title = "Line Graph with Two Lines over Time")  # Add labels


#ANOVA 
#Test dif between mics and control as a function of whether or not they are shielded 
dat_pre <- mutate(dat_pre, avg_exp = rowMeans(cbind(laeq_avg_5, laeq_avg_6, laeq_avg_7, laeq_avg_8)))
dat_pre <- mutate(dat_pre, rel_dif = avg_exp - laeq_avg_11)
model <- aov(rel_dif ~ shielded, data = dat_pre)
summary(model)

dat_post <- mutate(dat_post, avg_exp = rowMeans(cbind(laeq_avg_5, laeq_avg_6, laeq_avg_7, laeq_avg_8)))
dat_post <- mutate(dat_post, rel_dif = avg_exp - laeq_avg_11)
model <- aov(rel_dif ~ Wind_Speed_4, data = dat_post)
summary(model)

#Shielding alone does not explain much variance - on to the linear model 
model <- lm(rel_dif ~ shielded * bearing_FL * slant_angle * ground_distance_FL * Wind_Speed_2, data = dat_pre)
anova_results <- Anova(model, type = 2)
summary(anova_results)

#using backwards selection 
model <- lm(rel_dif ~ bearing_FL * ground_distance_FL * shielded * Wind_Speed_2, data = dat_pre)
anova_results <- Anova(model, type = 2)
summary(anova_results)
