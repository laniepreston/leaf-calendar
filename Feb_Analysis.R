library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(car)
library(purrr)
library(stats)


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

#only look at points when plane is closest to FL(taking points on either side 
#of closest point to account for slight variations in aircraft noise) 

dat <- dat %>%
  mutate(prev_point = lag(closest_point),
         next_point = lead(closest_point)) %>%
  filter(closest_point == TRUE |
           prev_point == TRUE |
           next_point == TRUE |
           lag(closest_point, 2) == TRUE |
           lead(closest_point, 2) == TRUE)


#filter data when planes are too far away from field lab (in line w/
#other months)
dat <- dat %>% filter(ground_distance_FL < 1000) 

#create data frames for pre and post tree placement, filter outliers  
dat_pre <- dat %>% filter(date_sound < '2023-02-20' & laeq_avg_5 > 30)
dat_post <- dat %>% filter(date_sound >= '2023-02-20') 

#export dat_post for later analysis
write.csv(dat_post, "data/working_copies/filtered/Feb_Filtered.csv", row.names=FALSE)


#Split dataset by departures and arrivals 
dat_split <- split(dat_pre, dat_pre$OP)
pre_arr <- dat_split$A
pre_dep <- dat_split$D

dat_split <- split(dat_post, dat_post$OP)
post_arr <- dat_split$A
post_dep <- dat_split$D

#graph some variables to see sources of variation
#wind direction 2, Bearing_FL, slant_angle, OP
selected_vars <- post_dep[, c("bearing_FL", "slant_angle", "Wind_Dir_deg_2")]

# Create histogram with the selected variables
hist_dat <- stack(selected_vars)
hist(hist_dat$values, col = c("dodgerblue3", "firebrick1", "lightskyblue3"), border = "white",
     xlab = "Value", ylab = "Frequency", main = "Varation of Selected Variables")
legend("topright", legend = names(selected_vars), fill = c("dodgerblue3", "firebrick1", "lightskyblue3"))

# DATA ANALYSIS 

#cross-analysis by arrivals vs departures 
pre_arr <- mutate(pre_arr, avg_exp = rowMeans(cbind(laeq_avg_7,laeq_avg_6, laeq_avg_5, laeq_avg_8)))
ks.test(pre_arr$avg_exp, pre_arr$laeq_avg_11)

pre_dep <- mutate(pre_dep, avg_exp = rowMeans(cbind(laeq_avg_7,laeq_avg_6, laeq_avg_5, laeq_avg_8)))
ks.test(pre_dep$avg_exp, pre_dep$laeq_avg_11)

post_arr <- mutate(post_arr, avg_exp = rowMeans(cbind(laeq_avg_7,laeq_avg_6, laeq_avg_5, laeq_avg_8)))
ks.test(post_arr$avg_exp, post_arr$laeq_avg_11)

post_dep <- mutate(post_dep, avg_exp = rowMeans(cbind(laeq_avg_7,laeq_avg_6, laeq_avg_5, laeq_avg_8)))
ks.test(post_dep$avg_exp, post_dep$laeq_avg_11)


#Finding overall avg relative difference for data table in thesis

#by departures vs. arrivals
pre_arr <- mutate(pre_arr, rel_diff_11 = laeq_avg_11 - avg_exp)
mean(pre_arr$rel_diff_11)
sd(pre_arr$rel_diff_11)
min(pre_arr$rel_diff_11)
max(pre_arr$rel_diff_11)

pre_dep <- mutate(pre_dep, rel_diff_11 = laeq_avg_11 - avg_exp)
mean(pre_dep$rel_diff_11)
sd(pre_dep$rel_diff_11)
min(pre_dep$rel_diff_11)
max(pre_dep$rel_diff_11)

post_arr <- mutate(post_arr, rel_diff_11 = laeq_avg_11 - avg_exp)
mean(post_arr$rel_diff_11)
sd(post_arr$rel_diff_11)
min(post_arr$rel_diff_11)
max(post_arr$rel_diff_11)

post_dep<- mutate(post_dep, rel_diff_11 = laeq_avg_11 - avg_exp)
mean(post_dep$rel_diff_11)
sd(post_dep$rel_diff_11)
min(post_dep$rel_diff_11)
max(post_dep$rel_diff_11)

#individual microphones  

#Mic 5 
pre_arr <- mutate(pre_arr, rel_diff_5 = laeq_avg_11 - laeq_avg_5)
mean(pre_arr$rel_diff_5)
sd(pre_arr$rel_diff_5)
min(pre_arr$rel_diff_5)
max(pre_arr$rel_diff_5)

pre_dep <- mutate(pre_dep, rel_diff_5 = laeq_avg_11 - laeq_avg_5)
mean(pre_dep$rel_diff_5)
sd(pre_dep$rel_diff_5)
min(pre_dep$rel_diff_5)
max(pre_dep$rel_diff_5)

post_arr <- mutate(post_arr, rel_diff_5 = laeq_avg_11 - laeq_avg_5)
mean(post_arr$rel_diff_5)
sd(post_arr$rel_diff_5)
min(post_arr$rel_diff_5)
max(post_arr$rel_diff_5)

post_dep <- mutate(post_dep, rel_diff_5 = laeq_avg_11 - laeq_avg_5)
mean(post_dep$rel_diff_5)
sd(post_dep$rel_diff_5)
min(post_dep$rel_diff_5)
max(post_dep$rel_diff_5)


#Mic 6
pre_arr <- mutate(pre_arr, rel_diff_6 = laeq_avg_11 - laeq_avg_6)
mean(pre_arr$rel_diff_6)
sd(pre_arr$rel_diff_6)
min(pre_arr$rel_diff_6)
max(pre_arr$rel_diff_6)

pre_dep <- mutate(pre_dep, rel_diff_6 = laeq_avg_11 - laeq_avg_6)
mean(pre_dep$rel_diff_6)
sd(pre_dep$rel_diff_6)
min(pre_dep$rel_diff_6)
max(pre_dep$rel_diff_6)

post_arr <- mutate(post_arr, rel_diff_6 = laeq_avg_11 - laeq_avg_6)
mean(post_arr$rel_diff_6)
sd(post_arr$rel_diff_6)
min(post_arr$rel_diff_6)
max(post_arr$rel_diff_6)

post_dep <- mutate(post_dep, rel_diff_6 = laeq_avg_11 - laeq_avg_6)
mean(post_dep$rel_diff_6)
sd(post_dep$rel_diff_6)
min(post_dep$rel_diff_6)
max(post_dep$rel_diff_6)


#Mic 7

pre_arr <- mutate(pre_arr, rel_diff_7 = laeq_avg_11 - laeq_avg_7)
mean(pre_arr$rel_diff_7)
sd(pre_arr$rel_diff_7)
min(pre_arr$rel_diff_7)
max(pre_arr$rel_diff_7)

pre_dep <- mutate(pre_dep, rel_diff_7 = laeq_avg_11 - laeq_avg_7)
mean(pre_dep$rel_diff_7)
sd(pre_dep$rel_diff_7)
min(pre_dep$rel_diff_7)
max(pre_dep$rel_diff_7)

post_arr <- mutate(post_arr, rel_diff_7 = laeq_avg_11 - laeq_avg_7)
mean(post_arr$rel_diff_7)
sd(post_arr$rel_diff_7)
min(post_arr$rel_diff_7)
max(post_arr$rel_diff_7)

post_dep <- mutate(post_dep, rel_diff_7 = laeq_avg_11 - laeq_avg_7)
mean(post_dep$rel_diff_7)
sd(post_dep$rel_diff_7)
min(post_dep$rel_diff_7)
max(post_dep$rel_diff_7)

#Mic 8
pre_arr <- mutate(pre_arr, rel_diff_8 = laeq_avg_11 - laeq_avg_8)
mean(pre_arr$rel_diff_8)
sd(pre_arr$rel_diff_8)
min(pre_arr$rel_diff_8)
max(pre_arr$rel_diff_8)

pre_dep <- mutate(pre_dep, rel_diff_8 = laeq_avg_11 - laeq_avg_8)
mean(pre_dep$rel_diff_8)
sd(pre_dep$rel_diff_8)
min(pre_dep$rel_diff_8)
max(pre_dep$rel_diff_8)
 
post_arr <- mutate(post_arr, rel_diff_8 = laeq_avg_11 - laeq_avg_8)
mean(post_arr$rel_diff_8)
sd(post_arr$rel_diff_8)
min(post_arr$rel_diff_8)
max(post_arr$rel_diff_8)

post_dep <- mutate(post_dep, rel_diff_8 = laeq_avg_11 - laeq_avg_8)
mean(post_dep$rel_diff_8)
sd(post_dep$rel_diff_8)
min(post_dep$rel_diff_8)
max(post_dep$rel_diff_8)



#prepare dataframes to be exported for plots 

#departures versus arrivals 

feb_pre_arr <- data.frame(pre_arr$date_sound, pre_arr$rel_diff_5, 
                              pre_arr$rel_diff_6,  pre_arr$rel_diff_7, 
                              pre_arr$rel_diff_8, pre_arr$Wind_Speed_2, 
                              pre_arr$bearing_FL, pre_arr$shield_angle,
                              pre_arr$ground_distance_FL, pre_arr$Ground_Speed_ms)

feb_pre_dep <- data.frame(pre_dep$date_sound, pre_dep$rel_diff_5, 
                              pre_dep$rel_diff_6,  pre_dep$rel_diff_7, 
                              pre_dep$rel_diff_8, pre_dep$Wind_Speed_2, 
                              pre_dep$bearing_FL, pre_dep$shield_angle,
                              pre_dep$ground_distance_FL, pre_dep$Ground_Speed_ms)

feb_post_arr <- data.frame(post_arr$date_sound, post_arr$rel_diff_5, 
                              post_arr$rel_diff_6,  post_arr$rel_diff_7, 
                              post_arr$rel_diff_8, post_arr$Wind_Speed_2, 
                              post_arr$bearing_FL, post_arr$shield_angle,
                              post_arr$ground_distance_FL, post_arr$Ground_Speed_ms)

feb_post_dep <- data.frame(post_dep$date_sound, post_dep$rel_diff_5, 
                              post_dep$rel_diff_6,  post_dep$rel_diff_7, 
                              post_dep$rel_diff_8, post_dep$Wind_Speed_2, 
                              post_dep$bearing_FL, post_dep$shield_angle,
                              post_dep$ground_distance_FL, post_dep$Ground_Speed_ms)

write.csv(feb_pre_arr, "data/working_copies/split_datasets/Feb_PreArr.csv", row.names=FALSE)
write.csv(feb_pre_dep, "data/working_copies/split_datasets/Feb_PreDep.csv", row.names=FALSE)
write.csv(feb_post_arr, "data/working_copies/split_datasets/Feb_PostArr.csv", row.names=FALSE)
write.csv(feb_post_dep, "data/working_copies/split_datasets/Feb_PostDep.csv", row.names=FALSE)


#T tests
#arrivals versus departures 
t.test(pre_arr$avg_exp, pre_arr$laeq_avg_11, paired = TRUE) 
t.test(pre_dep$avg_exp, pre_dep$laeq_avg_11, paired=TRUE)
t.test(post_arr$avg_exp, post_arr$laeq_avg_11, paired=TRUE)
t.test(post_dep$avg_exp, post_dep$laeq_avg_11, paired=TRUE)



#Linear Regression Analysis 
pre_arr$ACTYPE <- factor(pre_arr$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) + scale(Wind_Dir_deg_2) + scale(Temp_Out_2) + ACTYPE, data = pre_arr)
summary(model)
coefficients(model)
summary(model)$r.squared

#repeat process for the other dataframes 
pre_dep$ACTYPE <- factor(pre_dep$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) + scale(Wind_Dir_deg_2) + scale(Temp_Out_2) + ACTYPE, data = pre_dep)
summary(model)
coefficients(model)
summary(model)$r.squared

post_arr$ACTYPE <- factor(post_arr$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) + scale(Wind_Dir_deg_2) + scale(Temp_Out_2) + ACTYPE, data = post_arr)
summary(model)
coefficients(model)
summary(model)$r.squared

post_dep$ACTYPE <- factor(post_dep$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) + scale(Wind_Dir_deg_2) + scale(Temp_Out_2) + ACTYPE, data = post_dep)
summary(model)
coefficients(model)
summary(model)$r.squared
