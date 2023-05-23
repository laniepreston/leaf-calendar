library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(car)
library(purrr)
library(stats)
library(ggplot2)

dat <- fread("data/working_copies/MAR_MUNISENSE_CASPER_DAVIS_COMBINED_2023_03copy.csv")

# DATA PREPROCESSING 
#convert chr to timestamp
dat$date_sound <- as.POSIXct(dat$date_sound, format="%d/%m/%Y %H:%M")

#reduce sound measurements to decibels and 5 characteristic frequencies  
dat <- dat %>% select(-contains(c('spectrum_z_16Hz', 'spectrum_z_20Hz', 'spectrum_z_10Hz', 
                                  "spectrum_z_50Hz", "spectrum_z_40Hz", "spectrum_z_63Hz",    
                                  "spectrum_z_80Hz", "spectrum_z_160Hz", "spectrum_z_200Hz", 
                                  "spectrum_z_400Hz", "spectrum_z_630Hz",   "spectrum_z_800Hz", 
                                  "spectrum_z_1250Hz",  "spectrum_z_1600Hz",  "spectrum_z_2500Hz",  
                                  "spectrum_z_3150Hz",  "spectrum_z_4000Hz", 
                                  "spectrum_z_5000Hz",  "spectrum_z_6300Hz",  "spectrum_z_8000Hz", 
                                  "spectrum_z_10000Hz", "spectrum_z_12500Hz", "spectrum_z_16000Hz",
                                  "spectrum_z_20000Hz")))

#filter out windspeeds of more than 12 m/s
dat <- dat %>% subset(!Wind_Speed_1 > 12 || !Wind_Speed_4 >12)

#only look at points when plane is closest to FL to focus on one flight at 
#a time 
dat <- dat %>% subset(closest_point == TRUE)

#remove extreme outliers 
dat <- dat %>% filter(laeq_11 < 90, laeq_5 < 90, laeq_6 < 90, laeq_7 < 90, laeq_8 < 90)
dat <- dat %>% filter(ground_distance_FL < 1000) 


#Split microphones by shielded or not shielded 
dat_split <- split(dat, dat$shielded)
shielded <-  dat_split$'TRUE'
unshielded <- dat_split$'FALSE'


# DATA ANALYSIS 
shielded %>% 
  select(laeq_11, laeq_8, laeq_7, laeq_6, laeq_5) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

unshielded %>% 
  select(laeq_11, laeq_8, laeq_7, laeq_6, laeq_5) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

#ks tests
shielded <- mutate(shielded, avg_exp = rowMeans(cbind(laeq_7,laeq_6, laeq_5, laeq_8)))
ks.test(shielded$avg_exp, shielded$laeq_11)

unshielded <- mutate(unshielded, avg_exp = rowMeans(cbind(laeq_7, laeq_6, laeq_5, laeq_8)))
ks.test(unshielded$avg_exp, unshielded$laeq_11)

#Finding overall avg relative difference for data table in thesis
shielded <- mutate(shielded, rel_diff_11 = laeq_11 - avg_exp)
mean(shielded$rel_diff_11)
sd(shielded$rel_diff_11)
min(shielded$rel_diff_11)
max(shielded$rel_diff_11)

unshielded <- mutate(unshielded, rel_diff_11 = laeq_11 - avg_exp)
mean(unshielded$rel_diff_11)
sd(unshielded$rel_diff_11)
min(unshielded$rel_diff_11)
max(unshielded$rel_diff_11)


#individual mic relative differences for data visualizations 

#Mic 5 
shielded <- mutate(shielded, rel_diff_5 = laeq_11 - laeq_5)
mean(shielded$rel_diff_5)
sd(shielded$rel_diff_5)
min(shielded$rel_diff_5)
max(shielded$rel_diff_5)

unshielded <- mutate(unshielded, rel_diff_5 = laeq_11 - laeq_5)
mean(unshielded$rel_diff_5)
sd(unshielded$rel_diff_5)
min(unshielded$rel_diff_5)
max(unshielded$rel_diff_5)

#Mic 6 
shielded <- mutate(shielded, rel_diff_6 = laeq_11 - laeq_6)
mean(shielded$rel_diff_6)
sd(shielded$rel_diff_6)
min(shielded$rel_diff_6)
max(shielded$rel_diff_6)

unshielded <- mutate(unshielded, rel_diff_6 = laeq_11 - laeq_6)
mean(unshielded$rel_diff_6)
sd(unshielded$rel_diff_6)
min(unshielded$rel_diff_6)
max(unshielded$rel_diff_6)

#Mic 7
shielded <- mutate(shielded, rel_diff_7 = laeq_11 - laeq_7)
mean(shielded$rel_diff_7)
sd(shielded$rel_diff_7)
min(shielded$rel_diff_7)
max(shielded$rel_diff_7)

unshielded <- mutate(unshielded, rel_diff_7 = laeq_11 - laeq_7)
mean(unshielded$rel_diff_7)
sd(unshielded$rel_diff_7)
min(unshielded$rel_diff_7)
max(unshielded$rel_diff_7)

#Mic 8
shielded <- mutate(shielded, rel_diff_8 = laeq_11 - laeq_8)
mean(shielded$rel_diff_8)
sd(shielded$rel_diff_8)
min(shielded$rel_diff_8)
max(shielded$rel_diff_8)

unshielded <- mutate(unshielded, rel_diff_8 = laeq_11 - laeq_8)
mean(unshielded$rel_diff_8)
sd(unshielded$rel_diff_8)
min(unshielded$rel_diff_8)
max(unshielded$rel_diff_8)

mar_shielded <- data.frame(shielded$date_sound, shielded$rel_diff_5, 
                           shielded$rel_diff_6, shielded$rel_diff_7, 
                           shielded$rel_diff_8, shielded$Wind_Speed_1, 
                           shielded$bearing_FL, shielded$shield_angle,
                           shielded$ground_distance_FL, shielded$Ground_Speed_ms)

mar_unshielded <- data.frame(unshielded$date_sound, unshielded$rel_diff_5, 
                             unshielded$rel_diff_6, unshielded$rel_diff_7, 
                             unshielded$rel_diff_8, unshielded$Wind_Speed_1, 
                             unshielded$bearing_FL, unshielded$shield_angle,
                             unshielded$ground_distance_FL, unshielded$Ground_Speed_ms)

#write the dataframes to be used later for data visualizations 
write.csv(mar_shielded, "data/working_copies/Mar_Shielded.csv", row.names=FALSE)
write.csv(mar_unshielded, "data/working_copies/Mar_Unshielded.csv", row.names=FALSE)


#K-S Tests Individual Mics 
ks.test(shielded$rel_diff_11, shielded$rel_diff_5)
ks.test(shielded$rel_diff_11, shielded$rel_diff_6)
ks.test(shielded$rel_diff_11, shielded$rel_diff_7)
ks.test(shielded$rel_diff_11, shielded$rel_diff_8)

ks.test(unshielded$rel_diff_11, unshielded$rel_diff_5)
ks.test(unshielded$rel_diff_11, unshielded$rel_diff_6)
ks.test(unshielded$rel_diff_11, unshielded$rel_diff_7)
ks.test(unshielded$rel_diff_11, unshielded$rel_diff_8)

#T-tests 
t.test(shielded$avg_exp, shielded$laeq_11, paired = TRUE)
t.test(unshielded$avg_exp, unshielded$laeq_11, paired=TRUE)

t.test(shielded$rel_diff_11, shielded$rel_diff_5)
t.test(shielded$rel_diff_11, shielded$rel_diff_6)
t.test(shielded$rel_diff_11, shielded$rel_diff_7)
t.test(shielded$rel_diff_11, shielded$rel_diff_8)

t.test(unshielded$rel_diff_11, unshielded$rel_diff_5)
t.test(unshielded$rel_diff_11, unshielded$rel_diff_6)
t.test(unshielded$rel_diff_11, unshielded$rel_diff_7)
t.test(unshielded$rel_diff_11, unshielded$rel_diff_8)


#check lower-frequency sounds to test if the trunks impact 
shielded <- mutate(shielded, avg_exp_100hz = rowMeans(cbind(spectrum_z_100Hz_8, spectrum_z_100Hz_7, spectrum_z_100Hz_6, spectrum_z_100Hz_5)))
unshielded <- mutate(unshielded, avg_exp_100hz = rowMeans(cbind(spectrum_z_100Hz_8, spectrum_z_100Hz_7, spectrum_z_100Hz_6, spectrum_z_100Hz_5)))

t.test(shielded$avg_exp_100hz,shielded$spectrum_z_100Hz_11)
t.test(unshielded$avg_exp_100hz,unshielded$spectrum_z_100Hz_11)

#check higher frequency
shielded <- mutate(shielded, avg_exp_1000hz = rowMeans(cbind(spectrum_z_1000Hz_8, spectrum_z_1000Hz_7, spectrum_z_1000Hz_6, spectrum_z_1000Hz_5)))
unshielded <- mutate(unshielded, avg_exp_1000hz = rowMeans(cbind(spectrum_z_1000Hz_8, spectrum_z_1000Hz_7, spectrum_z_1000Hz_6, spectrum_z_1000Hz_5)))

t.test(shielded$avg_exp_100hz,shielded$spectrum_z_1000Hz_11)
t.test(unshielded$avg_exp_100hz,unshielded$spectrum_z_1000Hz_11)

#ANOVA

#looking for factors that influence the relative difference
model <- aov(rel_diff_11 ~ slant_angle + ground_distance_FL + Wind_Speed_1, data = shielded)
anova_results <- Anova(model, type = 2)
summary(anova_results)

