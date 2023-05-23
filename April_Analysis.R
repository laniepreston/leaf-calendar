library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(car)
library(purrr)
library(stats)
library(ggplot2)

dat <- fread("data/working_copies/APR_MUNISENSE_CASPER_DAVIS_COMBINED_2023_04copy.csv")

#use KNMI data to replace the data lost during Davis Station downtime from 12th to 23rd
knmi_path <- "data/working_copies/etmgeg_240.txt"
knmi <- data <- read.table(knmi_path, sep = ",", header = FALSE, skip = 50)

#replace weird abbreviations with better names 
col_names = c("Station","Date", "Avg_Wind_Direction","Avg_Windspeed",
              "Daily_Avg_Windspeed", "Max_Windspeed","Time_Max_Windspeed",
              "Min_Windspeed", "Time_Min_Windspeed", "Max_Gust", "Time_Max_Gust",
              "Avg_Temp","Min_Temp","Time_Min_Temp","Max_Temp","Time_Max_Temp",
              "Min_Temp_10cm","Time_Min_Temp_10cm","Sun_Duration","Pct_Max_Sun",
              "Radiation","Precip_Duration","Precip_Amt","Max_Precip",
              "Time_Max_Precip","Avg_Sea_Pressure","Max_Sea_Pressure",
              "Time_Max_Sea_Pressure","Min_Sea_Pressure","Time_Min_Sea_Pressure",
              "Min_Visibility","Time_Min_Visibility","Max_Visibility",
              "Time_Max_Visibility","Avg_Cloud_Cover","Avg_Atmos_Humidity",
              "Max_Humidity", "Time_Max_Humidity","Min_Humidity",
              "Time_Min_Humidity","Evapotranspiration")
names(knmi) <- col_names

#convert dates 
knmi$Date <- as.POSIXct(as.character(knmi$Date), format = "%Y%m%d")
knmi <- knmi %>% filter(Date >= '2023-04-12' & Date < '2023-04-24') 

#hourly data was not available, so just have to filter out days that had any 
#rain or winds over 12 m/s -> just leaves the 14th - 18th as valid dates,
#12 & 13, 19-23 must be removed 


# DATA PREPROCESSING

#only look at points when plane is closest to FL to remove some variance
dat <- dat %>% subset(closest_point == TRUE)

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

#filter out dates after configuration change
dat <- dat %>% filter(dat$date_sound < '2023-04-25')

#filter out dates with inclement weather during davis station downtime
dat <- filter(dat, !(format(date_sound, "%Y%m%d") %in% c("20230412", "20230413", 
                                                         "20230419", "20230420",
                                                         "20230421", "20230422",
                                                         "20230423")))

#filter out windspeeds of more than 12 m/s
dat <- dat %>% subset(!Wind_Speed_1 > 12 || !Wind_Speed_4 >12)

#filter extreme outliers 
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

apr_shielded <- data.frame(shielded$date_sound, shielded$rel_diff_5, 
                           shielded$rel_diff_6, shielded$rel_diff_7, 
                           shielded$rel_diff_8, shielded$Wind_Speed_1, 
                           shielded$bearing_FL, shielded$shield_angle,
                           shielded$ground_distance_FL, shielded$Ground_Speed_ms)

apr_unshielded <- data.frame(unshielded$date_sound, unshielded$rel_diff_5, 
                             unshielded$rel_diff_6, unshielded$rel_diff_7, 
                             unshielded$rel_diff_8, unshielded$Wind_Speed_1, 
                             unshielded$bearing_FL, unshielded$shield_angle,
                             unshielded$ground_distance_FL, unshielded$Ground_Speed_ms)

#write the dataframes to be used later for data visualizations 
write.csv(apr_shielded, "data/working_copies/Apr_Shielded.csv", row.names=FALSE)
write.csv(apr_unshielded, "data/working_copies/Apr_Unshielded.csv", row.names=FALSE)

#T-tests 
t.test(shielded$avg_exp, shielded$laeq_11, paired = TRUE)
t.test(unshielded$avg_exp, unshielded$laeq_11, paired=TRUE)

#ANOVA
#looking for factors that influence the relative difference
model <- aov(rel_diff_11 ~ slant_angle + ground_distance_FL + Wind_Speed_1, data = shielded)
anova_results <- Anova(model, type = 2)
summary(anova_results)

