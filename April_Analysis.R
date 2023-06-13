library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(car)
library(purrr)
library(stats)
library(ggplot2)

dat <- fread("data/working_copies/AprilMUNISENSE_CASPER_DAVIS_COMBINED_2023_04copy.csv")

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

#filter out days that had any rain or winds over 12 m/s -> just leaves the 
#14th - 18th as valid dates,12 & 13, 19-23 must be removed 

# DATA PREPROCESSING

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
dat <- dat %>% subset(!Wind_Speed_2 > 12 || !Wind_Speed_4 >12)

#filter extreme outliers 
dat <- dat %>% filter(ground_distance_FL < 1000) 

#export dat for later analysis
write.csv(dat, "data/working_copies/filtered/April_Filtered.csv", row.names=FALSE)


#split microphones by arriving vs. departing 
dat_split <- split(dat, dat$OP)
dat_arr <- dat_split$A
dat_dep <- dat_split$D


#graph some variables to see sources of variation
#wind direction 2, Bearing_FL, slant_angle, OP
selected_vars <- dat_dep[, c("bearing_FL", "slant_angle", "Wind_Dir_deg_2")]

# Create histogram with the selected variables
hist_dat <- stack(selected_vars)
hist(hist_dat$values, col = c("dodgerblue3", "firebrick1", "lightskyblue3"), border = "white",
     xlab = "Value", ylab = "Frequency", main = "Varation of Selected Variables")
legend("topright", legend = names(selected_vars), fill = c("dodgerblue3", "firebrick1", "lightskyblue3"))


# DATA ANALYSIS 
dat_arr <- mutate(dat_arr, avg_exp = rowMeans(cbind(laeq_7,laeq_6, laeq_5, laeq_8)))
dat_dep <- mutate(dat_dep, avg_exp = rowMeans(cbind(laeq_7, laeq_6, laeq_5, laeq_8)))

#Finding overall avg relative difference for data table in thesis
dat_arr <- mutate(dat_arr, rel_diff_11 = laeq_11 - avg_exp)
mean(dat_arr$rel_diff_11)
sd(dat_arr$rel_diff_11)
min(dat_arr$rel_diff_11)
max(dat_arr$rel_diff_11)

dat_dep <- mutate(dat_dep, rel_diff_11 = laeq_11 - avg_exp)
mean(dat_dep$rel_diff_11)
sd(dat_dep$rel_diff_11)
min(dat_dep$rel_diff_11)
max(dat_dep$rel_diff_11)


#individual mic relative differences for data visualizations 

#Mic 5 
dat_arr<- mutate(dat_arr, rel_diff_5 = laeq_11 - laeq_5)
mean(dat_arr$rel_diff_5)
sd(dat_arr$rel_diff_5)
min(dat_arr$rel_diff_5)
max(dat_arr$rel_diff_5)

dat_dep <- mutate(dat_dep, rel_diff_5 = laeq_11 - laeq_5)
mean(dat_dep$rel_diff_5)
sd(dat_dep$rel_diff_5)
min(dat_dep$rel_diff_5)
max(dat_dep$rel_diff_5)

#Mic 6 
dat_arr <- mutate(dat_arr, rel_diff_6 = laeq_11 - laeq_6)
mean(dat_arr$rel_diff_6)
sd(dat_arr$rel_diff_6)
min(dat_arr$rel_diff_6)
max(dat_arr$rel_diff_6)

dat_dep <- mutate(dat_dep, rel_diff_6 = laeq_11 - laeq_6)
mean(dat_dep$rel_diff_6)
sd(dat_dep$rel_diff_6)
min(dat_dep$rel_diff_6)
max(dat_dep$rel_diff_6)

#Mic 7
dat_arr <- mutate(dat_arr, rel_diff_7 = laeq_11 - laeq_7)
mean(dat_arr$rel_diff_7)
sd(dat_arr$rel_diff_7)
min(dat_arr$rel_diff_7)
max(dat_arr$rel_diff_7)

dat_dep <- mutate(dat_dep, rel_diff_7 = laeq_11 - laeq_7)
mean(dat_dep$rel_diff_7)
sd(dat_dep$rel_diff_7)
min(dat_dep$rel_diff_7)
max(dat_dep$rel_diff_7)

#Mic 8
dat_arr <- mutate(dat_arr, rel_diff_8 = laeq_11 - laeq_8)
mean(dat_arr$rel_diff_8)
sd(dat_arr$rel_diff_8)
min(dat_arr$rel_diff_8)
max(dat_arr$rel_diff_8)

dat_dep <- mutate(dat_dep, rel_diff_8 = laeq_11 - laeq_8)
mean(dat_dep$rel_diff_8)
sd(dat_dep$rel_diff_8)
min(dat_dep$rel_diff_8)
max(dat_dep$rel_diff_8)


apr_arr <- data.frame(dat_arr$date_sound, dat_arr$rel_diff_5, 
                      dat_arr$rel_diff_6, dat_arr$rel_diff_7, 
                      dat_arr$rel_diff_8, dat_arr$Wind_Speed_2, 
                      dat_arr$bearing_FL, dat_arr$shield_angle,
                      dat_arr$ground_distance_FL, dat_arr$Ground_Speed_ms)

apr_dep <- data.frame(dat_dep$date_sound, dat_dep$rel_diff_5, 
                      dat_dep$rel_diff_6, dat_dep$rel_diff_7, 
                      dat_dep$rel_diff_8, dat_dep$Wind_Speed_2, 
                      dat_dep$bearing_FL, dat_dep$shield_angle,
                      dat_dep$ground_distance_FL, dat_dep$Ground_Speed_ms)


#write the dataframes to be used later for data visualizations 
write.csv(apr_arr, "data/working_copies/split_datasets/Apr_Arr.csv", row.names=FALSE)
write.csv(apr_dep, "data/working_copies/split_datasets/Apr_Dep.csv", row.names=FALSE)

#T-tests 
t.test(dat_arr$avg_exp, dat_arr$laeq_11, paired = TRUE)
t.test(dat_dep$avg_exp, dat_dep$laeq_11, paired=TRUE)

#Linear Regression Analysis 
#examining significance of other variables 
dat_arr$ACTYPE <- factor(dat_arr$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) + scale(Wind_Dir_deg_2) + scale(Temp_Out_2) + ACTYPE, data = dat_arr)
summary(model)
coefficients(model)
summary(model)$r.squared

#repeat process for the other dataframe
dat_dep$ACTYPE <- factor(dat_dep$ACTYPE)
model <- lm(scale(rel_diff_11) ~ scale(bearing_FL) + scale(slant_angle) + scale(shield_angle) + scale(Wind_Dir_deg_2) + scale(Temp_Out_2) + ACTYPE, data = dat_dep)
summary(model)
coefficients(model)
summary(model)$r.squared

