library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(car)
library(purrr)
library(stats)



dat_feb <- fread("data/working_copies/Filtered/Feb_Filtered.csv") 
dat_march <- fread("data/working_copies/Filtered/March_Filtered.csv")
dat_april <- fread("data/working_copies/Filtered/April_Filtered.csv")



#Examining extra variables for Feb

#wind direction 2, Bearing_FL, slant_angle, OP
selected_vars <- dat_feb[, c( "bearing_FL", "slant_angle", "Wind_Dir_deg_2", "Out_Hum_2")]

# Create histogram with the selected variables
hist_dat <- stack(selected_vars)
hist(hist_dat$values, col = c("dodgerblue3", "firebrick1", "lightskyblue3", "firebrick4"), border = "white",
     xlab = "Value", ylab = "Frequency", main = "Varation of Selected Variables")
legend("topright", legend = names(selected_vars), fill = c("dodgerblue3", "firebrick1", "lightskyblue3", "firebrick4"))

#very little temperature variation - temperature on a separate histogram to see variation better
hist(dat_feb$Temp_Out_2, xlab="Temperature (C)", ylab="Frequency", col="dodgerblue3", main="Temperature Variation in February")


#Examining extra variables for March
selected_vars <- dat_march[, c( "bearing_FL", "slant_angle", "Wind_Dir_deg_2")]
hist_dat <- stack(selected_vars)
hist(hist_dat$values, col = c("dodgerblue3", "firebrick1", "lightskyblue3"), border = "white",
     xlab = "Value", ylab = "Frequency", main = "Varation of Selected Variables")
legend("topright", legend = names(selected_vars), fill = c("dodgerblue3", "firebrick1", "lightskyblue3", "firebrick4"))

hist(dat_march$Temp_Out_2, xlab="Temperature (C)", ylab="Frequency", col="dodgerblue3", main="Temperature Variation in March")

#Examining extra variables for April
selected_vars <- dat_april[, c( "bearing_FL", "slant_angle", "Wind_Dir_deg_2")]
hist_dat <- stack(selected_vars)
hist(hist_dat$values, col = c("dodgerblue3", "firebrick1", "lightskyblue3"), border = "white",
     xlab = "Value", ylab = "Frequency", main = "Varation of Selected Variables")
legend("topright", legend = names(selected_vars), fill = c("dodgerblue3", "firebrick1", "lightskyblue3", "firebrick4"))

hist(dat_april$Temp_Out_2, xlab="Temperature (C)", ylab="Frequency", col="dodgerblue3", main="Temperature Variation in April")




