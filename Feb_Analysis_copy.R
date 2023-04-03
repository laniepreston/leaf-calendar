library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(purrr)

#dat <- fread("data/working_copies/Lanie_CasperFebDatacopy_CSV.csv")
dat <- fread("data/working_copies/FEB_MUNISENSE_CASPER_DAVIS_COMBINED_CY2_2023_02copy.csv")

#convert chr to timestamp
dat$date_sound <- as.POSIXct(dat$date_sound, format="%d/%m/%Y %H:%M")

#create dataframes for pre and post tree placement 
dat_pre <- dat %>% filter(date_sound < '2023-02-20')
dat_post <- dat %>% filter(date_sound >= '2023-02-20') 

#summary stats all mics 
dat_pre %>% 
  select(laeq_avg_11, laeq_avg_8, laeq_avg_7, laeq_avg_6, laeq_avg_5) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

dat_post %>% 
  select(laeq_avg_11, laeq_avg_8, laeq_avg_7, laeq_avg_6, laeq_avg_5) %>% 
  map_df(.f = ~ broom::tidy(summary(.x)), .id = "variable")

#TODO: filter outliers 
dat_pre <- subset(dat_pre, laeq_avg_5 > 30)

boxplot(dat$Wind_Speed_4)

#check for distributions 
ks.test(dat_pre$laeq_avg_11, dat_pre$laeq_avg_8)
ks.test(dat_pre$laeq_avg_11, dat_pre$laeq_avg_7)
ks.test(dat_pre$laeq_avg_11, dat_pre$laeq_avg_6)
ks.test(dat_pre$laeq_avg_11, dat_pre$laeq_avg_5)

#Mic 8 has MUCH bigger variance than the other mics 
#(in post set - 70 vs ±54, in pre set 102 vs ±60)

#pre - standard devation of the mics is bigger in the rooftop mic (11) rest around 7.5 
#post - stddev mic 8 much bigger than others
sd(dat_post$laeq_avg_11)
sd(dat_post$laeq_avg_5)
sd(dat_post$laeq_avg_6)
sd(dat_post$laeq_avg_7)
sd(dat_post$laeq_avg_8)


#paired t-test on same microphones before and after  - mutate new column with the difference and then do single t test on mutated column
dat_post %>% mutate(
  diff_11 <- c(dat_pre$laeq_avg_11, rep_len(dat_post$laeq_avg_11 - dat_pre$laeq_avg_11))
)

#z <- c(x, rep_len(x, length(y) - length(x)))


#2 sample t test with mic 11 and tree courtyard mics 
#look at boxplots to see how they compare - seems like data in general is lower post 
boxplot(dat_pre$laeq_avg_11, dat_pre$laeq_avg_5, dat_pre$laeq_avg_6, dat_pre$laeq_avg_7, dat_pre$laeq_avg_8)
boxplot(dat_post$laeq_avg_11, dat_post$laeq_avg_5, dat_post$laeq_avg_6, dat_post$laeq_avg_7, dat_post$laeq_avg_8)



