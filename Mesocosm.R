#### R script for water quality data for LP3+ mesocosm experiment ####
#
#
#
## Load in necessary packages
library(dplyr)
library(ggplot2)
library(readxl)
#
#
#
# Set working directory for figures
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Figures")
#
#
# load in data
dat <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/Report C110 INTERIM 20250220_TS.xlsx", range= "A10:AD94")
#
#
# Drop columns 5 to 8, which are emptry
dat <- dat %>% select(-c(5:8))
#
# Replace non numeric values with NAs (cases below detection limits or insufficient sample)
dat <- dat %>%
  mutate(across(3:26, ~as.numeric(.), .names = "{.col}")) #NAs introduced error is expected



