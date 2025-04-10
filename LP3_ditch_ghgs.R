#### R script to calculate ditch GHG fluxes from concentration data ####

# Note sample naming conventions: Letter representing the date, official LP3 site code, ditch point (1., 2. ,3., or 4.), and replicate 1 or 2 (or AMB for ambient air).  
# D  12/2024; E 01/2025; F 02/2025 
#
#
# Site file directory for figures 
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Figures")
#
## Load in necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
#
#Read in the GHG concentration data
#
dat1 <- readxl::read_xls("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Edinburgh GC data/LOWLAND_December_16_12_2024.XLS", range="A5:I213") # December 2024 East Anglia sites
#
dat2 <- readxl::read_xls("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Edinburgh GC data/LOWLAND_January_2025.XLS", range="A5:I126")   # December 2024 and January 2025 Wrights Farm 
#
dat3 <- readxl::read_xls("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Edinburgh GC data/LOWLAND_13_02_2025.XLS", range="A5:I222")   # February 2025 East Anglia sites
#
##
# Drop rows with NAs or repeated headings
#
dat1 <- dat1 %>%
  slice(-c(69, 70, 139, 140))
#
dat2 <- dat2 %>%
  slice(-c(40, 41, 81, 82))
#
dat3 <- dat3 %>%
  slice(-c(72, 73, 145, 146))
#
#
# Rename columns
colnames(dat1)[colnames(dat1) == "Sample Name"] <- "sample_code"
colnames(dat1)[colnames(dat1) == "Compound Name"] <- "gas"
colnames(dat1)[colnames(dat1) == "Amount"] <- "amount"
#
colnames(dat2)[colnames(dat2) == "Sample Name"] <- "sample_code"
colnames(dat2)[colnames(dat2) == "Compound Name"] <- "gas"
colnames(dat2)[colnames(dat2) == "Amount"] <- "amount"
#
colnames(dat3)[colnames(dat3) == "Sample Name"] <- "sample_code"
colnames(dat3)[colnames(dat3) == "Compound Name"] <- "gas"
colnames(dat3)[colnames(dat3) == "Amount"] <- "amount"
#
#
# Replace underscores with dashes
dat1 <- dat1 %>%
  mutate(sample_code = gsub("_", "-", sample_code))
#
dat2 <- dat2 %>%
  mutate(sample_code = gsub("_", "-", sample_code))
#
dat3 <- dat3 %>%
  mutate(sample_code = gsub("_", "-", sample_code))
#
#
#
## Read in the ancillary data
#
ancil_dat <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Ancil_dat/ditch_ancillary_data_2025-03-18.csv")
#
#Import the sample list to note which vials were wet
list <- readxl::read_xlsx("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Edinburgh GC data/Samples list.xlsx")
#
#
# Combine data and ancillary data
#
dat1 <- dat1 %>%
  left_join(ancil_dat, by = "sample_code") 
#
dat2 <- dat2 %>%
  left_join(ancil_dat, by = "sample_code") 
#
dat3 <- dat3 %>%
  left_join(ancil_dat, by = "sample_code") 
#
#
#Add a new column for site
dat1 <- dat1 %>%
  mutate(site = sub("D-([A-Za-z]+(?:-[A-Za-z0-9]+)*)(?:-[0-9.]+)?", "\\1", sample_code), #extract the site name
  site = if_else(grepl("Std|Dummy|unknown|AMB", site), NA_character_, site), 
  ditch = as.numeric(sub(".*-(\\d+)[.].*", "\\1", sample_code)))
#
dat2 <- dat2 %>%
  mutate(site = sub("[DJ]-([A-Za-z]+(?:-[A-Za-z0-9]+)*)(?:-[0-9.]+)?", "\\1", sample_code), #extract the site name
         site = if_else(grepl("Std|Dummy|unknown|AMB", site), NA_character_, site), 
         ditch = as.numeric(sub(".*-(\\d+)[.].*", "\\1", sample_code)))
#
dat3 <- dat3 %>%
  mutate(site = sub("F-([A-Za-z]+(?:-[A-Za-z0-9]+)*)(?:-[0-9.]+)?", "\\1", sample_code), #extract the site name
         site = if_else(grepl("Stnd|Dummy|unknown|AMB", site), NA_character_, site), 
         ditch = as.numeric(sub(".*-(\\d+)[.].*", "\\1", sample_code)))
#
#
# Change column formats
dat1$amount <- as.numeric(dat1$amount)
dat1$Area <- as.numeric(dat1$Area)
#
dat2$amount <- as.numeric(dat2$amount)
dat2$Area <- as.numeric(dat2$Area)
#
dat3$amount <- as.numeric(dat3$amount)
dat3$Area <- as.numeric(dat3$Area)
#
#
#
#### Calibration curves ####
#
#From Julia Drewer: Just to clarify, you don’t use the concentrations from the GC output file. The ones listed in there are based on outdated calibrations but it’s in the system, please ignore those. All you need from the output file is the peak area or peak height. You need to calculate your own concentrations using our standards and either the peak area or peak height based on our standard concentrations that I have sent you in the past. It should be a linear calibration curve for all 3 gases and you use an average of all sets of standards included in each run. If you don’t have the concentrations of our standards to hand, Mark and Aurelia can send you the table again.
#
#### cal1 is for East Anglia Dec '24
#### cal2 is for Wrights Dec '24 and Jan '25
#### cal3 is for East Anglia Feb '25
#
CO2cal1 <- dat1 %>% #get the average of the standards
  filter(str_detect(sample_code, "Std") & gas == "CO2") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))
#
CO2cal2 <- dat2 %>% #get the average of the standards
  filter(str_detect(sample_code, "Std") & gas == "CO2") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))
#
CO2cal3 <- dat3 %>% #get the average of the standards
  filter(str_detect(sample_code, "Stnd") & gas == "CO2") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))
#
#
#Standard concentrations for GHGs standards
#Standard ID	Concentration (ppm)	from Aurelia's email 19/12/2024
#ID  N2O	  CH4	  CO2
#1	0.226	 1.21	  204.3
#2	0.357	 1.8	  436.5
#3	0.521	  5.2	  821
#4	1.0002	51.6	2016.4
#
CO2cal1 <- CO2cal1 %>%
  mutate(CO2_ppm = case_when(
    sample_code == "Std1" ~ 204.3,  # Replace with actual concentrations
    sample_code == "Std2" ~ 436.5,
    sample_code == "Std3" ~ 821,
    sample_code == "Std4" ~ 2016.4,
    TRUE ~ NA_real_  # Ensures numeric output
  ))
#
CO2cal2 <- CO2cal2 %>%
  mutate(CO2_ppm = case_when(
    sample_code == "Std1" ~ 204.3,  # Replace with actual concentrations
    sample_code == "Std2" ~ 436.5,
    sample_code == "Std3" ~ 821,
    sample_code == "Std4" ~ 2016.4,
    TRUE ~ NA_real_  # Ensures numeric output
  ))
#
CO2cal3 <- CO2cal3 %>%
  mutate(CO2_ppm = case_when(
    sample_code == "Stnd1" ~ 204.3,  # Replace with actual concentrations
    sample_code == "Stnd2" ~ 436.5,
    sample_code == "Stnd3" ~ 821,
    sample_code == "Stnd4" ~ 2016.4,
    TRUE ~ NA_real_  # Ensures numeric output
  ))
#
#
#plot the calibration curve
CO2_cal_curve <- ggplot(CO2cal3, aes(x = CO2_ppm, y = Area)) + #replace CO2cal1 with relevatn dataset
  geom_point(size = 3, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression
  labs(x = "Known Concentration CO2 ppm", y = "Average Area",  title = "CO2 Calibration Curve") +
  theme_minimal()
CO2_cal_curve
#
#linear relationship
CO2_model1 <- lm(Area ~ CO2_ppm, data = CO2cal1)
summary(CO2_model1)  # slope: 1.42717 and intercept: 76.09342
#
CO2_model2 <- lm(Area ~ CO2_ppm, data = CO2cal2)
summary(CO2_model2)  # slope: 1.51805 and intercept: 84.63979   
#
CO2_model3 <- lm(Area ~ CO2_ppm, data = CO2cal3)
summary(CO2_model3)  # slope: 1.461428 and intercept: 23.102572     
#
#
#predict, y=mx+b solving for x=(y-b)/m
#
dat1 <- dat1 %>%
  mutate(CO2_ppm = ifelse(gas == "CO2", ((Area - 76.09342) / 1.42717), NA))
#
dat2 <- dat2 %>%
  mutate(CO2_ppm = ifelse(gas == "CO2", ((Area - 84.63979) / 1.51805), NA))
#
dat3 <- dat3 %>%
  mutate(CO2_ppm = ifelse(gas == "CO2", ((Area - 23.102572) / 1.461428), NA))
#
#
###############################################################################
#### CH4 calibration curves ####
#
CH4cal1 <- dat1 %>% # get the average of the standards
  filter(str_detect(sample_code, "Std") & gas == "CH4") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))
#
CH4cal2 <- dat2 %>% # get the average of the standards
  filter(str_detect(sample_code, "Std") & gas == "CH4") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))
#
CH4cal3 <- dat3 %>% # get the average of the standards
  filter(str_detect(sample_code, "Stnd") & gas == "CH4") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))
#
#Standard concentrations for GHGs standards
#Standard ID	Concentration (ppm)	from Aurelia's email 19/12/2024
#ID  N2O	  CH4	  CO2
#1	0.226	 1.21	  204.3
#2	0.357	 1.8	  436.5
#3	0.521	  5.2	  821
#4	1.0002	51.6	2016.4
#
CH4cal1 <- CH4cal1 %>%
  mutate(CH4_ppm = case_when(
    sample_code == "Std1" ~ 1.21,  # Replace with actual concentrations
    sample_code == "Std2" ~ 1.8,
    sample_code == "Std3" ~ 5.2,
    sample_code == "Std4" ~ 51.6,
    TRUE ~ NA_real_  # Ensures numeric output
  ))
#
CH4cal2 <- CH4cal2 %>%
  mutate(CH4_ppm = case_when(
    sample_code == "Std1" ~ 1.21,  # Replace with actual concentrations
    sample_code == "Std2" ~ 1.8,
    sample_code == "Std3" ~ 5.2,
    sample_code == "Std4" ~ 51.6,
    TRUE ~ NA_real_  # Ensures numeric output
  ))
#
CH4cal3 <- CH4cal3 %>%
  mutate(CH4_ppm = case_when(
    sample_code == "Stnd1" ~ 1.21,  # Replace with actual concentrations
    sample_code == "Stnd2" ~ 1.8,
    sample_code == "Stnd3" ~ 5.2,
    sample_code == "Stnd4" ~ 51.6,
    TRUE ~ NA_real_  # Ensures numeric output
  ))
#
#
#
#plot the calibration curve
#
CH4_cal_curve <- ggplot(CH4cal3, aes(x = CH4_ppm, y = Area)) +   # replace with relevant data
  geom_point(size = 3, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression
  labs( x = "Known Concentration CH4 ppm", y = "Average Area", title = "CH4 Calibration Curve") +
  theme_minimal()
CH4_cal_curve
#
#linear relationship
CH4_model1 <- lm(Area ~ CH4_ppm, data = CH4cal1)
summary(CH4_model1)  # slope: 1.797356 and intercept: 1.952527   
#
CH4_model2 <- lm(Area ~ CH4_ppm, data = CH4cal2)
summary(CH4_model2)  # slope: 1.84116 and intercept: 1.89674 
#
CH4_model3 <- lm(Area ~ CH4_ppm, data = CH4cal3)
summary(CH4_model3)  # slope: 1.8210768 and intercept: 1.8022239   
#
#
#
#predict, y=mx+b solving for x=(y-b)/m
dat1 <- dat1 %>%
  mutate(CH4_ppm = ifelse(gas == "CH4", ((Area-1.952527 )/1.797356), NA))
#
dat2 <- dat2 %>%
  mutate(CH4_ppm = ifelse(gas == "CH4", ((Area-1.89674 )/1.84116), NA))
#
dat3 <- dat3 %>%
  mutate(CH4_ppm = ifelse(gas == "CH4", ((Area-1.8022239 )/1.8210768), NA))
#
#
#
#
###############################################################################
#### N2O calibration curves ####
#
N2Ocal1 <- dat1 %>% # get the average of the standards
  filter(str_detect(sample_code, "Std") & gas == "N2O") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))
#
N2Ocal2 <- dat2 %>% # get the average of the standards
  filter(str_detect(sample_code, "Std") & gas == "N2O") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))
#
N2Ocal3 <- dat3 %>% # get the average of the standards
  filter(str_detect(sample_code, "Stnd") & gas == "N2O") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))
#
#Standard concentrations for GHGs standards
#Standard ID	Concentration (ppm)	from Aurelia's email 19/12/2024
#ID  N2O	  CH4	  CO2
#1	0.226	 1.21	  204.3
#2	0.357	 1.8	  436.5
#3	0.521	  5.2	  821
#4	1.0002	51.6	2016.4
#
N2Ocal1 <- N2Ocal1 %>%
  mutate(N2O_ppm = case_when(
    sample_code == "Std1" ~ 0.226,  # Replace with actual concentrations
    sample_code == "Std2" ~ 0.357,
    sample_code == "Std3" ~ 0.521,
    sample_code == "Std4" ~ 1.002,
    TRUE ~ NA_real_  # Ensures numeric output
  ))
#
N2Ocal2 <- N2Ocal2 %>%
  mutate(N2O_ppm = case_when(
    sample_code == "Std1" ~ 0.226,  # Replace with actual concentrations
    sample_code == "Std2" ~ 0.357,
    sample_code == "Std3" ~ 0.521,
    sample_code == "Std4" ~ 1.002,
    TRUE ~ NA_real_  # Ensures numeric output
  ))
#
N2Ocal3 <- N2Ocal3 %>%
  mutate(N2O_ppm = case_when(
    sample_code == "Stnd1" ~ 0.226,  # Replace with actual concentrations
    sample_code == "Stnd2" ~ 0.357,
    sample_code == "Stnd3" ~ 0.521,
    sample_code == "Stnd4" ~ 1.002,
    TRUE ~ NA_real_  # Ensures numeric output
  ))
#
#
#plot the calibration curve
#
N2O_cal_curve <- ggplot(N2Ocal3, aes(x = N2O_ppm, y = Area)) + # replace with relevant calibration dat
  geom_point(size = 3, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression
  labs( x = "Known Concentration N2O ppm", y = "Average Area", title = "N2O Calibration Curve") +
  theme_minimal()
N2O_cal_curve
#
#linear relationship
N2O_model1 <- lm(Area ~ N2O_ppm, data = N2Ocal1)
summary(N2O_model1)  # slope: 3069.64 and intercept: 244.54
#
N2O_model2 <- lm(Area ~ N2O_ppm, data = N2Ocal2)
summary(N2O_model2) # slope: 1523.40 and intercept: 111.51 
#
N2O_model3 <- lm(Area ~ N2O_ppm, data = N2Ocal3)
summary(N2O_model3) # slope: 3034.11 and intercept: 200.49       
#
#
#
#predict, y=mx+b solving for x=(y-b)/m
dat1 <- dat1 %>%
  mutate(N2O_ppm = ifelse(gas == "N2O", ((Area-244.54)/3069.64), NA))
#
dat2 <- dat2 %>%
  mutate(N2O_ppm = ifelse(gas == "N2O", ((Area-111.51)/1523.40), NA))
#
dat3 <- dat3 %>%
  mutate(N2O_ppm = ifelse(gas == "N2O", ((Area-200.49)/3034.11), NA))
#
#
#
#
#### Check the atmospheric levels ####
#
dat_amb1 <- dat1 %>%
  filter(str_detect(sample_code, "AMB")) %>%
  mutate(  site = sub("D-([A-Za-z]{2})-.*", "\\1", sample_code))
#
dat_amb2 <- dat2 %>%
  filter(str_detect(sample_code, "AMB")) %>%
  mutate(  site = sub("D-([A-Za-z]{2})-.*", "\\1", sample_code))
#
dat_amb3 <- dat3 %>%
  filter(str_detect(sample_code, "AMB")) %>%
  mutate(  site = sub("D-([A-Za-z]{2})-.*", "\\1", sample_code))
#
#
#
mean_amb_by_gas1 <- dat_amb1 %>%
  group_by(gas) %>%
  summarize(mean_CO2ppm = mean(CO2_ppm, na.rm = TRUE), 
            mean_CH4ppm = mean(CH4_ppm, na.rm = TRUE),
            mean_N2Oppm = mean(N2O_ppm, na.rm = TRUE))
#
print(mean_amb_by_gas1)
#
#CO2       475    (recorded atmospheric levels: 425ppm)   
#CH4       2.04    (recorded atmospheric levels: 1.9 ppm)
#N2O       0.385    (recorded atmospheric levels:0.336 ppm)
#
#These values make sense
#
#
mean_amb_by_gas2 <- dat_amb2 %>%
  group_by(gas) %>%
  summarize(mean_CO2ppm = mean(CO2_ppm, na.rm = TRUE), 
            mean_CH4ppm = mean(CH4_ppm, na.rm = TRUE),
            mean_N2Oppm = mean(N2O_ppm, na.rm = TRUE))
#
print(mean_amb_by_gas2)
#
#CO2       630    (recorded atmospheric levels: 425ppm)   # this seems a bit high, could be contamination by breathing
#CH4       2.08    (recorded atmospheric levels: 1.9 ppm)
#N2O       0.448    (recorded atmospheric levels:0.336 ppm)
#
#
mean_amb_by_gas3 <- dat_amb3 %>%
  group_by(gas) %>%
  summarize(mean_CO2ppm = mean(CO2_ppm, na.rm = TRUE), 
            mean_CH4ppm = mean(CH4_ppm, na.rm = TRUE),
            mean_N2Oppm = mean(N2O_ppm, na.rm = TRUE))
#
print(mean_amb_by_gas3)
#
#CO2       385    (recorded atmospheric levels: 425ppm)   
#CH4       1.92   (recorded atmospheric levels: 1.9 ppm)
#N2O       0.364    (recorded atmospheric levels:0.336 ppm)
# These values make sense
#
#
#
##################################################################################
#### save as file ####
#
# Select specific columns
subset_dat1 <- dat1 %>% select(gas, sample_code, date, time, water_temp_c, CO2_ppm, CH4_ppm, N2O_ppm)
subset_dat2 <- dat2 %>% select(gas, sample_code, date, time, water_temp_c, CO2_ppm, CH4_ppm, N2O_ppm)
subset_dat3 <- dat3 %>% select(gas, sample_code, date, time, water_temp_c, CO2_ppm, CH4_ppm, N2O_ppm)
#
## Combine the separate data frames
dat_full <- bind_rows(subset_dat1, subset_dat2, subset_dat3) 
str(dat_full) #534 obs of 8 vars
#
#
#set wd
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling")
#
#
# Save as CSV
write.csv(dat_full, "LP3_GHG_data_raw.csv", row.names = FALSE)
#
#reset wd
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Figures")
#
#
#### Use the excel sheet from Mike to calculate the dissolved gas concentration and read in that back in here ####
#
conc1 <- read_xlsx("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Flux calculations/Dissolved GHG calc sheet_2024_TS_EastAnglia_Dec2024.xlsx", sheet="output")
#
conc2 <- read_xlsx("", sheet="output")
#
conc3 <- read_xlsx("", sheet="output")

#subset useful columns from dat 

dat_sub <- dat %>%
  select(sample_code, date, time, water_temp_c, site, ditch) %>%
  filter(!grepl("Std|Dummy|unknown|AMB", sample_code, ignore.case = TRUE))  %>%  # remove standards and ambient
  distinct()  #delete duplicate rows
 

dat_c <- dat_discons %>%
  full_join(dat_sub, by = "sample_code") 



#### plots ####

#reorder sites for plotting
dat_c$site <- as.factor(dat_c$site)
dat_c$site <- factor(dat_c$site, levels = c("RG-R6", "RG-R8", "SS-A", "SS-B", "TP-A" , "TP-B", "LC", "SW" ))


#CO2

tiff("LP3_CO2_mg_l_plot.tiff", units="in", width=6.5, height=4, res=300)

CO2plot <- ggplot(dat_c, aes(x = site, y = CO2_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs( y = expression(CO[2] ~ "mg/l"),  x = NULL, colour = "Ditch #") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
CO2plot

#outliers:
#Note the 1095 ppm value is from D-RG-R8-3.2. The 16504 ppm value is D-RG-R8-4.2. These could be outliers?

dev.off()

#CH4
tiff("LP3_CH4_mg_l_plot.tiff", units="in", width=6.5, height=4, res=300)

CH4plot <- ggplot(dat_c, aes(x = site, y = CH4_ug_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  #scale_y_log10() + #doesn't work with negative values
  scale_y_continuous(trans = 'pseudo_log') +
  labs(y = expression(CH[4] ~ "ug/l"), x = NULL, colour = "Ditch #") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") )
CH4plot

dev.off()

#Note: the ~2ppm values are D-RG-R8-3.2 and D-RG-R8-4.2

#N2O
tiff("LP3_N2O_ug_l_plot.tiff", units="in", width=6.5, height=4, res=300)

N2Oplot <- ggplot(dat_c, aes(x = site, y = N2O_ug_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  scale_y_log10() + 
  labs(y = expression(N[2]*O~ "ug/l"),   x = NULL, colour = "Ditch #") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
N2Oplot

dev.off()

#Note: 0.57 ppm value is D-RG-R8-3.2


#### Calculate GHG fluxes ####

#Step 1: Calculate aquatic gas concentration (Caq)

# Caq =  headspace gas concentration in ppm (Chs) * solubility of gas in water mol/L/atm (KH) * atmospheric pressure at sampling site (Patm) + (Chs - gas concentration in the atmosphere (Cair) ) * Patm * volume of air in headspace (Va) / ( volume of water in headspace (Vw) * universal gas constant (R) * in situ water temperature)


#solubility coefficient of CO2 (Weiss, 1974; Table 2) and CH4 (Yamamoto et al., 1976)
mean(dat$water_temp_c, na.rm=T) #8.18
#solubility of CO2 at 8.18C is 0.05751 mol/L/atm
#solubility of CH4 at 8.18C is 0.04579 ml/ml -  convert to mol/L/atm??

