#### R script to calculate ditch GHG fluxsed from concentration data ####

# Note sample naming conventions: Letter representing the date, official LP3 site code, ditch point (1., 2. ,3., or 4.), and replicate 1 or 2 (or AMB for ambient air).  
# D  12/2024; E 01/2025; F 02/2025 


# Site file directory for figures 
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Figures")

## Load in necessary packages
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

#Read in the GHG concentration data

dat <- readxl::read_xls("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Edinburgh GC data/LOWLAND_December_16_12_2024.XLS", range="A5:I213")

# Drop rows with NAs or repeated headings

dat <- dat %>%
  slice(-c(69, 70, 139, 140))

#rename columns
colnames(dat)[colnames(dat) == "Sample Name"] <- "sample_code"
colnames(dat)[colnames(dat) == "Compound Name"] <- "gas"
colnames(dat)[colnames(dat) == "Amount"] <- "amount"


#Replace underscores with dashes
dat <- dat %>%
  mutate(sample_code = gsub("_", "-", sample_code))

#Read in the ancillary data

ancil_dat <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Ancil_dat/ditch_ancillary_data_2025-01-20.csv")


#Import the sample list to note which vials were wet
list <- readxl::read_xlsx("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Edinburgh GC data/Samples list.xlsx")


# Combine data and ancillary data

dat <- dat %>%
  left_join(ancil_dat, by = "sample_code") 

#Add a new column for site
dat <- dat %>%
  mutate(site = sub("D-([A-Za-z]+(?:-[A-Za-z0-9]+)*)(?:-[0-9.]+)?", "\\1", sample_code), #extract the site name
  site = if_else(grepl("Std|Dummy|unknown|AMB", site), NA_character_, site), 
  ditch = as.numeric(sub(".*-(\\d+)[.].*", "\\1", sample_code)))
  
#Change column formats

dat$amount <- as.numeric(dat$amount)
dat$Area <- as.numeric(dat$Area)


#### Calibration curves ####
#From Julia Drewer: Just to clarify, you don’t use the concentrations from the GC output file. The ones listed in there are based on outdated calibrations but it’s in the system, please ignore those. All you need from the output file is the peak area or peak height. You need to calculate your own concentrations using our standards and either the peak area or peak height based on our standard concentrations that I have sent you in the past. It should be a linear calibration curve for all 3 gases and you use an average of all sets of standards included in each run. If you don’t have the concentrations of our standards to hand, Mark and Aurelia can send you the table again.

#### CO2 calibration curve ####

CO2cal <- dat %>% #get the average of the standards
  filter(str_detect(sample_code, "Std") & gas == "CO2") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))

#Standard concentrations for GHGs standards
#Standard ID	Concentration (ppm)	from Aurelia's email 19/12/2024
#ID  N2O	  CH4	  CO2
#1	0.226	 1.21	  204.3
#2	0.357	 1.8	  436.5
#3	0.521	  5.2	  821
#4	1.0002	51.6	2016.4

CO2cal <- CO2cal %>%
  mutate(CO2_ppm = case_when(
    sample_code == "Std1" ~ 204.3,  # Replace with actual concentrations
    sample_code == "Std2" ~ 436.5,
    sample_code == "Std3" ~ 821,
    sample_code == "Std4" ~ 2016.4,
    TRUE ~ NA_real_  # Ensures numeric output
  ))

#plot the calibration curve
CO2_cal_curve <- ggplot(CO2cal, aes(x = Area, y = CO2_ppm)) +
  geom_point(size = 3, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression
  labs(x = "Average Area", y = "Known Concentration CO2 ppm", title = "CO2 Calibration Curve") +
  theme_minimal()
CO2_cal_curve

#linear relationship
CO2_model <- lm(CO2_ppm ~ Area, data = CO2cal)
summary(CO2_model)  # slope: 0.700463 and intercept: -53.023365

#predict
dat <- dat %>%
  mutate(CO2_ppm = ifelse(gas == "CO2", predict(CO2_model, newdata = dat), NA))




#### CH4 calibration curve ####

CH4cal <- dat %>% # get the average of the standards
  filter(str_detect(sample_code, "Std") & gas == "CH4") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))

#Standard concentrations for GHGs standards
#Standard ID	Concentration (ppm)	from Aurelia's email 19/12/2024
#ID  N2O	  CH4	  CO2
#1	0.226	 1.21	  204.3
#2	0.357	 1.8	  436.5
#3	0.521	  5.2	  821
#4	1.0002	51.6	2016.4

CH4cal <- CH4cal %>%
  mutate(CH4_ppm = case_when(
    sample_code == "Std1" ~ 1.21,  # Replace with actual concentrations
    sample_code == "Std2" ~ 1.8,
    sample_code == "Std3" ~ 5.2,
    sample_code == "Std4" ~ 51.6,
    TRUE ~ NA_real_  # Ensures numeric output
  ))

#plot the calibration curve

CH4_cal_curve <- ggplot(CH4cal, aes(x = Area, y = CH4_ppm)) +
  geom_point(size = 3, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression
  labs(x = "Average Area", y = "Known Concentration CH4 ppm", title = "CH4 Calibration Curve") +
  theme_minimal()
CH4_cal_curve

#linear relationship
CH4_model <- lm(CH4_ppm ~ Area, data = CH4cal)
summary(CH4_model)  # slope: 0.5563701 and intercept: -1.0862595

#predict
dat <- dat %>%
  mutate(CH4_ppm = ifelse(gas == "CH4", predict(CH4_model, newdata = dat), NA))



#### N2O calibration curve ####

N2Ocal <- dat %>% # get the average of the standards
  filter(str_detect(sample_code, "Std") & gas == "N2O") %>%
  mutate(sample_code = factor(sample_code), 
         Area = as.numeric(Area))  %>%
  group_by(sample_code) %>%
  summarize(Area = mean(Area, na.rm = TRUE))

#Standard concentrations for GHGs standards
#Standard ID	Concentration (ppm)	from Aurelia's email 19/12/2024
#ID  N2O	  CH4	  CO2
#1	0.226	 1.21	  204.3
#2	0.357	 1.8	  436.5
#3	0.521	  5.2	  821
#4	1.0002	51.6	2016.4

N2Ocal <- N2Ocal %>%
  mutate(N2O_ppm = case_when(
    sample_code == "Std1" ~ 0.226,  # Replace with actual concentrations
    sample_code == "Std2" ~ 0.357,
    sample_code == "Std3" ~ 0.521,
    sample_code == "Std4" ~ 1.002,
    TRUE ~ NA_real_  # Ensures numeric output
  ))

#plot the calibration curve

N2O_cal_curve <- ggplot(N2Ocal, aes(x = Area, y = N2O_ppm)) +
  geom_point(size = 3, color = "blue") +  # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression
  labs(x = "Average Area", y = "Known Concentration N2O ppm", title = "N2O Calibration Curve") +
  theme_minimal()
N2O_cal_curve

#linear relationship
N2O_model <- lm(N2O_ppm ~ Area, data = N2Ocal)
summary(N2O_model)  # slope: 3.253e-04 and intercept: -7.872e-02 

#predict
dat <- dat %>%
  mutate(N2O_ppm = ifelse(gas == "N2O", predict(N2O_model, newdata = dat), NA))




#### Check the atmospheric levels ####

dat_amb <- dat %>%
  filter(str_detect(sample_code, "AMB")) %>%
  mutate(  site = sub("D-([A-Za-z]{2})-.*", "\\1", sample_code))

mean_amb_by_gas <- dat_amb %>%
  group_by(gas) %>%
  summarize(mean_CO2ppm = mean(CO2_ppm, na.rm = TRUE), 
            mean_CH4ppm = mean(CH4_ppm, na.rm = TRUE),
            mean_N2Oppm = mean(N2O_ppm, na.rm = TRUE))

print(mean_amb_by_gas)

#CO2       475    (recorded atmospheric levels: 425ppm)   
#CH4       2.04    (recorded atmospheric levels: 1.9 ppm)
#N2O       0.385    (recorded atmospheric levels:0.336 ppm)

#These values make sense


#### plots ####

#reorder sites for plotting
dat$site <- as.factor(dat$site)
dat$site <- factor(dat$site, levels = c("RG-R6", "RG-R8", "SS-A", "SS-B", "TP-A" , "TP-B", "LC", "SW" ))


#CO2

tiff("LP3_CO2_plot.tiff", units="in", width=6.5, height=4, res=300)

CO2plot <- ggplot(subset(dat, gas=="CO2" & site!="NA"), aes(x = site, y = CO2_ppm)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs( y = expression(CO[2] ~ "ppm"),  x = NULL, colour = "Ditch #") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
CO2plot
#Note the 1095 ppm value is from D-RG-R8-3.2. The 16504 ppm value is D-RG-R8-4.2. These could be outliers?

dev.off()

#CH4
tiff("LP3_CH4_plot.tiff", units="in", width=6.5, height=4, res=300)

CH4plot <- ggplot(subset(dat, gas=="CH4" & site!="NA"), aes(x = site, y = CH4_ppm)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  scale_y_log10() + 
  labs(y = expression(CH[4] ~ "ppm"), x = NULL, colour = "Ditch #") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
CH4plot

dev.off()

#Note: the ~2ppm values are D-RG-R8-3.2 and D-RG-R8-4.2

#N2O
tiff("LP3_N2O_plot.tiff", units="in", width=6.5, height=4, res=300)

N2Oplot <- ggplot(subset(dat, gas=="N2O" & site!="NA"), aes(x = site, y = N2O_ppm)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  scale_y_log10() + 
  labs(y = expression(N[2]*O~ "ppm"),   x = NULL, colour = "Ditch #") +
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

