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

list <- list %>%


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


#### Check the atmospheric levels ####

dat_amb <- dat %>%
  filter(str_detect(sample_code, "AMB")) %>%
  mutate(  site = sub("D-([A-Za-z]{2})-.*", "\\1", sample_code))

mean_amb_by_gas <- dat_amb %>%
  group_by(gas) %>%
  summarize(mean_ppm = mean(amount, na.rm = TRUE))

#CO2       187    (recorded atmospheric levels: 425ppm)   
#CH4       2.07    (recorded atmospheric levels: 1.9 ppm)
#N2O       1.00    (recorded atmospheric levels:0.336 ppm)

#Strage, the CO2 and N2O seems off (in line with the weird results for standards...)

# ambient CO2

ggplot(dat_amb, aes(x = as.factor(site), y = amount)) + 
  geom_point(aes(colour=gas), alpha = 0.5, size = 3) +   scale_y_log10() 


#### check GHG concentration standards ####
#Standard concentrations for GHGs standards
#Standard ID	Concentration (ppm)	
#ID  N2O	  CH4	  CO2
#1	0.226	 1.21	  204.3
#2	0.357	 1.8	  436.5
#3	0.521	  5.2	  821
#4	1.0002	51.6	2016.4

dat_std <- dat %>%
  filter(str_detect(sample_code, "Std")) %>%
  group_by(sample_code, gas) %>%
  summarize(mean_amount = mean(amount, na.rm = TRUE))%>%
  pivot_wider(names_from = gas, values_from = mean_amount)
print(dat_std)
  
#CO2 seems off, as the reported standards are 13.8, 175.8, 394, 1064
#N2o is also a bit off


#### plots ####

#reorder sites for plotting
dat$site <- as.factor(dat$site)
dat$site <- factor(dat$site, levels = c("RG-R6", "RG-R8", "SS-A", "SS-B", "TP-A" , "TP-B", "LC", "SW" ))


#CO2

#tiff("LP3_CO2_plot.tiff", units="in", width=6.5, height=4, res=300)

CO2plot <- ggplot(subset(dat, gas=="CO2" & site!="NA"), aes(x = site, y = amount)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs( y = expression(CO[2] ~ "ppm"),  x = NULL, colour = "Ditch #") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
CO2plot
#Note the ~500 ppm value is from D-RG-R8-3.2. The 9000 ppm value is D-RG-R8-4.2. These could be outliers?

#dev.off()

#CH4

CH4plot <- ggplot(subset(dat, gas=="CH4" & site!="NA"), aes(x = site, y = amount)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  scale_y_log10() + 
  labs(y = expression(CH[4] ~ "ppm"), x = NULL, colour = "Ditch #") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
CH4plot

#Note: the ~2ppm values are D-RG-R8-3.2 and D-RG-R8-4.2

#N2O

N2Oplot <- ggplot(subset(dat, gas=="N2O" & site!="NA"), aes(x = site, y = amount)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  scale_y_log10() + 
  labs(y = expression(N[2]*O~ "ppm"),   x = NULL, colour = "Ditch #") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
N2Oplot

#Note: 1.5 ppm value is D-RG-R8-3.2


#### Calculate GHG fluxes ####

#Step 1: Calculate aquatic gas concentration (Caq)

# Caq =  headspace gas concentration in ppm (Chs) * solubility of gas in water mol/L/atm (KH) * atmospheric pressure at sampling site (Patm) + (Chs - gas concentration in the atmosphere (Cair) ) * Patm * volume of air in headspace (Va) / ( volume of water in headspace (Vw) * universal gas constant (R) * in situ water temperature)


#solubility coefficient of CO2 (Weiss, 1974; Table 2) and CH4 (Yamamoto et al., 1976)
mean(dat$water_temp_c, na.rm=T) #8.18
#solubility of CO2 at 8.18C is 0.05751 mol/L/atm
#solubility of CH4 at 8.18C is 0.04579 ml/ml -  convert to mol/L/atm??

