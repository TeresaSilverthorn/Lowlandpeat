#### R script for water quality data for LP3+ mesocosm experiment ####
#
#
#
## Load in necessary packages
library(dplyr)
library(ggplot2)
library(readxl)
library(purrr)
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
#
#
#
# Load in the ancillary data file
ancil_dat <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Mesocosm sample log.xlsx", sheet= "Sample collection and despatch") 
#Note from Mike: The BW samples/field sites are listed in the “sample collection and despatch” tab. We can ignore the samples that are only Wasted Peat Project. 
#
#
head(ancil_dat)
#
colnames(ancil_dat)[colnames(ancil_dat) == "Source/site Location"] <- "source_site"
colnames(ancil_dat)[colnames(ancil_dat) == "Sample name"] <- "site_label"
#
#
ancil_dat <- ancil_dat %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
#
#
#make a new column for site
ancil_dat <- ancil_dat %>% 
  mutate(site = case_when(
    grepl("Pymoor", source_site) ~ "TP-A",
    grepl("Manchester", source_site) ~ "MM",
    grepl("R8", source_site) ~ "RG-R8",
    grepl("PEF", source_site) ~ "RG-R6",           # assume this must be R6
    grepl("Wrights", source_site) ~ "WF-A",             #double check this is the right field
    TRUE ~ NA_character_  # Keep other values as NA
  ))
#
#
# Get rid of spaces in site_label
ancil_dat$site_label <- gsub(" ", "", ancil_dat$site_label)
#
#
#
# Add sites to dat
dat <- dat %>% 
  left_join(ancil_dat %>% select(site_label, site), by = "site_label")
#
# Drop the rows where site is NA (those are from the Wasted Peat project)
dat <- dat %>% 
  filter(!is.na(site))
#
# Reorder columns to bring site closer to the start
dat <- dat %>% 
  select(1, 2, site, everything())
#
#
##############################################################################
#
#
#### PLOT ####
#
#
#
#### pH ####


tiff("pH_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

pH <- ggplot(dat, aes(x = site, y = pH)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs( y = "pH",  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
pH

dev.off()

#### EC ####

tiff("EC_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

EC <- ggplot(dat, aes(x = site, y = EC_us_cm)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(  y = expression("Conductivity (" * mu * "S cm"^"-1" * ")"),  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
EC

dev.off()


#### Fluoride ####

tiff("Fluoride_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Fluo <- ggplot(dat, aes(x = site, y = F_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("F- (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )
Fluo

dev.off()


#### Chloride ####

tiff("Cl_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Cl <- ggplot(dat, aes(x = site, y =Cl_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Cl- (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Cl

dev.off()



#### Nitrite	#### 
tiff("NO2_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NO2 <- ggplot(dat, aes(x = site, y =NO2_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
NO2

dev.off()


#### Nitrate ####

tiff("NO3_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NO3 <- ggplot(dat, aes(x = site, y =NO3_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
NO3

dev.off()

#### Phosphate	####

tiff("PO4_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

PO4 <- ggplot(dat, aes(x = site, y =PO4_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(PO[4]^"3-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
PO4

dev.off()

#### Sulfate	#### 

tiff("SO4_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

SO4 <- ggplot(dat, aes(x = site, y =SO4_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(SO[4]^"2-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
SO4

dev.off()

#### Lithium	####
# all NA 

#### Sodium	####

tiff("Na_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Na <- ggplot(dat, aes(x = site, y =Na_mg_l )) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Na (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Na

dev.off()


#### Ammonium	####

tiff("NH4_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NH4 <- ggplot(dat, aes(x = site, y =SO4_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
NH4

dev.off()

#### Magnesium	#### 

tiff("Mg_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Mg <- ggplot(dat, aes(x = site, y =Mg_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Mg (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Mg

dev.off()


#### Potassium	#### 

tiff("K_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

K <- ggplot(dat, aes(x = site, y =K_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("K (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
K

dev.off()


#### Calcium	#### 

tiff("Ca_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Ca <- ggplot(dat, aes(x = site, y =Ca_mg_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Ca (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Ca

dev.off()


#### Al	#####

tiff("Al_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Al <- ggplot(dat, aes(x = site, y =Al_ug_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Al (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Al

dev.off()


#### As ####
# all NA 

#### Cd	####
# all NA 

#### Cr	#### 
# all NA 


#### Cu	####

tiff("Cu_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Cu <- ggplot(dat, aes(x = site, y =Cu_ug_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Cu (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Cu

dev.off()


#### Fe	#### 

tiff("Fe_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Fe <- ggplot(dat, aes(x = site, y =Fe_ug_l)) +
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Fe (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Fe

dev.off()


#### Mn	####

tiff("Mn_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Mn <- ggplot(dat, aes(x = site, y =Mn_ug_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Mn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Mn

dev.off()


#### Ni	#### 

tiff("Ni_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Ni <- ggplot(dat, aes(x = site, y =Ni_ug_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Ni (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Ni

dev.off()


#### Pb ####
# all NA 


#### Zn	#### 
tiff("Zn_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Zn <- ggplot(dat, aes(x = site, y =Zn_ug_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Zn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    )
Zn

dev.off()

###############################################################################
#### test for differences between sites ####
#
#
# 1. Test for normality using shapiro wilks test. If the p-value is less than 0.05, you reject the null hypothesis, suggesting the data is not normally distributed.
shapiro_results <- sapply(dat[, c(4:11, 13:18, 22:25, 27)], shapiro.test)
# the majority are non-normal, so go with non-parametric tests
#
#
# 2. Use Kruskal-Wallis test to see differences between sites. If p<0.05 there is a significant difference between groups
#
#kruskal_results <- sapply(dat[, c(4:11, 13:18, 22:25, 27)], function(x) kruskal.test(x ~ dat$site))
#
kruskal_results <- dat %>%
  select(c(4:11, 13:18, 22:25, 27)) %>%
  map(~ kruskal.test(.x ~ dat$site))
#
kruskal_p_values <- kruskal_results %>%
  map_dbl(~ .x$p.value)
#
# Significant differences for: pH, EC, F, Cl, PO4, SO4, Na, NH4, Mg, K, Ca, Al, Cu, Fe, Mn
#
#
# 3. Run pairwise Wilcoxon rank sum test
#
# Identify significant variables
significant_vars <- names(kruskal_p_values)[kruskal_p_values < 0.05]

# Run pairwise Wilcoxon tests on significant variables
pairwise_results <- dat %>%
  select(all_of(significant_vars)) %>%
  map(~ pairwise.wilcox.test(.x, dat$site, p.adjust.method = "bonferroni", exact = FALSE))
#
# print each result by changing the element name within the brackets
pairwise_results[["pH"]]
