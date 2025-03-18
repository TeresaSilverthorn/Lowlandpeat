#### R script for water quality data for LP3+ mesocosm experiment ####
#
#
#
## Load in necessary packages
library(dplyr)
library(ggplot2)
library(readxl)
library(purrr)
library(factoextra)
library(ggpubr)
library(stringr)

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
# Data cleaning of values below detection limit
dat <- dat %>%
  mutate(across(3:26, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.))))) 
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
    grepl("PEF", source_site) ~ "RG-PEF",           # assume this must be R6
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
#
levels(as.factor(dat$site))
#
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/LP3+_mesocosm_BW_dat.csv")
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

pH <- ggplot(dat, aes(x = site, y = pH, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs( y = "pH",  x = NULL ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
pH

dev.off()

#### EC ####

tiff("EC_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

EC <- ggplot(dat, aes(x = site, y = EC_us_cm, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(  y = expression("Conductivity (" * mu * "S cm"^"-1" * ")"),  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
EC

dev.off()


#### Fluoride ####

tiff("Fluoride_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Fluo <- ggplot(dat, aes(x = site, y = F_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + # Add boxplot without showing outliers
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("F- (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Fluo

dev.off()


#### Chloride ####

tiff("Chloride_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Cl <- ggplot(dat, aes(x = site, y =Cl_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Cl- (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Cl

dev.off()



#### Nitrite	#### 
tiff("Nitrite_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NO2 <- ggplot(dat, aes(x = site, y =NO2_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
NO2

dev.off()


#### Nitrate ####

tiff("Nitrate_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NO3 <- ggplot(dat, aes(x = site, y =NO3_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) +
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
NO3

dev.off()

#### Phosphate	####

tiff("Phosphate_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

PO4 <- ggplot(dat, aes(x = site, y =PO4_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(PO[4]^"3-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
PO4

dev.off()

#### Sulfate	#### 

tiff("Sulfate_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

SO4 <- ggplot(dat, aes(x = site, y =SO4_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + # Add boxplot without showing outliers
 # geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(SO[4]^"2-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
SO4

dev.off()

#### Lithium	####
# all NA 

#### Sodium	####

tiff("Sodium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Na <- ggplot(dat, aes(x = site, y =Na_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + # Add boxplot without showing outliers
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Na (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Na

dev.off()


#### Ammonium	####


tiff("Ammonium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NH4 <- ggplot(dat, aes(x = site, y =NH4_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + # 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
NH4

dev.off()

#### Magnesium	#### 

tiff("Magnesium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Mg <- ggplot(dat, aes(x = site, y =Mg_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Mg (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Mg

dev.off()


#### Potassium	#### 

tiff("Potassium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

K <- ggplot(dat, aes(x = site, y =K_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("K (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
K

dev.off()


#### Calcium	#### 

tiff("Calcium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Ca <- ggplot(dat, aes(x = site, y =Ca_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Ca (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Ca

dev.off()


#### Aluminium	#####

tiff("Aluminium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Al <- ggplot(dat, aes(x = site, y =Al_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + #
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Al (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Al

dev.off()


#### As ####
# all NA 

#### Cd	####
# all NA 

#### Cr	#### 
# all NA 


#### Copper	####

tiff("Copper_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Cu <- ggplot(dat, aes(x = site, y =Cu_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Cu (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Cu

dev.off()


#### Iron	#### 

tiff("Iron_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Fe <- ggplot(dat, aes(x = site, y =Fe_ug_l, fill=site)) +
  geom_boxplot(width = 0.9) + #
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Fe (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Fe

dev.off()


#### Manganese	####

tiff("Manganese_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Mn <- ggplot(dat, aes(x = site, y =Mn_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Mn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Mn

dev.off()


#### Nickel	#### 

tiff("Nickel_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Ni <- ggplot(dat, aes(x = site, y =Ni_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
 # geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Ni (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Ni

dev.off()


#### Pb ####
# all NA 


#### Zn	#### 
tiff("Zinc_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Zn <- ggplot(dat, aes(x = site, y =Zn_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
 # geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Zn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=12), axis.text.y = element_text(size=12), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Zn

dev.off()

#### combine plots ####

jpeg("LP3+_mesocosm_BW_combined2.jpeg", units="in", width=12, height=12, res=200)

combine <- ggarrange(Al, Ca, Cl, Cu, EC, Fluo, Fe, K, 
                     Mg, Mn, Na, NH4, Ni, NO2, NO3, pH, 
                     PO4, SO4, Zn, 
                     ncol = 4, nrow = 5, align="hv",common.legend = F) # labels = c("(a)", "(b)", "(c)")
combine

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
pairwise_results[["EC_us_cm"]]
pairwise_results[["SO4_mg_l"]]
pairwise_results[["F_mg_l"]]
pairwise_results[["PO4_mg_l"]]
pairwise_results[["Al_ug_l"]]
#
#
#
################################################################################
##### Run PCA ####
#
# Select numerical columns (excluding variables with NAs)
dat_num <- dat[, c(4:7, 9:11, 13, 15:17, 22:23)]
#
# There are just a couple NA values, fill with mean (2 in F_mg_l, 2 in Fe_ug>l, 1 SO4_mg_l, 1 Cu_mg_l)
dat_num$F_mg_l[is.na(dat_num$F_mg_l)] <- mean(dat_num$F_mg_l, na.rm = TRUE)
dat_num$Fe_ug_l[is.na(dat_num$Fe_ug_l)] <- mean(dat_num$Fe_ug_l, na.rm = TRUE)
dat_num$SO4_mg_l[is.na(dat_num$SO4_mg_l)] <- mean(dat_num$SO4_mg_l, na.rm = TRUE)
dat_num$Cu_ug_l[is.na(dat_num$Cu_ug_l)] <- mean(dat_num$Cu_ug_l, na.rm = TRUE)
#
# Run PCA (standardizing the data)
pca_result <- prcomp(dat_num, center = TRUE, scale. = TRUE)
#
# View summary statistics
summary(pca_result)
#
# Scree plot showing variance explained by each PC
fviz_eig(pca_result, addlabels = TRUE, barfill = "steelblue", barcolor = "black") 
#
#
# Visualize the PCA with site as a grouping factor
#
my.col.var <- c("#4A90E2", "#A347F3", "#F4B400", "#66A035", "#E63978") #set colour palette

#
#
tiff("PCA_LP3+_mesocosm_BW.tiff", units="in", width=8, height=6, res=300)
PCA_fig <- fviz_pca_biplot(pca_result, 
                           col.ind = dat$site,
                           addEllipses = TRUE, label = "var",
                           pointsize=3,
                           alpha.ind=0.5,
                           mean.point=F,
                           palette=my.col.var,
                           col.var = "black", repel = TRUE,
                           legend.title = " ") + ggtitle(NULL) 
PCA_fig
dev.off()
#
# Interpretation: If sites cluster tightly in PCA space, it suggests they have similar water quality profiles. The further apart the clusters are, the more distinct the site differences.
#
# The longer the arrow for a variable, the more it influences the principal component. The direction of the arrow shows the correlation with the component, and the length indicates how much it "drives" that component.
#
# Variables that point in the same direction (or are aligned) are positively correlated with each other, while those in opposite directions are negatively correlated


