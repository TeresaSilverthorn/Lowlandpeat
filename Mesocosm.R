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
library(lubridate)

#
#
#
# Set working directory for figures
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Figures")
#
#
# load in data
dat1 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/Report C110 INTERIM 20250320_TS.xlsx", range= "A10:AF94")  
#
head(dat1) #84 obs of 32 vars
#
#
dat2 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/Report C113 INTERIM 20250325_TS.xlsx", range= "A10:X258") 
#
head(dat2)
#
#
#
# Drop any empty rows
dat1 <- dat1 %>% select(-c(5:8))    # Drop columns 5 to 8, which are empty
dat2 <- dat2 %>% select(-c(9:12)) 
#
#
# For dat1 the 61BW to 84BW rows are for the WastedPeat project and can be removed
dat1 <- dat1 %>%
  filter(!grepl("^6[1-9]BW$|^7[0-9]BW$|^8[0-4]BW$", site_label))
#
#
#
# Data cleaning of values below detection limit
dat1 <- dat1 %>%
  mutate(across(3:28, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.)))))    # NAs introduced warning is normal
#
#
dat2 <- dat2 %>%   # note that SO4 has a lot of >> than symbols here, so over the detection limit, check with Mike about how to deal with these?
  mutate(across(7:20, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.)))))    # NAs introduced warning is normal

#
#
# Combine data files into one dataframe
dat <- bind_rows(dat1, dat2)
#
str(dat)  # 332 obs of 32 vars
#
#
#
#
#
#
################################################################################
#
# Load in the ancillary data file
ancil_dat1 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Sample collection and despatch.csv") #this is associated with C110
#Note from Mike: The BW samples/field sites are listed in the “sample collection and despatch” tab. We can ignore the samples that are only Wasted Peat Project. 
head(ancil_dat1)
#
#
ancil_dat2 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/BL-Week 1.csv")  # this is associated with C113
head(ancil_dat2)
#
#
ancil_dat3 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Cycle 1-W1.csv")  # this is associated with C113; issues with date when using excel format
head(ancil_dat3)
#
#
ancil_dat4 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Cycle 1-W2.csv")  # this is associated with C113; 
head(ancil_dat4)
#
#
# Subet just the LP3+ data
ancil_dat1 <- ancil_dat1 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
ancil_dat2 <- ancil_dat2 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
ancil_dat3 <- ancil_dat3 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
ancil_dat4 <- ancil_dat4 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
#
#
# Modify the individual df's so they align for binding
ancil_dat1$Tag <- as.numeric(ancil_dat1$Tag) # make column numeric in order to facilitate bind_rows
#
ancil_dat3$Week1.2 <- 1 # make column numeric to align with ancil_dat4 and facilitate bind_rows
#
ancil_dat3$Cycle.no. <- as.character(1)
#
ancil_dat4$Cycle.no. <- as.character(ancil_dat4$Cycle.no.) # make column character in order to facilitate bind_rows
#
#
# Merge the ancil dat df's together
ancil_dat <- bind_rows(ancil_dat1, ancil_dat2, ancil_dat3, ancil_dat4)
#
colnames(ancil_dat)[colnames(ancil_dat) == "Source.site.Location"] <- "source_site"
colnames(ancil_dat)[colnames(ancil_dat) == "Sample.name"] <- "site_label"
colnames(ancil_dat)[colnames(ancil_dat) == "Sample.type"] <- "sample_type"
colnames(ancil_dat)[colnames(ancil_dat) == "Sample.collected"] <- "sample_collected"
colnames(ancil_dat)[colnames(ancil_dat) == "Sample.type.code"] <- "sample_type_code"
#
#
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
  )) %>%
  relocate(site, .after = source_site)
#
#
# Get rid of spaces in site_label between number and BW
ancil_dat$site_label <- gsub(" (?=BW)", "", ancil_dat$site_label, perl = TRUE)
#
# Get rid of trailing spaces in ancil date site_label
ancil_dat$site_label <- trimws(ancil_dat$site_label)
#
#
# Add sites and other ancillary information to dat
dat <- dat %>% 
  left_join(ancil_dat %>% select(site, site_label, sample_collected, Group, sample_type, sample_type_code, Week.no., Cycle.no., Week1.2), by = "site_label")  
#
# Create a level for BW in the Cycle.no. column
dat <- dat %>% 
mutate(Cycle.no. = case_when(
  str_detect(site_label, "BW") & is.na(Cycle.no.) ~ "BW",
  TRUE ~ as.character(Cycle.no.) ))
#
# Make a new colum called C.W (cycle week) which combines the cycle and week columns
dat <- dat %>%
  mutate(C.W = if_else(is.na(Week1.2), as.character(Cycle.no.), paste(Cycle.no., Week1.2, sep = "_")))
#
# Reorder the factor levels
dat <- dat %>%
  mutate(C.W = factor(C.W, levels = c("BW", "BL", "1_1", "1_2")))
#
# Trim trailing zeroes in Group
dat$Group <- trimws(dat$Group)
dat$Group <- as.factor(dat$Group)
#
# Note sure why in the Baseline (BL) data the group is called Fully re-wetted, while in the other weeks, it's called rewetted... change to align but check with Mike!
levels(dat$Group)[levels(dat$Group) == "Fully re-wetted"] <- "Rewetted"
#
# Reorder columns to bring site closer to the start
dat <- dat %>% 
 select(1, 2, site, everything())
#
levels(as.factor(dat$site)) # make site a factor 
#
#
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/LP3+_mesocosm_dat_all.csv")
#
#################################################################################
#
#### TIME SERIES PLOTS ####
#

jpeg("Cl_timeseries_LP3+_mesocosm1.jpg", units="in", width=6.5, height=4, res=300)

Cl_time <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW"), aes(x = C.W, y = Cl_mg_l, colour = Group)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5)) +
  labs(x = "Cycle_week", y = "Cl") +
  theme_minimal()
Cl_time

dev.off()


jpeg("Cl_timeseries_LP3+_mesocosm2.jpg", units="in", width=6.5, height=4, res=300)

Cl_time <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW"), aes(x = C.W, y = Cl_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6) +
  labs(x = "Cycle_week", y = "Cl") +
  theme_minimal()
Cl_time

dev.off()




NO2_time <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW"), aes(x = C.W, y = NO2_mg_l, colour = Group)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5)) +
  labs(x = "Cycle_week", y = "NO2") +
  theme_minimal()
NO2_time


NO3_time <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW"), aes(x = C.W, y = NO3_mg_l, colour = Group)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5)) +
  labs(x = "Cycle_week", y = "NO3") +
  theme_minimal()
NO3_time





##############################################################################
#
#
#### PLOTS FOR BIN WATER ####
#
#
#
#### pH ####


#tiff("pH_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

pH <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y = pH, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs( y = "pH",  x = NULL ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
pH

#dev.off()

#### EC ####

#tiff("EC_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

EC <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y = EC_us_cm, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(  y = expression("Conductivity (" * mu * "S cm"^"-1" * ")"),  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
EC

#dev.off()


#### Fluoride ####

#tiff("Fluoride_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Fluo <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y = F_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + # Add boxplot without showing outliers
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("F- (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Fluo

#dev.off()


#### Chloride ####

#tiff("Chloride_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Cl <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Cl_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Cl- (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Cl

#dev.off()



#### Nitrite	#### 
#tiff("Nitrite_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NO2 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =NO2_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
NO2

#dev.off()


#### Nitrate ####

#tiff("Nitrate_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NO3 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =NO3_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) +
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
NO3

#dev.off()

#### Phosphorous ####


#tiff("Phosphorous_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

P <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =P_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(P ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  #scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
P

#dev.off()

#### Silicon ####

#tiff("Si_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Si <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Si_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(Si ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
 # scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Si

#dev.off()



#### Phosphate	####

#tiff("Phosphate_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

PO4 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =PO4_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(PO[4]^"3-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
PO4

#dev.off()

#### Sulfate	#### 

#tiff("Sulfate_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

SO4 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =SO4_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + # Add boxplot without showing outliers
 # geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(SO[4]^"2-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
SO4

#dev.off()

#### Lithium	####
# all NA 

#### Sodium	####

#tiff("Sodium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Na <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Na_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + # Add boxplot without showing outliers
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Na (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Na

#dev.off()


#### Ammonium	####


#tiff("Ammonium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NH4 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =NH4_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + # 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  #scale_y_log10() + 
  scale_y_continuous(trans = 'pseudo_log') +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
NH4

#dev.off()

#### Magnesium	#### 

#tiff("Magnesium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Mg <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Mg_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Mg (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Mg

#dev.off()


#### Potassium	#### 

#tiff("Potassium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

K <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =K_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("K (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
K

#dev.off()


#### Calcium	#### 

#tiff("Calcium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Ca <- ggplot(subset(dat, Cycle.no. == "BW"),aes(x = site, y =Ca_mg_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Ca (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Ca

#dev.off()


#### Aluminium	#####

#tiff("Aluminium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Al <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Al_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + #
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Al (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  #scale_y_continuous(trans = 'pseudo_log') +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Al

#dev.off()


#### As ####
# all NA 

#### Cd	####
# all NA 

#### Cr	#### 
# all NA 


#### Copper	####

#tiff("Copper_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Cu <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Cu_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Cu (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Cu

#dev.off()


#### Iron	#### 

#tiff("Iron_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Fe <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Fe_ug_l, fill=site)) +
  geom_boxplot(width = 0.9) + #
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Fe (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  #scale_y_continuous(trans = 'pseudo_log') +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Fe

#dev.off()


#### Manganese	####

#tiff("Manganese_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Mn <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Mn_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Mn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Mn

#dev.off()


#### Nickel	#### 

#tiff("Nickel_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Ni <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Ni_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
 # geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Ni (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Ni

#dev.off()


#### Pb ####
# all NA 


#### Zn	#### 
#tiff("Zinc_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Zn <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site, y =Zn_ug_l, fill=site)) + 
  geom_boxplot(width = 0.9) + 
 # geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Zn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=12), axis.text.y = element_text(size=12), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("RG-R8" = "#D8B4F8", "RG-PEF" = "#FDE68A", "WF-A" = "#F8C8DC",  "MM" = "#A2D2FF",  "TP-A" ="#B5E48C") )
Zn

#dev.off()

#### combine plots ####

jpeg("LP3+_mesocosm_BW_combined2.jpeg", units="in", width=11, height=13, res=200)

combine <- ggarrange(Al, Ca, Cl, Cu, EC, Fluo, Fe, K, 
                     Mg, Mn, Na, NH4, Ni, NO2, NO3, P, pH, 
                     PO4, Si, SO4, Zn, 
                     ncol = 4, nrow = 6, align="hv",common.legend = F) # labels = c("(a)", "(b)", "(c)")
combine

dev.off()

###############################################################################
#### test for differences between sites ####
#
#
# 1. Test for normality using shapiro wilks test. If the p-value is less than 0.05, you reject the null hypothesis, suggesting the data is not normally distributed.
shapiro_results <- sapply(dat[, c(4:11, 13:20, 24:27, 29)], shapiro.test)
# the majority are non-normal, so go with non-parametric tests
#
#
# 2. Use Kruskal-Wallis test to see differences between sites. If p<0.05 there is a significant difference between groups
#
#kruskal_results <- sapply(dat[, c(4:11, 13:18, 22:25, 27)], function(x) kruskal.test(x ~ dat$site))
#
kruskal_results <- dat %>%
  select(c(4:11, 13:20, 24:27, 29)) %>%
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
# Select numerical columns (excluding variables with Na lot of 0s
dat_num <- dat[, c(4:11, 13, 15:20, 24, 25)]

#dat_num <- dat[, c(4:7, 9:11, 13, 15:17, 22:23)] 


#
# previously when there were NAs, now updated with 0s: 
#
# There are just a couple NA values, fill with mean (2 in F_mg_l, 2 in Fe_ug>l, 1 SO4_mg_l, 1 Cu_mg_l)
dat_num$F_mg_l[is.na(dat_num$F_mg_l)] <- mean(dat_num$F_mg_l, na.rm = TRUE)
dat_num$Fe_ug_l[is.na(dat_num$Fe_ug_l)] <- mean(dat_num$Fe_ug_l, na.rm = TRUE)
dat_num$SO4_mg_l[is.na(dat_num$SO4_mg_l)] <- mean(dat_num$SO4_mg_l, na.rm = TRUE)
#dat_num$NH4_mg_l[is.na(dat_num$SO4_mg_l)] <- mean(dat_num$NH4_mg_l, na.rm = TRUE)
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
#
#
#################################################################################
#


