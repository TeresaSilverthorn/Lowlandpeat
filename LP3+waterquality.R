###### LP3 water quality data ###########
#
# load necessary packages
library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(ggpubr)
library(cowplot)
library(scales)
library(ggrepel)
#
#
#
#
# Set working directory for figures
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Figures")
#
#
# Check how many samples have been analysed
sample_list <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Ancil dat/Sites to sample_TS.xlsx", sheet="Data")
#
sum(!is.na(sample_list$`Analysed?`))# count how many have been analysed
(166/324)*100
#
# load in data
#
dat1 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C111 20250311_TS.xlsx", range= "A10:AJ54")
#
head(dat1)  
#
dat2 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C112 20250325_TS.xlsx", range= "A10:AJ48")   # I am not sure about the dates on this one, need to double check
#
head(dat2)
#
dat3 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Copy of Report C102 20250306_TS_MP.xlsx", range= "A10:AK54")
#
head(dat3)
#
#
dat4 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C114 20250331_TS.xlsx", range= "A10:AK45")
#
head(dat4)
#
#
dat5 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C105 INTERIM 20250410_TS.xlsx", range= "A10:AJ97")
#
head(dat5) #remove the * in P (over range, estimated)
# NOTE : can classiffy leighton moss as semi-natural fen (originally  fen/reed bed); can classify win carbon farm as rewetted extraction (oroginally paludiculture). There is an "Adnesy South?" value that needs to be investigated
#
#
dat6 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C103 INTERIM 20250410_TS.xlsx", range= "A10:AK15")
#
head(dat6)
#
#
#replace non numeric values (with <) with zeros and NA for cases below detection limits or insufficient sample)
#
dat1 <- dat1 %>%
  mutate(across(7:36, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.))))) 
#
dat2 <- dat2 %>%
  mutate(across(7:36, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.))))) 
#
#
dat3 <- dat3 %>%
  mutate(across(8:37, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.))))) 
#
#
dat4 <- dat4 %>%
  mutate(across(8:37, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.))))) 
#
#
dat5 <- dat5 %>%
  mutate(across(7:36, ~ str_remove_all(., "\\*"))) %>%    # remove the * after the out of range value
  mutate(across(7:36, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.)))))  %>%  
 filter(sample_code != "C105-47")      # remove row with missing value
#
#
dat6 <- dat6 %>%
  mutate(across(8:37, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.))))) 
#
#
#
#You will get an NA's introduced by coercion error, this is fine as any cells with i.s., or nd will be replaced with NA
#
#
# Remove Laura's sites
#
dat1 <- dat1 %>% filter(!grepl("ByC 4 July", site_label))
dat1 <- dat1 %>% filter(!grepl("FE 5 July", site_label))
dat1 <- dat1 %>% filter(!grepl("Darwen 1", site_label))  
dat1 <- dat1 %>% filter(!grepl("Darwen 2", site_label)) 
#
dat3 <- dat3 %>% filter(!grepl("delete", notes)) 
#
#
#
#combine
#
dat <- bind_rows(dat1, dat2, dat3, dat4, dat5, dat6)
#
str(dat) #248 obs of 43 vars
#
#
#
#
dat$date<-as.POSIXct(dat$date, format="%Y-%m-%d", tz = "GMT")
#
dat <- dat %>%
  mutate(month = factor(format(date, "%b"), levels = month.abb))
#
#
dat$land_use <- as.factor(dat$land_use)
#

#
dat <- dat %>%
mutate(month = floor_date(date, unit = "month"))
#
#
# As per latest update, amalgate Grassland and Grassland/raised WTs land use
levels(dat$land_use)[levels(dat$land_use) == "Grassland/raised WTs"] <- "Grassland"
#
# To better align with the other broader categories, make leighton moss as semi-natural fen (originally  fen/reed bed); can classify win carbon farm as rewetted extraction (oroginally paludiculture)
levels(dat$land_use)[levels(dat$land_use) == "Paludiculture"] <- "Rewetted extraction"
levels(dat$land_use)[levels(dat$land_use) == "Fen/reed bed"] <- "Semi-natural fen"
#
#
# Determine which data is a part of repeated sampling and which is synoptic
df_counts <- dat %>%
  group_by(site) %>%
  summarise(sample_count = n_distinct(date)) %>%
  mutate(sampling_frequency = ifelse(sample_count >= 3, "repeated", "synoptic"))
# sites sampled once are synoptic
# sites sampled 3 times are: WM, RIS, LM, DEL, WF
# From Mike, other repeated sites: Wicken (Sedge Fen, Bakers Fen, Burwell Fen, Tubney Fen); Manchester Mosses (Railway View, Foresters Field, Little Woolden, Holcroft Moss); Great Fen, Woodwalton, Holme sites; Lancashire Mosses sites + Wrights; Rosedene, Roughs. 
#
# Make a new column called sampling_frequency and differentiate between synoptic and repeated
# Add new column to the dat dataframe: 
dat <- dat %>%
  left_join(df_counts %>% select(site, sampling_frequency), by = "site")
#
#data summary
#
#dat_sum <- dat %>%
#  group_by(site, month, land_use) %>%
#  summarise(Count = n(), .groups = "drop") 
#
#
# For plotting, remove the site=NA, for "Adney South?" until you confirm this site
dat <- dat %>%
  filter(!is.na(site))
#
# reorder columns and save file
#
dat <- dat %>% select(sample_code, site_label, site, date, month, land_use, sampling_frequency, coordinates, notes, everything())
#
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/LP3_ditch_wq_dat_combined.csv")
#
############

#plots#
#
colnames(dat)
#"pH"          "EC_us_cm"   "NPOC_mg_l"     "F_mg_l"      "Cl_mg_l"    
#"NO2_mg_l"    "NO3_mg_l"    "PO4_mg_l"    "SO4_mg_l"    "Li_mg_l"     "Na_mg_l"     "NH4_mg_l"    "Mg_mg_l"    
#"K_mg_l"      "Ca_mg_l"     "P_mg_l"      "Si_mg_l"     "Al_ug_l"     "As_ug_l"     "Cd_ug_l"     "Cr_ug_l"    
#"Cu_ug_l"     "Fe_ug_l"     "Mn_ug_l"     "Ni_ug_l"     "Pb_ug_l"     "Zn_ug_l"     "IC_mg_l"     "TC_mg_l"    
#"TOC_mg_l"    "TN_mg_l" 
#
# Change order of land use factor
dat$land_use <- factor(dat$land_use, levels = c("Semi-natural bog", "Semi-natural fen",  "Grassland", "Rewetted bog", "Rewetted extraction", "Cropland",  "River/HLC"    ))
#
#
# Calculate mean and SD for each land use per month
## NOT NEEDED IF USING STAT_SUMMARY - CAN DELETE 
summary_dat <- dat %>%
  group_by(month, land_use) %>%
  summarise(
    across(
      where(is.numeric), 
      list(mean = ~mean(. , na.rm = TRUE), sd = ~sd(. , na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
#
#
#
#
#### pH ####


#tiff("LP3+_water_quality_pH.tiff", units="in", width=6.5, height=4, res=300)

pH <- ggplot(dat, aes(x = land_use, y = pH, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
    labs( y = "pH",  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) #element_text(angle = 45, hjust = 1, size=12)
pH

#dev.off()

pH_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = pH, colour = land_use )) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = "pH", colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.title = element_text(size = 14),  axis.text.y = element_text(size = 12), legend.title = element_blank(),  axis.title.x = element_blank()) + 
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
pH_time
#
#
#
# Create a new dataframe to select the first sample for each site
dat_labels_pH <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(pH = mean(pH, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup() 


pH_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = pH, colour = land_use, group = interaction(site, land_use) )) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = "pH", colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.title = element_text(size = 14),  axis.text.y = element_text(size = 12), legend.title = element_blank(),  axis.title.x = element_blank()) + 
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +
scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                            "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                            "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                            "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_pH, aes(label = site), size = 3, colour = "black", box.padding = 0.35, 
                  max.overlaps = 10) 
pH_time2

#dev.off()


#### EC ####

#tiff("LP3+_water_quality_EC.tiff", units="in", width=6.5, height=4, res=300)

EC <- ggplot(dat, aes(x = land_use, y = EC_us_cm, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(  y = expression("Conductivity (" * mu * "S cm"^"-1" * ")"),  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12), 
EC

#dev.off()
#
#
#
#
EC_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = EC_us_cm, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Conductivity (" * mu * "S cm"^"-1" * ")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
EC_time
#
#
#
# Create a new dataframe to select the first sample for each site
dat_labels_EC <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(EC_us_cm = mean(EC_us_cm, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
EC_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = EC_us_cm, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Conductivity (" * mu * "S cm"^"-1" * ")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
 # geom_text(data = dat_labels_EC, aes(label = site), size = 3, hjust = 0.5, vjust = -0.5, colour = "black")
  geom_text_repel(data = dat_labels_EC, aes(label = site), size = 3, colour = "black", box.padding = 0.35, 
                  max.overlaps = 10) 
EC_time2
#
#
#### Fluoride ####

#tiff("LP3+_water_quality_Fluoride.tiff", units="in", width=6.5, height=4, res=300)
Fluo <- ggplot(dat, aes(x = land_use, y = F_mg_l, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("F- (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )  + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #axis.text.x = element_text(angle = 45, hjust = 1, size=12), 
Fluo
#dev.off()
#
#
#
#### Chloride ####

#tiff("LP3+_water_quality_Chloride.tiff", units="in", width=6.5, height=4, res=300)

Cl <- ggplot(dat, aes(x = land_use, y =Cl_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Cl- (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
Cl
#dev.off()
#
#
#
Cl_time <-  ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Cl_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Cl- (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Cl_time
#
#
#
dat_labels_Cl <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Cl_mg_l = mean(Cl_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Cl_time2 <-  ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Cl_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Cl- (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)   +
geom_text_repel(data = dat_labels_Cl, aes(label = site), size = 3, colour = "black", box.padding = 0.35, 
 max.overlaps = 10) 
Cl_time2

#### Nitrite	#### 
#tiff("LP3+_water_quality_NO2.tiff", units="in", width=6.5, height=4, res=300)

NO2 <- ggplot(dat, aes(x = land_use, y =NO2_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  #scale_y_log10() + # the 0s cause an error with the log transformation
  scale_y_continuous(trans = 'pseudo_log') +
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
NO2
#dev.off()    
#
#
#
NO2_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = NO2_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) 
NO2_time
#
#
#
dat_labels_NO2 <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(NO2_mg_l = mean(NO2_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
NO2_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = NO2_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_NO2, aes(label = site), size = 3, colour = "black", box.padding = 0.35, 
                  max.overlaps = 10) 
NO2_time2

#### Nitrate ####

#("LP3+_water_quality_NO3.tiff", units="in", width=6.5, height=4, res=300)

NO3 <- ggplot(dat, aes(x = land_use, y =NO3_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
NO3    #element_text(angle = 45, hjust = 1, size=12), 
#
#
NO3_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = NO3_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) 
NO3_time
#
#
#
dat_labels_NO3 <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(NO3_mg_l = mean(NO3_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
NO3_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = NO3_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)  +
  geom_text_repel(data = dat_labels_NO3, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 15) 
NO3_time2
#
#
#### Bromine (in the excel its Bromide which is the ion) ####

#tiff("LP3+_water_quality_Br.tiff", units="in", width=6.5, height=4, res=300)

#Br <- ggplot(dat, aes(x = land_use, y =Br_mg_l,  fill = land_use)) + # Use fill for land use categories
  #geom_boxplot(outlier.shape = NA) + 
  #geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  #labs(y = expression(Br~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  #theme_minimal() + # Clean theme
  #theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "Rewetted bog" = "#6C91BF", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) #element_text(angle = 45, hjust = 1, size=12), 
#Br
#dev.off() # Br data not available in the updated OU data files
#
#
#### Phosphate	####



#tiff("LP3+_water_quality_PO4.tiff", units="in", width=6.5, height=4, res=300)

PO4 <- ggplot(dat, aes(x = land_use, y =PO4_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(PO[4]^"3-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
 # scale_y_log10() + 
  scale_y_continuous(trans = 'pseudo_log') +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + 
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  # element_text(angle = 45, hjust = 1, size=12), 
PO4
#dev.off()
#
#
PO4_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = PO4_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(PO[4]^"3-" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
PO4_time
#
#
#
dat_labels_PO4 <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(PO4_mg_l = mean(PO4_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
PO4_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = PO4_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(PO[4]^"3-" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
geom_text_repel(data = dat_labels_PO4, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
PO4_time2

#### Sulfate	#### 

#tiff("LP3+_water_quality_SO4.tiff", units="in", width=6.5, height=4, res=300)

SO4 <- ggplot(dat, aes(x = land_use, y =SO4_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(SO[4]^"2-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12), 
SO4
#
#
SO4_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = SO4_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(SO[4]^"2-" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
SO4_time
#
#
#
#
dat_labels_SO4 <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(SO4_mg_l = mean(SO4_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
tiff("LP3+_water_quality_SO4_time2.tiff", units="in", width=6.5, height=4, res=300)
#
SO4_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = SO4_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(SO[4]^"2-" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_SO4, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
SO4_time2
#
dev.off()
#
#
#### NPOC ####

NPOC <- ggplot(dat, aes(x = land_use, y =NPOC_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("NPOC (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12), axis.text.x= element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12), 
NPOC


# NPOC_time only 2 dates, so wait until more data 

#### Total Carbon ####

TC <- ggplot(dat, aes(x = land_use, y =TC_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("TC (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12), 
TC


#### Total Nitrogen #####


TN <- ggplot(dat, aes(x = land_use, y =TN_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("TN (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12),  
TN



#### Total organic carbon #####


TOC <- ggplot(dat, aes(x = land_use, y =TOC_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("TOC (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #
TOC  #element_text(angle = 45, hjust = 1, size=12),


#### Lithium	####

#tiff("LP3+_water_quality_Li.tiff", units="in", width=6.5, height=4, res=300)

#Li <- ggplot(dat, aes(x = land_use, y =Li_mg_l,  fill = land_use)) + # Use fill for land use categories
 # geom_boxplot(outlier.shape = NA) + 
 # geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
 # labs(y = expression("Li (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
 # theme_minimal() + # Clean theme
 # theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "Rewetted bog" = "#6C91BF",  "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) #element_text(angle = 45, hjust = 1, size=12)
#Li

#dev.off()    # new data is all 0


#### Sodium	####

#tiff("LP3+_water_quality_Na.tiff", units="in", width=6.5, height=4, res=300)

Na <- ggplot(dat, aes(x = land_use, y =Na_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Na (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
Na
#dev.off()
#
#
Na_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Na_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Na (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Na_time
#
#
#
#
dat_labels_Na <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Na_mg_l = mean(Na_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Na_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Na_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Na (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)   +
  geom_text_repel(data = dat_labels_Na, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Na_time2

#### Ammonium	####

#tiff("LP3+_water_quality_NH4.tiff", units="in", width=6.5, height=4, res=300)

NH4 <- ggplot(dat, aes(x = land_use, y =NH4_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  #scale_y_log10() + 
  #scale_y_continuous(trans = 'pseudo_log') +
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 20, 30, 40), labels = scales::number)+
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
NH4
#dev.off()
#
#
#
NH4_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = NH4_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
NH4_time
#
#
#
#
dat_labels_NH4 <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(NH4_mg_l = mean(NH4_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
NH4_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = NH4_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)   + 
  geom_text_repel(data = dat_labels_NH4, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
NH4_time2

#### Magnesium	#### 

#tiff("LP3+_water_quality_Mg.tiff", units="in", width=6.5, height=4, res=300)

Mg <- ggplot(dat, aes(x = land_use, y =Mg_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Mg (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12), 
Mg
#dev.off()
#
#
Mg_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Mg_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Mg (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Mg_time
#
#
#
#
dat_labels_Mg <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Mg_mg_l = mean(Mg_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Mg_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Mg_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Mg (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
geom_text_repel(data = dat_labels_Mg, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Mg_time2


#### Potassium	#### 

#tiff("LP3+_water_quality_K.tiff", units="in", width=6.5, height=4, res=300)

K <- ggplot(dat, aes(x = land_use, y =K_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("K (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 100), labels = scales::number)+
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12),
K
#dev.off()
#
#
#
K_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = K_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("K (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
K_time
#
#
#
#
dat_labels_K <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(K_mg_l = mean(K_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
K_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = K_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("K (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_K, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
K_time2
#
#### Calcium	#### 

#tiff("LP3+_water_quality_Ca.tiff", units="in", width=6.5, height=4, res=300)

Ca <- ggplot(dat, aes(x = land_use, y =Ca_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Ca (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
Ca
#dev.off()
#
#
#
Ca_time <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Ca_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Ca (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Ca_time
#
#
#
#
dat_labels_Ca <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Ca_mg_l = mean(Ca_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Ca_time2 <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Ca_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Ca (mg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_Ca, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Ca_time2
#
#### Al	#####

#tiff("LP3+_water_quality_Al.tiff", units="in", width=6.5, height=4, res=300)

Al <- ggplot(dat, aes(x = land_use, y =Al_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Al (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
Al
#dev.off()
#
#
#
Al_time  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Al_ug_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Al (µg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +   scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +  
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Al_time
#
#
#
#
dat_labels_Al <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Al_ug_l = mean(Al_ug_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Al_time2  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Al_ug_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Al (µg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +   scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +  
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_Al, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Al_time2
#
#### As ####

#tiff("LP3+_water_quality_As.tiff", units="in", width=6.5, height=4, res=300)

As <- ggplot(dat, aes(x = land_use, y =As_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("As (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme  
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 50, 100), labels = scales::number)+
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
As  #element_text(angle = 45, hjust = 1, size=12),

#dev.off()

#### Cd	####

#tiff("LP3+_water_quality_Cd.tiff", units="in", width=6.5, height=4, res=300)

#Cd <- ggplot(dat, aes(x = land_use, y =Cd_ug_l,  fill = land_use)) + # Use fill for land use categories
 # geom_boxplot(outlier.shape = NA) + 
#  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
#  labs(y = expression("Cd (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
##  theme_minimal() + # Clean theme
#  #scale_y_log10() + 
#  scale_y_continuous(trans = 'pseudo_log') +
#  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))   #element_text(angle = 45, hjust = 1, size=12), 
#Cd    # Only 4 data points non-zero

#dev.off()

#### Cr	#### 

#tiff("LP3+_water_quality_Cr.tiff", units="in", width=6.5, height=4, res=300)

Cr <- ggplot(dat, aes(x = land_use, y =Cr_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Cr (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
Cr
#dev.off()
#
#
#
Cr_time  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Cr_ug_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Cr (µg l"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) 
Cr_time
#
#
#
#
dat_labels_Cr <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Cr_ug_l = mean(Cr_ug_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Cr_time2  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Cr_ug_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Cr (µg l"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)  +
  geom_text_repel(data = dat_labels_Cr, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Cr_time2
#
#### Cu	####

#tiff("LP3+_water_quality_Cu.tiff", units="in", width=6.5, height=4, res=300)

Cu <- ggplot(dat, aes(x = land_use, y =Cu_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Cu (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
Cu
#dev.off()
#
#
Cu_time  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Cu_ug_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Cu (µg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Cu_time
#
#
#
#
dat_labels_Cu <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Cu_ug_l = mean(Cu_ug_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Cu_time2  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Cu_ug_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Cu (µg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_Cu, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Cu_time2


#### Fe	#### 

#tiff("LP3+_water_quality_Fe.tiff", units="in", width=6.5, height=4, res=300)

Fe <- ggplot(dat, aes(x = land_use, y =Fe_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Fe (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  #scale_y_continuous(trans = 'pseudo_log') +
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 100, 1000, 10000), labels = scales::number)+
  #scale_y_log10() +        #only 2 zeros, so this looks better than pseudo_log
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12)
Fe
#dev.off() #there are 3 zeros which messes with the log scale
#
#
#
Fe_time  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Fe_ug_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Fe (µg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Fe_time
#
#
#
#
dat_labels_Fe <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Fe_ug_l = mean(Fe_ug_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Fe_time2  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Fe_ug_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Fe (µg L"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_Fe, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Fe_time2

#### Inorganic Carbon ####

#tiff("LP3+_water_quality_IC.tiff", units="in", width=6.5, height=4, res=300)
 
IC <- ggplot(dat, aes(x = land_use, y =IC_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("IC (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  #scale_y_log10() + 
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 3, 10, 30), labels = scales::number) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12)
IC

#dev.off()


#### Mn	####

#tiff("LP3+_water_quality_Mn.tiff", units="in", width=6.5, height=4, res=300)

Mn <- ggplot(dat, aes(x = land_use, y =Mn_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Mn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 10, 100, 1000, 10000), labels = scales::number)+
  #scale_y_continuous(trans = 'pseudo_log') +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12), 
Mn
#dev.off()
#
Mn_time  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Mn_ug_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Mn (µg L"^-1*")"),  colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Mn_time
#
#
#
#
dat_labels_Mn <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Mn_ug_l = mean(Mn_ug_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Mn_time2  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Mn_ug_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Mn (µg L"^-1*")"),  colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
geom_text_repel(data = dat_labels_Mn, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Mn_time2
#
#### Ni	#### 

#tiff("LP3+_water_quality_Ni.tiff", units="in", width=6.5, height=4, res=300)

Ni <- ggplot(dat, aes(x = land_use, y =Ni_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Ni (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 5, 10, 20, 50), labels = scales::number)+
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12), 
Ni
#dev.off()
#
#
Ni_time  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Ni_ug_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Ni (µg L"^-1*")"),  colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Ni_time
#
#
#
#
dat_labels_Ni <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Ni_ug_l = mean(Ni_ug_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Ni_time2  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Ni_ug_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Ni (µg L"^-1*")"),  colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_Ni, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Ni_time2
#
#### Pb ####	

#tiff("LP3+_water_quality_Pb.tiff", units="in", width=6.5, height=4, res=300)

Pb <- ggplot(dat, aes(x = land_use, y =Pb_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Pb (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12),
Pb

#dev.off()


#### Zn	#### 
#tiff("LP3+_water_quality_Zn.tiff", units="in", width=6.5, height=4, res=300)

Zn <- ggplot(dat, aes(x = land_use, y =Zn_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Zn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  #scale_y_log10() + 
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 100, 1000), labels = scales::number)+
  # scale_y_continuous(trans = 'pseudo_log') +
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
Zn

#dev.off()    # the zeros introduce an error with the log scale

#### P	####
#tiff("LP3+_water_quality_P.tiff", units="in", width=6.5, height=4, res=300)

#can use units of ug or mg

P <- ggplot(dat, aes(x = land_use, y =P_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("P (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  #scale_y_log10(labels = label_number()) +
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10), labels = scales::number)+
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  
P
#dev.off()     # There are 2 zeros which produce an error for the log transformation
#
#
P_time  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = P_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("P (mg L"^-1*")"),  colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
P_time
#
#
#
#
dat_labels_P <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(P_mg_l = mean(P_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
P_time2  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = P_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("P (mg L"^-1*")"),  colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12),panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
  geom_text_repel(data = dat_labels_P, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
P_time2
#
#### Si ####
#tiff("LP3+_water_quality_Si.tiff", units="in", width=6.5, height=4, res=300)

# for units can use ug or mg

Si <- ggplot(dat, aes(x = land_use, y =Si_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Si (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=12),   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) #element_text(angle = 45, hjust = 1, size=12), 
Si   #
#dev.off()
#
#
Si_time  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Si_mg_l, colour = land_use)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Si (mg l"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE)
Si_time
#
#
#
#
dat_labels_Si <- dat %>%
  filter(sampling_frequency == "repeated") %>%
  group_by(site, land_use, month) %>%
  summarise(Si_mg_l = mean(Si_mg_l, na.rm = TRUE), .groups = "drop") %>%
  group_by(site, land_use) %>%
  slice_min(order_by = month, n = 1) %>%
  ungroup()
#
Si_time2  <- ggplot(subset(dat, sampling_frequency =="repeated"), aes(x = month, y = Si_mg_l, colour = land_use, group = interaction(site, land_use))) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    labs(y = expression("Si (mg l"^-1*")"), colour = "Land Use") +  theme_minimal() +  theme(legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank()) +  
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  scale_colour_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), drop = FALSE) +
geom_text_repel(data = dat_labels_Si, aes(label = site), size = 3, colour = "black", box.padding = 0.35, max.overlaps = 20) 
Si_time2
#
#### combine plots ####
# use cowplot bc ggarrange spacing is weird (too much white space)


jpeg("LP3+_water_quality1.jpeg", units="in", width=10, height=12, res=300)

combine1 <- plot_grid(Al, As, 
                      Ca, Cl, Cr, Cu, 
                      EC, Fluo, Fe, IC, 
                      ncol = 2, align = "v", rel_heights = c( 1,1,1, 1, 1.6) )   # Excluded Cd bc very few data points
combine1

dev.off()

#
#
#
#
jpeg("LP3+_water_quality2.jpeg", units="in", width=10, height=12, res=300)

combine2 <- plot_grid( K, Mg, Mn, Na, NH4,  
                      Ni, NO2, NO3, NPOC,
                      P,
                      ncol = 2, align = "v", rel_heights = c( 1,1,1,1, 1.6)) 
combine2

dev.off()
#
#
#
#
jpeg("LP3+_water_quality3.jpeg", units="in", width=10, height=12, res=300)

combine3 <- plot_grid(pH, Pb,
                      PO4, Si, SO4, TC, TN, TOC, Zn,  
                      ncol = 2, align = "v", rel_heights = c( 1,1,1, 1, 1.6)) 
combine3

dev.off()
#
#############################################################################
#### Combine time series plots ####
# Use ggarrange as it allows for an easy common legend

jpeg("LP3+_water_quality_timeseries1.jpeg", units="in", width=10, height=12, res=200)
time_series_a <- ggarrange(Al_time, Ca_time, Cr_time, Cu_time, 
                     EC_time, Fe_time, K_time, Mg_time, Mn_time, Na_time,
                      ncol = 2, nrow=5,  align = "v", common.legend=T )
time_series_a
dev.off() # the semi natural bog and grassland colour does not appear, might have to add manually in Illustrator (might get fixed once you have more data)

jpeg("LP3+_water_quality_timeseries2.jpeg", units="in", width=10, height=12, res=200)
time_series_b <- ggarrange(NH4_time, Ni_time, NO2_time, NO3_time, 
                          P_time, pH_time2, PO4_time, Si_time, SO4_time,
                          ncol = 2, nrow=5,  align = "v", common.legend=T )
time_series_b
dev.off()  # the semi natural bog colour does not appear, might have to add manually in Illustrator
#
#
#
# Time series with site labels
#
jpeg("LP3+_water_quality_timeseries_site1.jpeg", units="in", width=10, height=11, res=200)
time_series_1 <- ggarrange(Al_time2, Ca_time2, Cr_time2, Cu_time2, EC_time2, Fe_time2,
                           ncol = 2, nrow=3,  align = "v", common.legend=T )
time_series_1
dev.off() 



jpeg("LP3+_water_quality_timeseries_site2.jpeg", units="in", width=12, height=14, res=200)
time_series_2 <- ggarrange(  K_time2, Mg_time2, Mn_time2, Na_time2,  NH4_time2, Ni_time2, 
                          ncol = 2, nrow=3,  align = "v", common.legend=T )
time_series_2
dev.off()


jpeg("LP3+_water_quality_timeseries_site3.jpeg", units="in", width=12, height=14, res=200)
time_series_3 <- ggarrange( NO2_time2, NO3_time2, P_time2, pH_time2, PO4_time2, Si_time2, 
                             ncol = 2, nrow=3,  align = "v", common.legend=T )
time_series_3
dev.off()

# SO4_time2,


