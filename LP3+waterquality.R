###### LP3 water quality data ###########
#
# load necessary packages
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggpubr)
library(cowplot)
#
#
# Set working directory for figures
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Figures")
#
#
# load in data
#
dat1 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/C111 Report INTERIM 20241126_TS_MP.xlsx", range= "A10:AF54")
#
head(dat1)
#
dat2 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Report C112 INTERIM 20250113_TS_MP.xlsx", range= "A10:AF48")
#
head(dat2)
#
dat3 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Report C102 20250306_TS.xlsx", range= "A10:AK64")
#
head(dat3)
#
#replace non numeric values with NAs (cases below detection limits or insufficient sample)
#
dat1 <- dat1 %>%
  mutate(across(7:32, ~as.numeric(.), .names = "{.col}"))
#
dat2 <- dat2 %>%
  mutate(across(7:32, ~as.numeric(.), .names = "{.col}"))
#
dat3 <- dat3 %>%
  mutate(across(8:37, ~as.numeric(.), .names = "{.col}"))
#
#You will get an NA's introduced by coercion error, this is fine as any cells with <, is, or nd will be replaced with NA
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
#combine
#
dat <- bind_rows(dat1, dat2, dat3)
#
#
#format date column
#
dat$date<-as.POSIXct(dat$date, format="%Y-%m-%d", tz = "GMT")
#
#
dat <- dat %>%
  mutate(month = factor(format(date, "%b"), levels = month.abb))
#
#
dat$land_use <- as.factor(dat$land_use)
#
# Add a month column 
#
#dat <- dat %>%
 # mutate(month = month(date, label = TRUE, abbr = TRUE))  #This one creates a factor
#
dat <- dat %>%
mutate(month = floor_date(date, unit = "month"))
#
#
#
#data summary
#
#dat_sum <- dat %>%
#  group_by(site, month, land_use) %>%
#  summarise(Count = n(), .groups = "drop") 
#
#
# reorder columns and save file
#
dat <- dat %>% select(sample_code, site_label, site, date, month, land_use, coordinates, notes, everything())
#
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/LP3_ditch_wq_dat_combined.csv")
#
############

#plots#

# Change order of land use factor
dat$land_use <- factor(dat$land_use, levels = c("Semi-natural bog", "Semi-natural fen",  "Grassland", "Grassland/raised WTs", "Rewetted extraction", "Cropland",  "River/HLC"    ))


# Calculate mean and SD

# Calculate mean and SD for each land use per month
summary_dat <- dat %>%
  group_by(month, land_use) %>%
  summarise(
    mean_pH = mean(pH),
    sd_pH = sd(pH),
    .groups = "drop"
  )


#### pH ####


tiff("LP3+_water_quality_pH.tiff", units="in", width=6.5, height=4, res=300)

pH <- ggplot(dat, aes(x = land_use, y = pH, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + # Jitter points 
  labs( y = "pH",  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
pH

dev.off()

pH_time <- ggplot(summary_dat, aes(x = month, y = mean_pH, color = land_use)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +  # Note position dodge and caps don't work unless date is a factor

  geom_errorbar(aes(ymin = mean_pH - sd_pH, ymax = mean_pH + sd_pH),
                width = 0.5, position = position_dodge(width = 0.2)) +  # SD whiskers
  geom_line(aes(group = land_use), linewidth = 1, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  labs(x = "Month", y = "Mean pH (±SD)", color = "Land Use") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
pH_time


#### EC ####

tiff("LP3+_water_quality_EC.tiff", units="in", width=6.5, height=4, res=300)

EC <- ggplot(dat, aes(x = land_use, y = EC_us_cm, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(  y = expression("Conductivity (" * mu * "S cm"^"-1" * ")"),  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
EC

dev.off()


#### Fluoride ####

tiff("LP3+_water_quality_Fluoride.tiff", units="in", width=6.5, height=4, res=300)

Fluo <- ggplot(dat, aes(x = land_use, y = F_mg_l, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("F- (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
Fluo

dev.off()



#### Chloride ####

tiff("LP3+_water_quality_Chloride.tiff", units="in", width=6.5, height=4, res=300)

Cl <- ggplot(dat, aes(x = land_use, y =Cl_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Cl- (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
Cl

dev.off()


#### Nitrite	#### 
tiff("LP3+_water_quality_NO2.tiff", units="in", width=6.5, height=4, res=300)

NO2 <- ggplot(dat, aes(x = land_use, y =NO2_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
NO2

dev.off()


#### Nitrate ####

tiff("LP3+_water_quality_NO3.tiff", units="in", width=6.5, height=4, res=300)

NO3 <- ggplot(dat, aes(x = land_use, y =NO3_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
NO3

dev.off()

#### Bromine (in the excel its Bromide which is the ion) ####

tiff("LP3+_water_quality_Br.tiff", units="in", width=6.5, height=4, res=300)

Br <- ggplot(dat, aes(x = land_use, y =Br_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(Br~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
Br

dev.off()


#### Phosphate	####



tiff("LP3+_water_quality_PO4.tiff", units="in", width=6.5, height=4, res=300)

PO4 <- ggplot(dat, aes(x = land_use, y =PO4_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(PO[4]^"3-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
PO4

dev.off()


#### Sulfate	#### 

tiff("LP3+_water_quality_SO4.tiff", units="in", width=6.5, height=4, res=300)

SO4 <- ggplot(dat, aes(x = land_use, y =SO4_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(SO[4]^"2-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
SO4

dev.off()

#### Lithium	####

tiff("LP3+_water_quality_Li.tiff", units="in", width=6.5, height=4, res=300)

Li <- ggplot(dat, aes(x = land_use, y =Li_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Li (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
Li

dev.off()


#### Sodium	####

tiff("LP3+_water_quality_Na.tiff", units="in", width=6.5, height=4, res=300)

Na <- ggplot(dat, aes(x = land_use, y =Na_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Na (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
Na

dev.off()

#### Ammonium	####

tiff("LP3+_water_quality_NH4.tiff", units="in", width=6.5, height=4, res=300)

NH4 <- ggplot(dat, aes(x = land_use, y =SO4_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D")) 
NH4

dev.off()

#### Magnesium	#### 

tiff("LP3+_water_quality_Mg.tiff", units="in", width=6.5, height=4, res=300)

Mg <- ggplot(dat, aes(x = land_use, y =Mg_mg_L,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Mg (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Mg

dev.off()



#### Potassium	#### 

tiff("LP3+_water_quality_K.tiff", units="in", width=6.5, height=4, res=300)

K <- ggplot(dat, aes(x = land_use, y =K_mg_L,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("K (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
K

dev.off()

#### Calcium	#### 

tiff("LP3+_water_quality_Ca.tiff", units="in", width=6.5, height=4, res=300)

Ca <- ggplot(dat, aes(x = land_use, y =Ca_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Ca (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Ca

dev.off()

#### Al	#####

tiff("LP3+_water_quality_Al.tiff", units="in", width=6.5, height=4, res=300)

Al <- ggplot(dat, aes(x = land_use, y =Al_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Al (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Al

dev.off()

#### As ####

tiff("LP3+_water_quality_As.tiff", units="in", width=6.5, height=4, res=300)

As <- ggplot(dat, aes(x = land_use, y =As_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("As (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
As

dev.off()

#### Cd	####

tiff("LP3+_water_quality_Cd.tiff", units="in", width=6.5, height=4, res=300)

Cd <- ggplot(dat, aes(x = land_use, y =Cd_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Cd (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Cd

dev.off()

#### Cr	#### 

tiff("LP3+_water_quality_Cr.tiff", units="in", width=6.5, height=4, res=300)

Cr <- ggplot(dat, aes(x = land_use, y =Cr_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Cr (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Cr

dev.off()

#### Cu	####

tiff("LP3+_water_quality_Cu.tiff", units="in", width=6.5, height=4, res=300)

Cu <- ggplot(dat, aes(x = land_use, y =Cu_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Cu (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Cu

dev.off()

#### Fe	#### 

tiff("LP3+_water_quality_Fe.tiff", units="in", width=6.5, height=4, res=300)

Fe <- ggplot(dat, aes(x = land_use, y =Fe_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Fe (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Fe

dev.off()

#### Mn	####

tiff("LP3+_water_quality_Mn.tiff", units="in", width=6.5, height=4, res=300)

Mn <- ggplot(dat, aes(x = land_use, y =Mn_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Mn (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Mn

dev.off()

#### Ni	#### 

tiff("LP3+_water_quality_Ni.tiff", units="in", width=6.5, height=4, res=300)

Ni <- ggplot(dat, aes(x = land_use, y =Ni_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Ni (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Ni

dev.off()


#### Pb ####	

tiff("LP3+_water_quality_Pb.tiff", units="in", width=6.5, height=4, res=300)

Pb <- ggplot(dat, aes(x = land_use, y =Pb_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Pb (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Pb

dev.off()


#### Zn	#### 
tiff("LP3+_water_quality_Zn.tiff", units="in", width=6.5, height=4, res=300)

Zn <- ggplot(dat, aes(x = land_use, y =Zn_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Zn (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Zn

dev.off()

#### P	####
tiff("LP3+_water_quality_P.tiff", units="in", width=6.5, height=4, res=300)

P <- ggplot(dat, aes(x = land_use, y =P_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("P (mg l"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
P

dev.off()

#### Si ####
tiff("LP3+_water_quality_Si.tiff", units="in", width=6.5, height=4, res=300)

Si <- ggplot(dat, aes(x = land_use, y =Si_ug_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression("Si (µg l"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Grassland/raised WTs" = "#A2D2FF","Rewetted extraction"= "#A5F2D4", "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D"))
Si

dev.off()



#### combine plots ####

jpeg("LP3+_water_quality1.jpeg", units="in", width=12, height=12, res=200)

combine1 <- ggarrange(Al, Ca, Cl, Cu, EC, Fluo, Fe, K, 
                    
                     ncol = 2, nrow = 4, align="h", common.legend = F, heights = c(1, 1, 1, 1.5)) # labels = c("(a)", "(b)", "(c)")
combine1

dev.off()


# try cowplot


combine1 <- plot_grid(Al, Ca, Cl, Cu, 
                      EC, Fluo, Fe, K, 
                      ncol = 2, align = "hv")
combine1









jpeg("LP3+_water_quality2.jpeg", units="in", width=12, height=12, res=200)

combine2 <- ggarrange(  Mg, Mn, Na, NH4,  
                      Ni, NO2, NO3, pH, 
                      PO4, SO4, Zn, 
                     ncol = 4, nrow = 5, align="hv",common.legend = F) # labels = c("(a)", "(b)", "(c)")
combine2

dev.off()



