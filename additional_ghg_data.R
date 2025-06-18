#### R script for ghg concentrations/fluxes for 'additional' "LP3" data #######
#
# Load necessary packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(purrr)
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(cowplot)
library(forcats)
library(lubridate)
library(fuzzyjoin)
library(corrplot)
library(Hmisc)
#
# Set wd for figures
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Figures")
#
# Read in GHG concentration data (from Mike's excel file calculations)
conc_dat <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Flux calculations/Dissolved GHG calc sheet_2024_TS_additional_ghg_data.xlsx", sheet="output")
head(conc_dat)  #64 obs of 4 vars
#
#
# Read in GHG flux data (from Mike's excel file calculations)
flux_dat <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Flux calculations/Copy of Fluxes from concs_additional_ghg_data.xlsx", sheet="output")
head(flux_dat)  #64 of 4 vars
#
#
# Read in ancil dat
ancil_dat<- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/Additional GHG data/additional_ghg_data_ppm.csv")
head(ancil_dat)
#
ancil_dat <- ancil_dat %>%
  mutate(date = as.Date(date, format = "%d-%b-%y")) %>%
  mutate(date = as.POSIXct(date))
#
#
# Combine concentration and flux data
dat <- conc_dat %>%
  left_join(flux_dat, by = c("sample_name", "date")) %>%
  left_join(ancil_dat %>% select(sample_name, site, land_use, date), by = c("sample_name", "date"))
#
#
# Semi natural bog only has one point, so make a new level just called semi natural that combines with fens
dat$land_use <- as.factor(dat$land_use) 
#
dat <- dat %>%
  mutate(land_use = recode_factor(land_use,
                           "Semi-natural fen" = "Semi-natural",
                           "Semi-natural bog" = "Semi-natural"))
#
# Reorder
dat$land_use <- factor(dat$land_use, levels = c("Semi-natural", "Grassland", "Rewetted bog", "Rewetted extraction", "Cropland",  "River/HLC"    ))
#
# For reporting and plotting make a new column with CO2 in g and CH4 mg/l
dat$CO2_g_m2_d <- dat$CO2_mg_m2_d/1000
dat$CH4_mg_l <- dat$CH4_ug_l/1000
dat$N2O_mg_l <- dat$N2O_ug_l/1000
#
# Make a column for month
dat <- dat %>%
  mutate(month = factor(format(date, "%b"), levels = month.abb))
#
#
# Determine which data is a part of repeated sampling and which is synoptic
df_counts <- dat %>%
  group_by(site) %>%
  summarise(sample_count = n_distinct(date)) %>%
  mutate(sampling_frequency = ifelse(sample_count >= 3, "repeated", "synoptic"))
# Number of sites
n_distinct(dat$site)
#
# Rename sample names to align with water quality data
dat$sample_name <- trimws(dat$sample_name) # trim white spaces
#
dat$sample_name <- sapply(dat$sample_name, function(x) {
  switch(x,
         "Baker's Fen BF1 (East)" = "Baker's Fen East",
         "Baker's Fen BF4 (West)" = "Baker's Fen West",
         "Burwell Fen Harrison's Drove" = "Harrison's Drove July",
         "Burwell Fen Ditch 1" = "Burwell West July",      #assumed West based on landscape position
        "Burwell Fen Ditch 2" =  "Burwell South July",      #assumed South based on landscape position
         "Delamere Alvanley lake"  = "Alvanley Lake",
         "Delamere Blakemere ditch" = "Blakemere Ditch",
         "Foresters' Field North ditch"  = "Foresters North" ,
         "Foresters' Field West ditch"  = "Foresters West",
          "Foresters' Field Internal ditch"  = "Foresters Internal",
         "Holliday Moss Outflow" = "Holiday Moss",
          "Little Woolden Moss East outflow" = "LWM East outflow", 
          "Little Woolden Moss Lake" = "LWM Lake",
         "Little Woolden Moss West outflow north" = "LWM West outflow north", 
         "Little Woolden Moss West outflow south"  = "LWM West outflow south",
          "Moss Side Farm Central ditch" = "MM Central",
          "Moss Side Farm North ditch" = "MM N",
          "Railway View East ditch" = "Railway View East",
          "Railway View NW ditch" = "Railway View NW",
          "Stuarts East ditch" = "Stuarts East June",
          "Stuarts Pond"  =  "Stuarts Pond June", 
          "Weald Moors Adney North"  =  "Adney North",
           "Weald Moors Adney South" = "Adney South", 
            "Weald Moors HA2" = "HA2",
         x)  # Default case: returns original value if no match
})
# also align Wrights naming
dat$sample_name <- gsub("Wright's", "Wrights", dat$sample_name)
#
#
# Try to combine some water quality data to examine relationships
#
water_qual <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/LP3+_wq_dat_combined.csv")
# change date format
water_qual <- water_qual %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d", tz = "UTC"))
#
colnames(water_qual)[colnames(water_qual) == "site_label"] <- "sample_name"  # rename column
#
dat_joined <- dat %>%
  left_join(water_qual, by = c("sample_name", "date"))
#
# Note that some GHG measures don't have accompanying wq data, for example, no wq data at Railway view nor Foresters or Roughs IDB on 20/05/2024


##############################
### Plots ####

### concentrations ###
CO2_conc <- ggplot(dat, aes(x = fct_reorder(land_use, CO2_mg_l, .fun = median, .na_rm = TRUE), y = CO2_mg_l, fill = land_use)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression(CO[2] ~ "(mg L"^-1*")"),  x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 100), labels = scales::number)+
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=12),  axis.text.y = element_text(size=12), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural" = "#6DA34D" ))  +
  annotate("text", x =1, y = 300 , label = "a", size=3) + 
  annotate("text", x =2, y = 300 , label = "ab", size=3) +
  annotate("text", x =3, y = 300 , label = "ab", size=3) +
  annotate("text", x =4, y = 300 , label = "ab", size=3) +
  annotate("text", x =5, y = 300 , label = "ab", size=3) +
  annotate("text", x =6, y = 300 , label = "b", size=3)
CO2_conc 
# Sig diffs: Rewetted bog - Cropland                -8.580 2.54 57  -3.382  0.0157
#
#
CH4_conc <- ggplot(dat, aes( x= fct_reorder(land_use, CH4_ug_l, .fun = median, .na_rm = TRUE), y = CH4_ug_l, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs( y = expression(CH[4] ~ "(μg L"^-1*")"),   x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  #scale_y_log10() +
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(10, 100, 1000, 10000), labels = scales::number)+
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=12),  axis.text.y = element_text(size=12), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",   "Semi-natural" = "#6DA34D" )) 
CH4_conc 
#
#
N2O_conc <- ggplot(dat, aes(x= fct_reorder(land_use, N2O_ug_l, .fun = median, .na_rm = TRUE), y = N2O_ug_l, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression(N[2]*O ~ " (μg" ~ L^{-1} ~ ")"),   x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(1, 10, 100, 1000), labels = scales::number) +
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=12), axis.text.y = element_text(size=12), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural" = "#6DA34D" )) 
N2O_conc #axis.text.x = element_text(angle = 45, hjust = 1, size=12),
#
#
###############################################################################
### fluxes ###
#
CO2_flux <- ggplot(dat, aes( x= fct_reorder(land_use, CO2_g_m2_d, .fun = median, .na_rm = TRUE), y = CO2_g_m2_d, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression(g~CO[2]*~m^-2*~d^-1), x = NULL, fill = " " ) +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 3, 10, 30), labels = scales::number)+
  theme( legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1, size=12), axis.title = element_text(size = 14), axis.text.y = element_text(size=12), panel.grid.major = element_blank(),  panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",   "Semi-natural" = "#6DA34D" )) 
CO2_flux #axis.text.x = element_text(angle = 45, hjust = 1, size=12),
#
#
CH4_flux <- ggplot(dat, aes(x= fct_reorder(land_use, CH4_mg_m2_d, .fun = median, .na_rm = TRUE), y = CH4_mg_m2_d, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs( y = "CO2",  x = NULL,   fill = "Land Use" ) +
  labs( y = expression(~mg~CH[4]*~m^-2~d^-1),   x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  #scale_y_log10() +
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(1, 10, 100, 1000), labels = scales::number)+
    theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1, size=12),
         panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",   "Semi-natural" = "#6DA34D" )) 
CH4_flux # axis.text.x = element_text(angle = 45, hjust = 1, size=12),
#
#
N2O_flux <- ggplot(dat, aes(x= fct_reorder(land_use, N2O_mg_m2_d, .fun = median, .na_rm = TRUE), y = N2O_mg_m2_d, fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs( y = "CO2",  x = NULL,   fill = "Land Use" ) +
  labs(y = expression(mg~N[2]*`O`*~m^-2~d^-1),   x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  #scale_y_log10() +
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(1, 10, 100, 1000), labels = scales::number)+
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12), panel.grid.major = element_blank(),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),
         panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural" = "#6DA34D" )) 
N2O_flux

################################################################################
## combine plots ##
#
#
jpeg("additional_ghg_plots_all.jpeg", units="in", width=8, height=10, res=300)

flux_conc <-  plot_grid(CO2_conc, CO2_flux, CH4_conc, CH4_flux, N2O_conc, N2O_flux,
                        ncol=2, align="v", labels = c("A", "B", "C", "D", "E", "F"))
flux_conc

dev.off()




jpeg("additional_ghg_plots_CO2.jpeg", units="in", width=10, height=5, res=300)

flux_concCO2 <- plot_grid(CO2_conc, CO2_flux,  
                       ncol = 2, align = "v") 
flux_concCO2

dev.off()
#
#
jpeg("additional_ghg_plots_CH4.jpeg", units="in", width=10, height=5, res=300)

flux_concCH4 <- plot_grid(CH4_conc, CH4_flux,  
                          ncol = 2, align = "v") 
flux_concCH4

dev.off()
#
#
jpeg("additional_ghg_plots_N2O.jpeg", units="in", width=10, height=5, res=300)

flux_concN2O <- plot_grid(N2O_conc, N2O_flux,  
                          ncol = 2, align = "v") 
flux_concN2O

dev.off()
#
#
#
jpeg("additional_ghg_plots.jpeg", units="in", width=10, height=12, res=300)

flux_conc <- plot_grid(CO2_conc, CO2_flux, CH4_conc, CH4_flux, N2O_conc, N2O_flux,  
                      ncol = 2, align = "v", rel_heights = c( 1,1, 1.6)) 
flux_conc

dev.off()

################################################################################
#### Statistical analysis ####
#
# Summary stats
#
mean(dat$CO2_mg_l, na.rm=T)
sd(dat$CO2_mg_l, na.rm=T)

mean(dat$CH4_mg_l, na.rm=T)
sd(dat$CH4_mg_l, na.rm=T)

mean(dat$N2O_mg_l, na.rm=T)
sd(dat$N2O_mg_l, na.rm=T)
#
#
mean(dat$CO2_g_m2_d, na.rm=T)
sd(dat$CO2_g_m2_d, na.rm=T)

mean(dat$CH4_mg_m2_d, na.rm=T)
sd(dat$CH4_mg_m2_d, na.rm=T)

mean(dat$N2O_mg_m2_d, na.rm=T)
sd(dat$N2O_mg_m2_d, na.rm=T)

# % uptake
dat %>% 
  summarise(percent_neg = mean(CO2_g_m2_d < 0, na.rm = TRUE) * 100)  #23.8
#
dat %>% 
  summarise(percent_neg = mean(CH4_mg_m2_d < 0, na.rm = TRUE) * 100)  #3.12
#
dat %>% 
  summarise(percent_neg = mean(N2O_mg_m2_d < 0, na.rm = TRUE) * 100)  #1.56


#
#
#Since you have repeated measures (6 ditches sampled twice, you need to account for the non-independence of those repeated samples.
# However, because the random error structure accounts for nearly 0 variablity, it is not needed

# Concentrations
CO2_conc_lmer <- lmer(CO2_mg_l ~ land_use + (1 | sample_name), data = dat)
#
CO2_conc_lmer_log <- lmer(log(CO2_mg_l) ~ land_use + (1 | sample_name), data = dat) # doesn't improve by much
# singularity error, random effect may be close to 0, the random structure isn't needed
anova(CO2_conc_lmer_log)
summary(CO2_conc_lmer_log)
#
summary(CO2_conc_lmer_log)$varcor
#
plot(CO2_conc_lmer) 
qqnorm(resid(CO2_conc_lmer))
qqline(resid(CO2_conc_lmer)) # both log and untransformed deviate from normality
#
shapiro.test(resid(CO2_conc_lmer))
shapiro.test(resid(CO2_conc_lmer_log)) # both have non-normal residuals, but LMM is robust to this
#
leveneTest(CO2_mg_l ~ land_use, data = dat)
leveneTest(log(CO2_mg_l) ~ land_use, data = dat) # no significant heterodescaity in either case
#
AIC(CO2_conc_lmer, CO2_conc_lmer_log) # lower AIC with log transformation, so go with that
#
#
#
# Linear models
CO2_conc_lm_log <- lm(log(CO2_mg_l) ~ land_use, data = dat)
summary(CO2_conc_lm_log)
anova(CO2_conc_lm_log) # p = 0.04
emmeans(CO2_conc_lm_log, pairwise ~ land_use) # pairwise differences: rewetted bog and cropland p = 0.02
###
min(dat$CO2_g_m2_d, na.rm = TRUE)
CO2_flux_lm_log <- lm(log(CO2_g_m2_d+1.4765558) ~ land_use, data = dat)
summary(CO2_flux_lm_log)
anova(CO2_flux_lm_log)  # p= 0.07 so not sig
#
#
# 
CH4_conc_lm_log <- lm(log(CH4_ug_l) ~ land_use, data = dat)
qqnorm(resid(CH4_conc_lm_log))
qqline(resid(CH4_conc_lm_log)) # log transforming improves residuals
anova(CH4_conc_lm_log) # not sig p=0.12
summary(CH4_conc_lm_log)
###
min(dat$CH4_mg_m2_d, na.rm = TRUE)
CH4_flux_lm_log <- lm(log(CH4_mg_m2_d+1.3956323) ~ land_use, data = dat)
qqnorm(resid(CH4_flux_lm_log))
qqline(resid(CH4_flux_lm_log))
summary(CH4_flux_lm_log)
anova(CH4_flux_lm_log)  # p= 0.1
#
#
#
N2O_conc_lm_log <- lm(log(N2O_mg_l) ~ land_use, data = dat)
qqnorm(resid(N2O_conc_lm_log))
qqline(resid(N2O_conc_lm_log)) # log transformation improves residuals
anova(N2O_conc_lm_log) # p = 0.054 (not sig)
summary(N2O_conc_lm_log)
#
###]
min(dat$N2O_mg_m2_d, na.rm = TRUE) #-0.04972287
N2O_flux_lm_log <- lm(log(N2O_mg_m2_d+1.04972287) ~ land_use, data = dat ) #there is just one negative value
qqnorm(resid(N2O_flux_lm_log))
qqline(resid(N2O_flux_lm_log)) # log transformation improves residuals
anova(N2O_flux_lm_log) # p = 0.059 (not sig)
summary(N2O_flux_lm_log)

#### Correlation between GHG and WQ ####
# Make a correlation plot (try both Spearman and Pearson)
#
#
#subset data that has both GHG and wq
dat_cor <- subset(dat_joined, !is.na(pH)) 
dat_numeric <- select(dat_cor, where(is.numeric))
dat_numeric <- dat_numeric %>%
  select(-X, -CO2_mg_m2_d, -CO2_g_m2_d, -CH4_mg_m2_d, -N2O_mg_m2_d, -CH4_ug_l, -N2O_mg_l,  -As_ug_l, -Cd_ug_l, -Cr_ug_l, -Li_mg_l, -IC_mg_l, -TC_mg_l, -TN_mg_l, -P_ug_l, -Pb_ug_l, -Cu_ug_l, -Zn_ug_l, -C_mol_L, -C_umol_L, -N_umol_L, -P_umol_L, -NH4_N_mg_l, -NO2_N_mg_l, -NO3_N_mg_l, -PO4_P_mg_l, -TIN_mg_l, -N_redfield, -P_redfield, -sum_CNP, -TIN, -C_tern, -N_tern, -P_tern, -CN_ratio, -CP_ratio) # drop empty columns and drop NPOC, Cd, Cr, Li bc mostly 0, as well as redundant GHG/stoich vars (use only conc) # because there are quite a lot of variables, 
zero_counts <- sapply(dat_numeric, function(x) sum(x == 0, na.rm = TRUE))  #drop also WQ vars with a lot of 0: Pb, Cu, Zn
#

# make data frame into matrix
dat_matrix <- Hmisc::rcorr(as.matrix(dat_numeric),  type = "spearman")
#
cor_mat <- dat_matrix$r
p_mat <- dat_matrix$P
#
# Make correlation plot
jpeg("LP3+_GHG_corrplot_spearman.jpeg", units="in", width=6, height=6, res=300)

corrplot(cor_mat,  type="upper", diag=FALSE, tl.col = "black",
         p.mat = p_mat, sig.level = 0.05, insig = "blank") 

dev.off()

