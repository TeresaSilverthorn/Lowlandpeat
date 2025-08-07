#### R script to calculate ditch GHG fluxes from concentration data ####
# Load packages
library(dplyr)
library(ggplot2)
library(ggpubr)
library(purrr)
library(readxl)
library(lubridate)
#
# Notes from Mike on flux calc excel sheet: I’ve attached a sheet for diss GHGs. Fill in the green columns – hopefully self explanatory. Can assume 0m ASL elevation.The “fluxes” sheet, again add to green/yellow columns. I would set k600 as 0.33 m/d which was the median k600 for Rosedene in this paper (which only reports k, not k600, but nevermind)
#https://www.sciencedirect.com/science/article/pii/S0048969716324366
#
#
# Site file directory for figures 
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Figures")
#
##################################################################################################################
##### Concentrations ####
#
#### Use the excel sheet from Mike to calculate the dissolved gas concentration and read in that back in here ####
#
conc1 <- read_xlsx("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Flux calculations/Dissolved GHG calc sheet_2024_TS_EastAnglia_Dec2024.xlsx", sheet="output")
#
conc2 <- read_xlsx("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Flux calculations/Dissolved GHG calc sheet_2024_TS_Wrights_Dec24_Jan25.xlsx", sheet="output")
#
conc3 <- read_xlsx("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Flux calculations/Dissolved GHG calc sheet_2024_TS_EastAnglia_Feb2025.xlsx", sheet="output")
#
conc4 <- read_xlsx("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Flux calculations/Dissolved GHG calc sheet_2024_Somerset_Feb25_Mar25_Apr25_May25_Jun25.xlsx", sheet="output")
#
#
# Combine data setd
conc_full <- bind_rows(conc1, conc2, conc3, conc4) 
str(conc_full)  #152 obs of 5 columns
conc_full <- conc_full %>% select(1:5)
head(conc_full)
#
# Add a column for site and ditch #site = str_extract(sample_code, "(?<=-).*(?=-)"),
conc_full <- conc_full %>%
  mutate( site = str_extract(sample_code, "(?<=^[A-Z]-)[^-\\d]+(?:-[^-\\d]+)*"),
          site = if_else(str_detect(site, "RG-R"),paste0(site, str_extract(sample_code, "(?<=RG-R)\\d+")),site),
          ditch = str_extract(sample_code, "\\d+(\\.\\d+)?$") %>% as.numeric()  )
#
#
conc_full <- conc_full %>%
  mutate(  date = as.POSIXct(date, format="%Y/%m/%d", tz = "GMT"),
    month = lubridate::month(date, label = TRUE, abbr = TRUE))
#
#
# Add note if it's a business as usual (BAU) or high water table (HWT)
conc_full <- conc_full %>%
  mutate(
    treatment = case_when(
      site == "LC" ~ "BAU",
      site == "SW" ~ "BAU",
      site == "SS-A" ~ "HWT",
      site == "SS-B" ~ "BAU",
      site == "RG-R6" ~ "HWT",
      site == "RG-R8" ~ "BAU",
      site == "WF-A" ~ "HWT",
      site == "WF-B" ~ "BAU",
      site == "TP-A" ~ "HWT",
      site == "TP-B" ~ "BAU",
      site == "GC-A" ~ "HWT",
      site == "GC-B" ~ "BAU",
      site == "LAN" ~ "BAU",
      site == "CHE" ~ "HWT",
      TRUE ~ NA_character_   )  )
#######################################################
#
# Read in GHG flux data (from Mike's excel file calculations)
flux_dat <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Flux calculations/Copy of Fluxes from concs_TS.xlsx", sheet="output")
head(flux_dat)  #152 of 5 vars
#
# Make column for site and ditch
flux_dat <- flux_dat %>%
  mutate( site = str_extract(sample_code, "(?<=^[A-Z]-)[^-\\d]+(?:-[^-\\d]+)*"),
          site = if_else(str_detect(site, "RG-R"),paste0(site, str_extract(sample_code, "(?<=RG-R)\\d+")),site),
          ditch = str_extract(sample_code, "\\d+(\\.\\d+)?$") %>% as.numeric()  )
#
# Set date format
flux_dat <- flux_dat %>%
  mutate(  date = as.POSIXct(date, format="%Y-%m-%d", tz = "GMT"),
           month = lubridate::month(date, label = TRUE, abbr = TRUE))
#
# Load in the ditch DOC data
doc <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/DOC data/LP3_ditch_DOC_all.csv")
head(doc)  # 231 obs of 22 vars
#
# Set date format
doc <- doc %>%
  mutate(  date = as.POSIXct(date, format="%Y-%m-%d", tz = "GMT"))
#
#
#
# Combine concentration, flux data
dat <- conc_full %>%
  left_join(flux_dat, by = c("sample_code", "site", "ditch", "date", "month"))
#
# Now add the DOC data
#
# rename ditch to ditch_rep and make a new column for ditch which excludes the replicate
names(dat)[names(dat) == "ditch"] <- "ditch_rep"
dat$ditch <- as.numeric(sub("\\..*", "", dat$ditch_rep))
# likewise for doc
names(doc)[names(doc) == "ditch"] <- "ditch_rep"
doc$ditch <- as.numeric(sub("\\..*", "", doc$ditch_rep))
#
# Reorder columns
dat <- dat %>%
  select(sample_code, date, month, site, treatment, ditch_rep, ditch, everything())
#
#
# Reformat to aid in join 
doc$site <- as.factor(doc$site)
dat$site <- as.factor(dat$site)
#
dat$month <- factor(dat$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"), ordered = TRUE)
doc$month <- factor(doc$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"), ordered = TRUE)
#
#
# Subset only unique rows of DOC
doc_unique <- doc %>%
  distinct(site, date, ditch, .keep_all = TRUE)
#
# Merge
dat <- dat %>%
  left_join(doc_unique %>% select(site, ditch,  date, DOC_mg_l), 
            by = c("site", "date", "ditch"))   # just keep in mind that there is one DOC value for the ditch, and two GHG points, probably later on you'll want to get the average GHG of each ditch and comapare that to DOC, otherwise you are artificially inflating your sample size I think...
#
str(dat) #152 obs. of  14 variables
#
#

# Save as CSV
write.csv(conc_full, "C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/LP3_GHG_concentrations.csv")
#
#
# Save as CSV
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/LP3_GHG_conc_flux_DOC.csv")
#
#
#### Plots the data for quality control ####
#
#reorder sites for plotting
dat$site <- as.factor(dat$site)
dat$month <- as.factor(dat$month)
dat$treatment <- as.factor(dat$treatment)
dat$site <- factor(dat$site, levels = c("CHE", "LAN", "WF-A", "WF-B" , "RG-R6", "RG-R8", "SS-A", "SS-B", "TP-A" , "TP-B", "GC-A", "GC-B", "LC", "SW" )) 
#
#
#### CO2 ####
#
#tiff("LP3_CO2_mg_l_plot.tiff", units="in", width=6.5, height=4, res=300)
CO2plot <- ggplot(subset(dat, month =="Feb"), aes(x = site, y = CO2_mg_l )) + #change month
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch), shape = as.factor(month)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs( y = expression(CO[2] ~ "mg/l"),  x = NULL, colour = "Ditch #", shape = "Month") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) 
CO2plot
#dev.off()

## Potential outliers ##
#Note we took replicate measurements, when one of these is far from the other--towards atmospheric concentrations, it indicates user error and the value can be dropped

# In December:
# D-RG-R8-3.2 is very low (4.806691) and  for from 3.1
# D-RG-R8-4.2. is also low and far from 4.1 

# In January : all WFA OK

# In February: all OK 


#### CH4 ####
#tiff("LP3_CH4_mg_l_plot.tiff", units="in", width=6.5, height=4, res=300)

CH4plot <- ggplot(subset(dat, month =="Feb"), aes(x = site, y = CH4_ug_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch), shape = as.factor(month)), alpha=0.5, size = 3, width = 0.2) +  # Jitter points to show individual observations
  #scale_y_log10() + #doesn't work with negative values
  scale_y_continuous(trans = 'pseudo_log') +
  labs(y = expression(CH[4] ~ "ug/l"), x = NULL, colour = "Ditch #", shape = "Month") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) 
CH4plot
#dev.off()

# In December: 
#D-RG-R8-3.2 
#D-RG-R8-4.2
# In January: all WFA OK
# In February: all OK


#### N2O ####

#tiff("LP3_N2O_ug_l_plot.tiff", units="in", width=6.5, height=4, res=300)

N2Oplot <- ggplot(subset(dat, month=="Feb"), aes(x = site, y = N2O_ug_l)) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch), shape = as.factor(month)), alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  scale_y_log10() + 
  labs(y = expression(N[2]*O ~ " (µg" ~ L^{-1} ~ ")"),  x = NULL, colour = "Ditch #", shape ="Month") +
  theme_minimal() + # Clean theme
  theme( axis.text.x = element_text(angle = 45, hjust = 1) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) 
N2Oplot
#dev.off()

# In December:
#D-RG-R8-3.2 is outlying
# In January: All OK
# In February : All OK

########################## data QAQC ######################################
# Remove outlying values
#
# CO2 December  D-RG-R8-3.2 , D-RG-R8-4.2
# CH4 December #D-RG-R8-3.2 , D-RG-R8-4.2
# N2O December D-RG-R8-3.2 
#
dat <- dat %>%
  mutate(    CO2_mg_l = case_when(
      sample_code %in% c("D-RG-R8-3.2", "D-RG-R8-4.2") ~ NA_real_,
      TRUE ~ CO2_mg_l),
            CO2_mg_m2_d = case_when(
      sample_code %in% c("D-RG-R8-3.2", "D-RG-R8-4.2") ~ NA_real_,
      TRUE ~ CO2_mg_m2_d) )
#
dat <- dat %>%
  mutate(
    CH4_ug_l = case_when(
      sample_code %in% c("D-RG-R8-3.2", "D-RG-R8-4.2") ~ NA_real_,
      TRUE ~ CH4_ug_l),
    CH4_mg_m2_d = case_when(
      sample_code %in% c("D-RG-R8-3.2", "D-RG-R8-4.2") ~ NA_real_,
      TRUE ~ CH4_mg_m2_d) )
#
dat <- dat %>%
  mutate(
    N2O_ug_l = case_when(
      sample_code %in% c("D-RG-R8-3.2") ~ NA_real_,
      TRUE ~ N2O_ug_l),
    N2O_mg_m2_d = case_when(
      sample_code %in% c("D-RG-R8-3.2") ~ NA_real_,
      TRUE ~ N2O_mg_m2_d) )

#
#
# For reporting and plotting make a new column with CO2 in g
dat$CO2_g_m2_d <- dat$CO2_mg_m2_d/1000
#
#
#
#
##############################################################################
#### Summary stats ####
#
mean(dat$CO2_g_m2_d, na.rm=T)
sd(dat$CO2_g_m2_d, na.rm=T)
min(dat$CO2_g_m2_d, na.rm=T)
max(dat$CO2_g_m2_d, na.rm=T)
sum(!is.na(dat$CO2_g_m2_d))
#
mean(dat$CO2_mg_l, na.rm=T)
sd(dat$CO2_mg_l, na.rm=T)
min(dat$CO2_mg_l, na.rm=T)
max(dat$CO2_mg_l, na.rm=T)
sum(!is.na(dat$CO2_mg_l))
#
mean(dat$CH4_mg_m2_d, na.rm=T)
sd(dat$CH4_mg_m2_d, na.rm=T)
min(dat$CH4_mg_m2_d, na.rm=T)
max(dat$CH4_mg_m2_d, na.rm=T)
sum(!is.na(dat$CH4_mg_m2_d))
#
mean(dat$CH4_ug_l, na.rm=T)
sd(dat$CH4_ug_l, na.rm=T)
min(dat$CH4_ug_l, na.rm=T)
max(dat$CH4_ug_l, na.rm=T)
sum(!is.na(dat$CH4_ug_l))
#
mean(dat$N2O_mg_m2_d, na.rm=T)
sd(dat$N2O_mg_m2_d, na.rm=T)
min(dat$N2O_mg_m2_d, na.rm=T)
max(dat$N2O_mg_m2_d, na.rm=T)
sum(!is.na(dat$N2O_mg_m2_d))
#
mean(dat$N2O_ug_l, na.rm=T)
sd(dat$N2O_ug_l, na.rm=T)
min(dat$N2O_ug_l, na.rm=T)
max(dat$N2O_ug_l, na.rm=T)
sum(!is.na(dat$N2O_ug_l))
##
##
######
# Summary stats by site
summary_stats_ghg <- dat %>%
  group_by(site) %>%
  summarise(
    CO2_g_m2_d_mean = mean(CO2_g_m2_d, na.rm = TRUE),
    CO2_sd = sd(CO2_g_m2_d, na.rm = TRUE),
    CH4_mg_m2_d_mean = mean(CH4_mg_m2_d, na.rm = TRUE),
    CH4_sd = sd(CH4_mg_m2_d, na.rm = TRUE),
    N2O_mg_m2_d_mean = mean(N2O_mg_m2_d, na.rm = TRUE),
    N2O_sd = sd(N2O_mg_m2_d, na.rm = TRUE)   )
#
write.csv(summary_stats_ghg, "C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/LP3_ghgflux_summary_stats.csv")
#
#
summary_stats_conc <- dat %>%
  group_by(site) %>%
  summarise(
    DOC_mg_l_mean = mean(DOC_mg_l, na.rm = TRUE),
    DOC_sd = sd(DOC_mg_l, na.rm = TRUE),
    CO2_mg_l_mean = mean(CO2_mg_l, na.rm = TRUE),
    CO2_sd = sd(CO2_g_m2_d, na.rm = TRUE),
    CH4_ug_l_mean = mean(CH4_ug_l, na.rm = TRUE),
    CH4_sd = sd(CH4_mg_m2_d, na.rm = TRUE),
    N2O_ug_l_mean = mean(N2O_ug_l, na.rm = TRUE),
    N2O_sd = sd(N2O_mg_m2_d, na.rm = TRUE)   )
#
write.csv(summary_stats_conc, "C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/LP3_GHGconc_summary_stats.csv")
#
#
###
#### test for differences between sites (ghg conc and fluxes) ####
#
#
# 1. Test for normality using shapiro wilks test. If the p-value is less than 0.05, you reject the null hypothesis, suggesting the data is not normally distributed.
shapiro_results1 <- sapply(dat[, c(4:6)], shapiro.test) # the majority are non-normal, so go with non-parametric tests
shapiro_results2 <- sapply(dat[, c(14:16)], shapiro.test) # the majority are non-normal, so go with non-parametric tests
#
#
#
# 2. Use Kruskal-Wallis test to see differences between sites. If p<0.05 there is a significant difference between groups
#
#kruskal_results <- sapply(dat[, c(4:11, 13:18, 22:25, 27)], function(x) kruskal.test(x ~ dat$site))
#
kruskal_results_conc <- dat %>%  # for concentration data
  select(c(4:6)) %>%
  map(~ kruskal.test(.x ~ dat$site))
#
kruskal_p_values_conc <- kruskal_results_conc %>%
  map_dbl(~ .x$p.value)   # all significant
#
#
kruskal_results_flux <- dat %>%    # for flux data
  select(c(14:16)) %>%
  map(~ kruskal.test(.x ~ dat$site))
#
kruskal_p_values_flux <- kruskal_results_flux %>%
  map_dbl(~ .x$p.value)   # all significant
#
#
#
# 3. Run pairwise Wilcoxon rank sum test
#
# Identify significant variables
significant_vars_conc <- names(kruskal_p_values_conc)[kruskal_p_values_conc < 0.05]
significant_vars_flux <- names(kruskal_p_values_flux)[kruskal_p_values_flux < 0.05]

# Run pairwise Wilcoxon tests on significant variables
pairwise_results_conc <- dat %>%
  select(all_of(significant_vars_conc)) %>%
  map(~ pairwise.wilcox.test(.x, dat$site, p.adjust.method = "bonferroni", exact = FALSE))
#
pairwise_results_flux <- dat %>%
  select(all_of(significant_vars_flux)) %>%
  map(~ pairwise.wilcox.test(.x, dat$site, p.adjust.method = "bonferroni", exact = FALSE))
#
# print each result by changing the element name within the brackets
pairwise_results_conc[["CO2_mg_l"]] # SS-A and SS-B p = 0.02; TP-A and TB-B p =0.01
pairwise_results_conc[["CH4_ug_l"]] # TP-A and TB-B p =0.02
pairwise_results_conc[["N2O_ug_l"]] # WFA and WFB p=0.0007 ; TP-A and TB-B p =0.01 
#
pairwise_results_flux[["CO2_mg_m2_d"]] # SS-A and SS-B p = 0.03; TP-A and TB-B p =0.01
pairwise_results_flux[["CH4_mg_m2_d"]] # TP-A and TB-B p =0.01
pairwise_results_flux[["N2O_mg_m2_d"]]  # WFA and WFB p=0.009 ; TP-A and TB-B p =0.01 
#
#
#
##### Relationship between DOC and GHG #####################################
#
# Plot
#
DOC_CO2 <- ggplot(dat, aes(x = DOC_mg_l, y = CO2_g_m2_d)) +
  geom_point(fill = "#6497bf", alpha = 0.6, size=3, shape=21) +  
  #geom_smooth(method = "loess", se = F, color = "#e57e73") +
  #geom_smooth(method = "lm", se = F, color = "#e57e73") +  # Linear regression line
  labs(x = expression(DOC ~ "(mg L"^-1*")"),  y = expression(CO[2] ~ "(mg L"^-1*")"))  +
  theme_minimal()  +
  stat_cor(method="spearman", label.sep = "\n", label.x = 50, label.y = 45) +
  theme(axis.title = element_text(size = 14), axis.text.y = element_text(size=12), axis.text.x = element_text(size=12),  panel.grid = element_blank(), panel.border = element_rect(color = "black", fill = NA, linewidth = 1), axis.ticks = element_line(color = "black")  )
DOC_CO2

DOC_CH4 <- ggplot(dat, aes(x = DOC_mg_l, y = CH4_mg_m2_d)) +
  geom_point(fill = "#6497bf", alpha = 0.6, size=3, shape=21) +  
  #geom_smooth(method = "loess", se = F, color = "#e57e73") +
  labs(x = expression(DOC ~ "(mg L"^-1*")"),  y = expression(CH[4] ~ "(µg L"^-1*")") )  +
  theme_minimal()  +
  stat_cor(method="spearman", label.sep = "\n", label.x = 7.2, label.y = 400) +
  theme(axis.title = element_text(size = 14), axis.text.y = element_text(size=12), axis.text.x = element_text(size=12),  panel.grid = element_blank(), panel.border = element_rect(color = "black", fill = NA, linewidth = 1), axis.ticks = element_line(color = "black")  )
DOC_CH4

DOC_N2O <- ggplot(dat, aes(x = DOC_mg_l, y = N2O_mg_m2_d)) +
  geom_point(fill = "#6497bf", alpha = 0.6, size=3, shape=21) +  
  #geom_smooth(method = "loess", se = F, color = "#e57e73") +
  labs(x = expression(DOC ~ "(mg L"^-1*")"),  y = expression(N[2]*O ~ " (µg" ~ L^{-1} ~ ")") )  +
  theme_minimal()  +
  stat_cor(method="spearman", label.sep = "\n", label.x = 50, label.y =240) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1), axis.title = element_text(size = 14), axis.text.y = element_text(size=12), axis.text.x = element_text(size=12),  panel.grid = element_blank(),  axis.ticks = element_line(color = "black")  )
DOC_N2O

## combine ##


jpeg("LP3_ditch_DOC_GHGs.jpg", units="in", width=9, height=3, res=300)

DOC_GHG <- ggarrange(DOC_CO2,DOC_CH4, DOC_N2O,
                          labels = c("A", "B", "C"),
                          ncol = 3, nrow = 1, align="v" )
DOC_GHG

dev.off()

################################################################################
#### PLOTS FOR REPORTING ####
#
#
# Make a new column for month (month2) for plotting in early stages
dat <- dat %>%
  mutate(month2 = case_when(
    month %in% c("Jan", "Feb") ~ "Jan/Feb",
    month == "Dec" ~ "Dec",
    TRUE ~ month  ))
#
#
#
##
###
#### CO2 ####
tiff("LP3_CO2_mg_l_plot_all.tiff", units="in", width=6.5, height=4, res=300)

CO2_conc <- ggplot(dat, aes(x = site, y = CO2_mg_l)) + # Use fill for land use categories
  geom_boxplot(aes(fill = treatment), outlier.shape = NA) + 
  geom_jitter(aes(fill = treatment), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(CO[2] ~ "(mg L"^-1*")"),  x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  theme( axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("#e57e73", "#6497bf")) +
  geom_bracket(xmin = c(5, 7), xmax = c(6, 8),
               y.position = c(150, 150),
               label = c(" ", " "),
               tip.length = 0.01, label.size = 4) +
  annotate("text", x =5.5, y = 151 , label = "*", size=5) +
  annotate("text", x =7.5, y = 151 , label = "*", size=5) 
CO2_conc

dev.off()


CO2_flux <- ggplot(dat, aes(x = site, y = CO2_g_m2_d)) + # Use fill for land use categories
  geom_boxplot(aes(fill = treatment), outlier.shape = NA) + 
  geom_jitter(aes(fill = treatment), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(g~CO[2]*~m^-2*~d^-1), x = NULL, fill = " " ) +
  theme_minimal() + # Clean theme
  theme( axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("#e57e73", "#6497bf")) +
  geom_bracket(xmin = c(5, 7), xmax = c(6, 8),
              y.position = c(35, 35),
               label = c(" ", " "),
               tip.length = 0.01, label.size = 4) +
  annotate("text", x =5.5, y = 35.5 , label = "*", size=5) +
  annotate("text", x =7.5, y = 35.5 , label = "*", size=5) 
CO2_flux

CO2_conc_monthly <- ggplot(dat, aes(x = site, y = CO2_mg_l )) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch), shape = as.factor(month)), alpha=0.5, size = 3, width = 0.2) + 
  scale_shape_manual(values = c("Dec" = 16, "Feb" = 17, "Jan" = 15, "Mar" = 18)) +
  labs( y = expression(CO[2] ~ "(mg L"^-1*")"),   x = NULL, colour = "Ditch #", shape = "Month") +
  theme_minimal() + # Clean theme
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=12) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ,  panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank() , panel.grid.minor = element_blank() ) +
  facet_wrap(~month2)
CO2_conc_monthly

CO2_flux_monthly <- ggplot(dat, aes(x = site, y = CO2_g_m2_d )) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch), shape = as.factor(month)), alpha=0.5, size = 3, width = 0.2) + 
  scale_shape_manual(values = c("Dec" = 16, "Feb" = 17, "Jan" = 15, "Mar" = 18)) +
  labs( y = expression(g~CO[2]*~m^-2*~d^-1),  x = NULL, colour = "Ditch #", shape = "Month") +
  theme_minimal() + # Clean theme
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=12) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ,  panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank() , panel.grid.minor = element_blank() ) +
  facet_wrap(~month2)
CO2_flux_monthly


#### CH4 ####

tiff("LP3_CH4_mg_l_plot_all.tiff", units="in", width=6.5, height=4, res=300)

CH4_conc <- ggplot(dat, aes(x = site, y = CH4_ug_l)) + # Use fill for land use categories
  geom_boxplot(aes(fill = treatment), outlier.shape = NA) + 
  geom_jitter(aes(fill = treatment), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs( y = expression(CH[4] ~ "(µg L"^-1*")"),   x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0.1, 1, 10, 100, 1000), labels = scales::number) +
  theme(  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("#e57e73", "#6497bf")) +
  geom_bracket(xmin = c( 7), xmax = c( 8),
               y.position = c(1),
               label = c(" "),
               tip.length = 0.01, label.size = 4) +
  annotate("text", x =7.5, y = 2.5 , label = "*", size=5) 
CH4_conc

dev.off()


CH4_flux <- ggplot(dat, aes(x = site, y = CH4_mg_m2_d)) + # Use fill for land use categories
  geom_boxplot(aes(fill = treatment), outlier.shape = NA) + 
  geom_jitter(aes(fill = treatment), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs( y = expression(~mg~CH[4]*~m^-2~d^-1),   x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0.1, 1, 10, 100, 1000), labels = scales::number) +
  theme(  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("#e57e73", "#6497bf")) +
  geom_bracket(xmin = c( 7), xmax = c( 8),
               y.position = c(1),
               label = c(" "),
               tip.length = 0.01, label.size = 4) +
  annotate("text", x =7.5, y = 2.5 , label = "*", size=5) 
CH4_flux



CH4_conc_monthly <- ggplot(dat, aes(x = site, y = CH4_ug_l )) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch), shape = as.factor(month)), alpha=0.5, size = 3, width = 0.2) + 
  scale_shape_manual(values = c("Dec" = 16, "Feb" = 17, "Jan" = 15, "Mar" = 18)) +
  labs( y = expression(CH[4] ~ "(µg L"^-1*")"),  x = NULL, colour = "Ditch #", shape = "Month") +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0.1, 1, 10, 100, 1000), labels = scales::number) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=12) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ,  panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank() , panel.grid.minor = element_blank() ) +
  facet_wrap(~month2)
CH4_conc_monthly

CH4_flux_monthly <- ggplot(dat, aes(x = site, y = CH4_mg_m2_d )) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch), shape = as.factor(month)), alpha=0.5, size = 3, width = 0.2) + 
  scale_shape_manual(values = c("Dec" = 16, "Feb" = 17, "Jan" = 15, "Mar" = 18)) +
  labs(  y = expression(~mg~CH[4]*~m^-2~d^-1),  x = NULL, colour = "Ditch #", shape = "Month") +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0.1, 1, 10, 100, 1000), labels = scales::number) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=12) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ,  panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank() , panel.grid.minor = element_blank() ) +
  facet_wrap(~month2)
CH4_flux_monthly

#### N2O ####


tiff("LP3_N2O_mg_l_plot_all.tiff", units="in", width=6.5, height=4, res=300)

N2O_conc <- ggplot(dat, aes(x = site, y = N2O_ug_l)) + # Use fill for land use categories
  geom_boxplot(aes(fill = treatment), outlier.shape = NA) + 
  geom_jitter(aes(fill = treatment), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(N[2]*O ~ " (µg" ~ L^{-1} ~ ")"),   x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  scale_y_log10(labels = scales::label_number(accuracy = 1)) + 
  theme(  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("#e57e73", "#6497bf")) +
  geom_bracket(xmin = c(1, 7), xmax = c(2, 8),
               y.position = c(2, 3.1),
              label = c(" ", " "),
             tip.length = 0.01, label.size = 4) +
  annotate("text", x =1.5, y = 101 , label = "**", size=5) +
  annotate("text", x =7.5, y = 1280 , label = "*", size=5) 
N2O_conc

dev.off()



N2O_flux <- ggplot(dat, aes(x = site, y = N2O_mg_m2_d)) + # Use fill for land use categories
  geom_boxplot(aes(fill = treatment), outlier.shape = NA) + 
  geom_jitter(aes(fill = treatment), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(mg~N[2]*`O`*~m^-2~d^-1),   x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(1, 10, 100), labels = scales::number) +
 # scale_y_log10(labels = scales::label_number(accuracy = 1),  breaks = c(0, 1, 10, 100, 1000)) + 
  theme(  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("#e57e73", "#6497bf")) +
  geom_bracket(xmin = c(1, 7), xmax = c(2, 8),
               y.position = c(5, 6),
               label = c(" ", " "),
               tip.length = 0.01, label.size = 4) +
  annotate("text", x =1.5, y = 150 , label = "**", size=5) +
  annotate("text", x =7.5, y = 405 , label = "*", size=5) 
N2O_flux


N2O_conc_monthly <- ggplot(dat, aes(x = site, y = N2O_ug_l )) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch), shape = as.factor(month)), alpha=0.5, size = 3, width = 0.2) +
  scale_shape_manual(values = c("Dec" = 16, "Feb" = 17, "Jan" = 15, "Mar" = 18)) +
  labs( y = expression(N[2]*O ~ " (µg" ~ L^{-1} ~ ")"), x = NULL, colour = "Ditch #", shape = "Month") +
  theme_minimal() + # Clean theme
  scale_y_log10(labels = scales::label_number(accuracy = 1)) + 
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=12) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ,  panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank() , panel.grid.minor = element_blank() ) +
  facet_wrap(~month2)
N2O_conc_monthly

N2O_flux_monthly <- ggplot(dat, aes(x = site, y = N2O_mg_m2_d )) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch), shape = as.factor(month)), alpha=0.5, size = 3, width = 0.2) + 
  scale_shape_manual(values = c("Dec" = 16, "Feb" = 17, "Jan" = 15, "Mar" = 18)) +
  labs( y = expression(mg~N[2]*`O`*~m^-2~d^-1), x = NULL, colour = "Ditch #", shape = "Month") +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(1, 10, 100), labels = scales::number) +
  theme(axis.title = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=12) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ,  panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank() , panel.grid.minor = element_blank() ) +
  facet_wrap(~month2)
N2O_flux_monthly


#### COMBINE ####
#Combine concentration and flux plots

jpeg("LP3_ditch_GHG_conc_monthly.jpg", units="in", width=10, height=12, res=300)

monthly_conc <- ggarrange(CO2_conc_monthly, CH4_conc_monthly, N2O_conc_monthly,
                       labels = c("A", "B", "C"),
                       ncol = 1, nrow = 3, align="v", common.legend = TRUE, legend = "top" )
monthly_conc

dev.off()
#
#
#
jpeg("LP3_ditch_GHG_flux_monthly.jpg", units="in", width=10, height=12, res=300)

monthly_flux <- ggarrange(CO2_flux_monthly, CH4_flux_monthly, N2O_flux_monthly,
                          labels = c("A", "B", "C"),
                          ncol = 1, nrow = 3, align="v", common.legend = TRUE, legend = "top" )
monthly_flux

dev.off()
#
#
#
jpeg("LP3_ditch_GHG_conc_flux_combined.jpg", units="in", width=10, height=12, res=300)

Conc_flux <- ggarrange(CO2_conc, CO2_flux, CH4_conc, CH4_flux, N2O_conc, N2O_flux,
                              labels = c("A", "B", "C", "D", "E", "F"),
                              ncol = 2, nrow = 3, align="v", common.legend = TRUE, legend = "top" )
Conc_flux

dev.off()

################################################################################
# BACI plot for EGU 2025 presentation
dat <- dat %>%
  mutate(BACI = "before")

dat$month <- factor(dat$month, levels = c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul"))


jpeg("LP3_CO2_conc_BACI.jpg", units="in", width=6, height=4, res=300)

CO2_BACI <- ggplot(dat, aes(x = month, y = CO2_mg_l, colour = treatment)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1, position = position_dodge(width = 0.2)) +  
  stat_summary(aes(group=treatment), fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +
  scale_colour_manual(values = c("#e57e73", "#6497bf")) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    
  labs(y = expression(CO[2] ~ "(mg L"^-1*")"),  x = NULL,   fill = " " ) + 
  scale_x_discrete(drop = FALSE) + 
  theme_minimal() +  theme(legend.position = "top", legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8))  +
  annotate("rect", xmin = 4.25, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "lightgrey", alpha = 0.5) +
  coord_cartesian(ylim = c(0, 200)) +
  annotate("text", x = 5, y = 0, label = "after", size = 5) +
  annotate("text", x = 1.5, y = 0, label = "before", size = 5) 
CO2_BACI

dev.off()




jpeg("LP3_CH4_conc_BACI.jpg", units="in", width=6, height=4, res=300)

CH4_BACI <- ggplot(dat, aes(x = month, y = CH4_ug_l, colour = treatment)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1, position = position_dodge(width = 0.2)) +  
  stat_summary(aes(group=treatment), fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +
  scale_colour_manual(values = c("#e57e73", "#6497bf")) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    
  labs(y = expression(CH[4] ~ "(µg L"^-1*")"),  x = NULL,   fill = " " ) + 
  scale_x_discrete(drop = FALSE) + 
  theme_minimal() +  theme(legend.position = "top", legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8))  +
  annotate("rect", xmin = 4.25, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "lightgrey", alpha = 0.5) +
  coord_cartesian(ylim = c(0, 2000)) +
  annotate("text", x = 5, y = 2000, label = "after", size = 5) +
  annotate("text", x = 1.5, y = 2000, label = "before", size = 5) 
CH4_BACI

dev.off()





jpeg("LP3_N2O_conc_BACI.jpg", units="in", width=6, height=4, res=300)

N2O_BACI <- ggplot(dat, aes(x = month, y = N2O_ug_l, colour = treatment)) +
  stat_summary(fun = mean, geom = "point", size = 3.5, alpha = 0.7, position = position_dodge(width = 0.2)) +  
  stat_summary(fun.data = function(y) mean_se(y),  geom = "errorbar", linewidth = 0.6, alpha = 0.7, width=1, position = position_dodge(width = 0.2)) +  
  stat_summary(aes(group=treatment), fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +
  scale_colour_manual(values = c("#e57e73", "#6497bf")) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +    
  labs(y = expression(N[2]*O ~ "(µg L"^-1*")"),  x = NULL,   fill = " " ) + 
  scale_x_discrete(drop = FALSE) + 
  theme_minimal() +  theme(legend.position = "top", legend.text = element_text(size = 12), panel.grid = element_blank(), axis.line = element_line(), axis.ticks = element_line(), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.title = element_blank(), axis.title = element_text(size = 14),  axis.title.x = element_blank(), panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8))  +
  annotate("rect", xmin = 4.25, xmax = Inf, ymin = -Inf, ymax = Inf, 
           fill = "lightgrey", alpha = 0.5) +
  coord_cartesian(ylim = c(0, 500)) +
  annotate("text", x = 5, y = 500, label = "after", size = 5) +
  annotate("text", x = 1.5, y = 500, label = "before", size = 5) 
N2O_BACI

dev.off()

