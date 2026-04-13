##### XRF analysis on LP3+ peat cores #####
#
#
# Load necessary packages 
library(dplyr)
library(ggplot2)
library(data.table) # for fread
library(stringr)
library(rlang)
library(purrr)
library(patchwork)
library(vegan)
library(Hmisc)
library(corrplot)
library(factoextra)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)
library(geosphere)
library(glmmTMB)
#
#
#
##### NOTES #######
# Note, XRF / LOI data is missing for MOS 84-98
#
#
##########
# Site file directory for figures 
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Figures")
#
#
#
# Read in XRF data
dat <- fread("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/XRF data/XRF_T.Silverthorn 2025-03-26_CorrectedLOI.txt")
#
head(dat)
#
# Make column names based on the first two rows (element, units)
# And make any other adjustments to the column names
#
dat <- dat %>%
  {setNames(., paste(.[1, ], .[2, ], sep = "_"))} %>%        # Extract the first two rows for elements and units
  rename_with(~ gsub("Netto Counts", "netto_counts", .)) %>% 
  rename_with(~ gsub("/", "_", .)) %>%
  rename_with(~ gsub("%", "percent", .)) %>%
  rename_with(~ gsub("\\?", "u", .)) %>%      # Double check with jenny if the ? is really micro
  rename_with(~ gsub("_$", "", .)) %>%        # Remove trailing underscores
  rename(sample_code = "Element_Dimension") %>%
  mutate(site = str_extract(sample_code, "(?<=LP3\\+\\s)[A-Za-z0-9]{2,}(?=\\d|_|$)"))  %>%   # add a column for site
  mutate(depth_cm = as.numeric(str_extract(sample_code, "(?<=_)[0-9]+(?=$)"))) %>%      # add a column for depth
  slice(-1:-2) %>%                                               # Remove the first two columns
  select(sample_code, site, depth_cm, everything())
#
#
# Make cases below detection limits 0's
dat <- dat %>%
  mutate(across(6:77, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  
    TRUE ~ as.character(.))))) #NAs introduced error is expected
#
#
# Add a column for land use
#
dat$site <- as.factor(dat$site) # make site a factor
#
dat <- dat %>%
  mutate(land_use = case_when(
    site %in% c("LC", "WFA", "RGR6") ~ "Conventional arable",
    site %in% c("SW") ~ "Regenerative arable",
    site %in% c("ND", "MF", "WBF", "RV", "MOS") ~ "Grassland",
    site %in% c("HF", "WW", "WSF" ) ~ "Semi-natural fen",
    site %in% c("BM" ) ~ "Rewetted bog",   # Note from Mike this update not semi-natural, but rewetted
    TRUE ~ "Other"  # Assign "Other" to all other sites (modify as needed)
  )) %>%                                               # Remove the first two columns
  select(sample_code, site, land_use, depth_cm, everything())
#
#
#
# Get maximum depth per site
max_depth_per_site <- dat %>%
  group_by(site) %>%
  dplyr::summarize(max_depth = max(depth_cm, na.rm = TRUE))
#
#
dat$site <- factor(dat$site, levels = c("BM", "WSF", "WW", "HF", "SW", "LC", "RGR6", "WFA", "MF", "MOS", "ND", "RV", "WBF"))
#
#
# Correct error in WW, delete the _ after WW
dat <- dat %>%
  mutate(sample_code = str_replace(sample_code, "WW_", "WW"))
#
dat <- dat %>%
  mutate(sample_code = str_replace(sample_code, "MF_", "MF"))
#
# Get rid of duplicates with "(1)"
dat <- dat %>% 
  filter(!str_detect(sample_code, "\\(1\\)"))  # Remove rows where sample_code includes (1)
#717-628 #lost 89 rows
#
#
########
#
#### Read in the LOI data ####
dat_LOI <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/LOI/LOI_all_data_combined_2025-03-31.csv")
#
head(dat_LOI)  #625 obs
#
# Rename id
colnames(dat_LOI)[colnames(dat_LOI) == "id"] <- "sample_code"
#
# Adjust to match dat
dat_LOI$sample_code <- paste("LP3+ ", dat_LOI$sample_code, sep = "")
#
# add LOI data to dat # left join excludes the HF data where we have LOI but no XRF
dat <- dat %>%
  full_join(dat_LOI, by = "sample_code") %>%
  select(sample_code, site, land_use, depth_cm, percent_loi, percent_moisture, everything()) 
#
# For sites that have LOI data but no matching XRF data, we need to add site, land use and depth
dat <- dat %>%
  mutate(
    site = if_else(is.na(site) & str_detect(sample_code, "MOS"), "MOS", site),
    land_use = if_else(site == "MOS" & is.na(land_use), "Grassland", land_use),
    depth_cm = if_else(site == "MOS" & is.na(depth_cm), 
                       as.numeric(str_extract(sample_code, "[^_]+$")), 
                       depth_cm)  )
#
#
# Clean LOI values: remove negatives and large values
#
dat <- dat %>%
  mutate(percent_loi = case_when(
    percent_loi < 0 ~ NA_real_,
    percent_loi > 1000 ~ NA_real_,
    percent_loi > 100 ~ 100,
    TRUE ~ percent_loi
  ))
#
#
# Based on visual inspection and comparison with XRF computed values, remove a couple other points
#
dat <- dat %>%
  mutate(percent_loi = ifelse(
    (site == "WW" & percent_loi < 50) |
      (site == "HF" & percent_loi < 30) |
      (site == "LC" & percent_loi > 75) |
      (site == "WBF" & percent_loi > 75) |
      (site == "RGR6" & percent_loi < 25) |
      (site == "WSF" & depth_cm < 50 & percent_loi < 40) |
      (site == "RV" & depth_cm > 25 & percent_loi < 25),
    NA, 
    percent_loi
  ))
#
#
## Based on visual inspection, remove some impossible soil moisture data points <0 and >100, as well as some other data cleaning of individual outliers
#
dat <- dat %>%
  mutate(percent_moisture = if_else(percent_moisture < 0 | percent_moisture > 100 | 
                                      (site %in% c("HF", "RGR6") & percent_moisture < 50) | 
                                      (site == "SW" & percent_moisture < 20)| 
                                      (site == "WW" & percent_moisture < 60),
                                    NA_real_, percent_moisture))


# Rename some sites for the report to align with water chemistry sampling
dat$site[dat$site == "BM"] <- "DEL"
dat$site[dat$site == "RGR6"] <- "RG"
dat$site[dat$site == "WFA"] <- "WF"
dat$site[dat$site == "MOS"] <- "MS"
#
# Remove regenerative
dat$land_use[dat$land_use == "Regenerative arable"] <- "Arable"
dat$land_use[dat$land_use == "Conventional arable"] <- "Arable"
dat$land_use[dat$land_use == "Semi-natural fen"] <- "Semi-natural"
dat$land_use[dat$land_use == "Rewetted bog"] <- "Rewetted"

#
#
# Reorder site levels
dat$site <- factor(dat$site, levels = c( "WSF", "WW", "HF","DEL", "SW", "LC", "RG", "WF", "MF", "MS", "ND", "RV", "WBF"))
#
# Reorder land use levels
dat$land_use <- factor(dat$land_use, levels = c( "Semi-natural", "Rewetted", "Arable",  "Grassland" ))
#
# There is something going on with BM - 4 cm : Fran calculated 100% LOI and the XRF 0... causing some issues, so delete
dat <- subset(dat, sample_code != "LP3+ BM1.1_4")
# Also HF 1.1 - 22 cm, Fran calculated 93 white the XRF 0.....
dat <- subset(dat, sample_code != "LP3+ HF1.1_22")
#
#
#### Make a new column for peat type ####
dat <- dat %>%
  mutate(peat = case_when(
    site == "RV" ~ "Bog",
    site == "MS" ~ "Bog",
    site == "DEL" ~ "Bog",
    site == "WF" ~ "Bog",
    site == "HF" ~ "Bog",
    site == "LC" ~ "Fen",
    site == "RG" ~ "Fen",
    site == "SW" ~ "Fen",
    site == "MF" ~ "Fen",
    site == "HF" ~ "Fen",
    site == "ND" ~ "Fen",
    site == "WW" ~ "Fen",
    site == "WBF" ~ "Fen",
    site == "WSF" ~ "Fen",
    TRUE ~ NA_character_   )) %>%
  select(sample_code, site, land_use, peat, depth_cm, everything())
#
#
#### Make a new column for total heavy metals (no Fe since it's in units of mg) ####
dat$total_metals_ug_g <- dat$Zn_ug_g + dat$Pb_ug_g + dat$Hg_ug_g + dat$Cu_ug_g
#
#
# Make new column for Ca:Mg molar ratio, which indicates ombrotrophic conditions if <1
dat$Ca_Mg <- (dat$Ca_mg_g/40.078) / (dat$Mg_mg_g/24.305) 
#
#
# add the coordinates
coords <- read.csv(  "C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Fieldwork/LP3+ peat coring site codes 2026-04-13.csv", fileEncoding = "Windows-1252" ) %>%
  select(site, lat, lon)
#
dat <- dat %>%
  left_join(coords, by = "site")
#
## Save as csv
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/LP3+peatcore_XRF_LOI_clean.csv")
#
#

################################################################################
#### Check LOI ####
# Plot our LOIs by the XRF LOIs
LOI <- ggplot (dat, aes(y=percent_loi, x=LOI_percent, colour=site))+ geom_point()
LOI
#
####
# Summary statistics - other LOI info
summary_loi_by_site <- dat %>% # mean and min LOI by site
  group_by(site) %>%
  summarise(
    min_percent_loi = min(percent_loi, na.rm = TRUE),
    mean_percent_loi = mean(percent_loi, na.rm = TRUE),
  )
#
#
#### plot LOI ####
#
#
tiff("LOI_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
LOI_all <- ggplot(subset(dat, !is.na(site)), aes(y = percent_loi, x = depth_cm, shape=peat)) +
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
    geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  geom_vline(data = subset(dat, site == "WSF") %>% distinct(site), aes(xintercept = 65), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "WF") %>% distinct(site), aes(xintercept = 50), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "WBF") %>% distinct(site), aes(xintercept = 10), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "ND") %>% distinct(site), aes(xintercept = 91), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  scale_shape_manual(values=c( 21, 19)) +
   labs(y = "LOI (%)", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  scale_y_continuous(breaks = seq(0, 100, by = 50)) +
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"), legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Arable" = "#D8B4F8",  "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
LOI_all
#
dev.off()
#

#
#
#### Plot % moisture ####
#

tiff("moisture_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
moisture_all <- ggplot(subset(dat, !is.na(site)), aes(y = percent_moisture, x = depth_cm, shape=peat)) +
geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Moisture (%)", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  scale_y_continuous(breaks = seq(0, 100, by = 50)) +
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"), legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
moisture_all
#
dev.off()
#
##### Plots ##################################
#
#
#
#### Fe, Cu, Zn and Pb, are indicative of industrial pollution (Turner et al 2014) ####
#
#
# Test plot Pb for just one site
Pb_MOS <- ggplot(subset(dat, site=="MOS"), aes(y = Pb_ug_g, x = depth_cm)) +
  geom_point() +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  #labs(x = "Pb (ug/g)", y = "Depth (cm)") +
  scale_x_reverse() + 
  coord_flip() +
  theme_minimal() 
Pb_MOS
#
#
#
#### total heavy metals ####
#
#
tiff("Total_heavy_metals_LP3+_peat_cores_4000.tiff", units="in", width=8, height=4.5, res=300)
#
# 
total_metals <- ggplot(subset(dat, !is.na(site) & site != "RV"),  aes(y = total_metals_ug_g, x = depth_cm, shape=peat)) +
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  geom_vline(data = subset(dat, site == "WSF") %>% distinct(site), aes(xintercept = 65), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "WF") %>% distinct(site), aes(xintercept = 50), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "WBF") %>% distinct(site), aes(xintercept = 10), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "ND") %>% distinct(site), aes(xintercept = 91), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Total metals", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  #scale_y_log10() +
  scale_y_continuous(breaks = seq(0, 4000, by = 2000)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
total_metals
#
dev.off()

#### Pb plot for all sites ####
#
tiff("Pb_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
Pb_all <- ggplot(subset(dat, !is.na(site)),  aes(y = Pb_ug_g, x = depth_cm, shape=peat)) +
geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Pb", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 4400, by = 2000)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Pb_all
#
dev.off()

sum(dat$Pb_ug_g > 300, na.rm = TRUE) 
Pb_dat <- subset(dat, Pb_ug_g > 300)
#
#
#
#### Zn plot for all sites ####
#
tiff("Zn_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
Zn_all <- ggplot(subset(dat, !is.na(site)), aes(y = Zn_ug_g, x = depth_cm, shape=peat)) +
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Zn", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 2000, by = 1000)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Zn_all
#
dev.off()
#
#
sum(dat$Zn_ug_g > 52, na.rm = TRUE) 
Zn_dat <- subset(dat, Zn_ug_g > 52)

#
#### Fe plot for all sites ####
#
tiff("Fe_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
Fe_all <- ggplot(subset(dat, !is.na(site)), aes(y = Fe_mg_g, x = depth_cm, shape=peat)) +
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Fe", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=6, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Fe_all
#
dev.off()
#
#
#
#### Cu plot for all sites ####
#
tiff("Cu_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
Cu_all <- ggplot(subset(dat, !is.na(site)), aes(y = Cu_ug_g, x = depth_cm, shape=peat)) +
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Cu", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Cu_all
#
dev.off()
#
#
#
#### Hg plot for all sites ####
#
tiff("Hg_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
Hg_all <- ggplot(subset(dat, !is.na(site)), aes(y = Hg_ug_g, x = depth_cm, shape=peat)) +
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Hg", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=6.5, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Hg_all
#
dev.off()
#

sum(dat$Hg_ug_g > 1, na.rm = TRUE)  # what number of samples above 1 ug/g (EC directive limit)
Hg_dat <- subset(dat, Hg_ug_g > 1)

unique(Hg_dat$site)
unique(Pb_dat$site)
unique(Zn_dat$site)

################################################################################
#
#
#### Elements P and K are associated with agricultural fertilizers (Turner et al 2014) ####
#
#
#### P plot for all sites ####
#
tiff("P_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
P_all <- ggplot(subset(dat, !is.na(site)), aes(y = P_mg_g, x = depth_cm, shape=peat)) +
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  geom_vline(data = subset(dat, site == "WSF") %>% distinct(site), aes(xintercept = 65), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "WF") %>% distinct(site), aes(xintercept = 50), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "WBF") %>% distinct(site), aes(xintercept = 10), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "ND") %>% distinct(site), aes(xintercept = 91), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "P", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
P_all
#
dev.off()
#
#
#
##### K plot for all sites ####
#
tiff("K_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
K_all <- ggplot(subset(dat, !is.na(site)), aes(y = K_mg_g, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  geom_vline(data = subset(dat, site == "WSF") %>% distinct(site), aes(xintercept = 65), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "WF") %>% distinct(site), aes(xintercept = 50), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "WBF") %>% distinct(site), aes(xintercept = 10), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  geom_vline(data = subset(dat, site == "ND") %>% distinct(site), aes(xintercept = 91), colour = "black",linetype = "dashed", linewidth = 0.9 ) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "K", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 30, by = 15)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
    scale_fill_manual(values = c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
K_all
#
dev.off()
#
#
#

#
###############################################################################
#
#### Br - bromine, a halogen which is retained stratigraphically in accumulating peat (RoosBarraclough et al., 2002) ####
#
#
tiff("Br_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
Br_all <- ggplot(subset(dat, !is.na(site)), aes(y = Br_ug_g, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Br", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 6000, by = 3000)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Br_all
#
dev.off()
#
#

#
##################################################################################
#
#### Na ####
#
tiff("Na_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
Na_all <- ggplot(subset(dat, !is.na(site)), aes(y = Na_mg_g, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Na", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 125, by = 50)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Na_all
#
dev.off()
#
#
##################################################################################
#
#### Cl ####
#
tiff("Cl_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
Cl_all <- ggplot(subset(dat, !is.na(site)), aes(y = (Cl_ug_g)/1000, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Cl", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 60, by = 30)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Cl_all
#
dev.off()
#
#
#
#### Ti - the soil erosion indicator (Holzer and Holzer, 1998) ####
#
#
tiff("Ti_LP3+_peat_cores_all.tiff", units="in",  width=8, height=4.5, res=300)
#
Ti_all <- ggplot(subset(dat, !is.na(site)), aes(y = Ti_ug_g, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Ti", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Ti_all
#
dev.off()
#
#
##################################################################################
#
#### Si - human impact indicator? ####
#
#
tiff("Si_LP3+_peat_cores_all.tiff", units="in",  width=8, height=4.5, res=300)
#
Si_all <- ggplot(subset(dat, !is.na(site)), aes(y = Si_mg_g, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Si", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 400, by = 200)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Si_all
#
dev.off()
#
#
##################################################################################
#
#### Y - indicates dust loading ####
#
max(subset(dat, site == "WBF")$Y_ug_g, na.rm = TRUE)
#
tiff("Y_LP3+_peat_cores_all.tiff", units="in",  width=8, height=4.5, res=300)
#
Y_all <- ggplot(subset(dat, !is.na(site)), aes(y = Y_ug_g, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Y", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  #scale_y_continuous(breaks = seq(0, 400, by = 200)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Y_all
#
dev.off()
#
#
##################################################################################
#
#### Rb - indicates dust loading ####
#
dat <- dat %>% 
  mutate( Cu_Rb = Cu_ug_g /Rb_ug_g , 
          Cu_Rb = ifelse(is.finite(Cu_Rb), Cu_Rb, NA))

dat <- dat %>% 
  mutate( Fe_Rb = Fe_mg_g*1000 / Rb_ug_g, 
          Fe_Rb = ifelse(is.finite(Fe_Rb), Fe_Rb, NA))

dat <- dat %>%
  mutate(Pb_Rb = Pb_ug_g / Rb_ug_g,
         Pb_Rb = ifelse(is.finite(Pb_Rb), Pb_Rb, NA))

dat <- dat %>%
  mutate(As_Rb = As_ug_g / Rb_ug_g,
         As_Rb = ifelse(is.finite(As_Rb), As_Rb, NA))

dat <- dat %>%
  mutate(Zn_Rb = Zn_ug_g / Rb_ug_g,
         Zn_Rb = ifelse(is.finite(Zn_Rb), Zn_Rb, NA))

#
#
tiff("Rb_LP3+_peat_cores_all.tiff", units="in",  width=8, height=4.5, res=300)
#
Rb_all <- ggplot(subset(dat, !is.na(site)), aes(y = Rb_ug_g, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Rb", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 250, by = 100)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Rb_all
#
dev.off()
#
#
tiff("ZnRb_LP3+_peat_cores_all.tiff", units="in",  width=8, height=4.5, res=300)

Zn_Rb_plot <- ggplot(subset(dat, !is.na(site)), aes(y = Zn_Rb, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Zn:Rb", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 400, by = 200)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values =  c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4")  , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Zn_Rb_plot

dev.off()


tiff("CuRb_LP3+_peat_cores_all.tiff", units="in",  width=8, height=4.5, res=300)

Cu_Rb_plot <- ggplot(subset(dat, !is.na(site)), aes(y = Cu_Rb, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Cu:Rb", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 100, by = 50)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values =  c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4")  , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Cu_Rb_plot

dev.off()


tiff("PbRb_LP3+_peat_cores_all.tiff", units="in",  width=8, height=4.5, res=300)

Pb_Rb_plot <- ggplot(subset(dat, !is.na(site)), aes(y = Pb_Rb, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Pb:Rb", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous( breaks = seq(0, 400, by = 200)) + 
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values =  c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4")  , guide = guide_legend(override.aes = list(alpha = 1)) ) # override alpha 1 for legend 
Pb_Rb_plot

dev.off()

As_Rb_plot <- ggplot(subset(dat, !is.na(site)), aes(y = As_Rb, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "As:Rb", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  #scale_y_continuous( breaks = seq(0, 500, by = 250)) + 
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values =  c("Arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural" ="#B5E48C", "Rewetted" = "#A5F2D4")  , guide = guide_legend(override.aes = list(alpha = 1)) ) # override alpha 1 for legend 
As_Rb_plot


##################################################################################
#
#### Ca:Mg - indicates ombrotrophic conditions when <1 ####
#
tiff("Ca_Mg_LP3+_peat_cores_all.tiff", units="in", width=8, height=4.5, res=300)
#
Ca_Mg <- ggplot(subset(dat, !is.na(site)), aes(y = Ca_Mg, x = depth_cm, shape=peat)) + 
  geom_rect( data = subset(dat, !is.na(site)) %>% distinct(site, land_use),  aes(fill = land_use),     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.8,  inherit.aes = FALSE ) +
  geom_point(size=1.5, alpha=0.7) +   geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red", linewidth=0.4, alpha=0.5) +
  geom_hline(yintercept = 1, colour="red", size=0.65, linetype="dashed") +
  scale_shape_manual(values=c( 21, 19)) +
  labs(y = "Ca:Mg", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  scale_y_continuous(breaks = seq(0, 50, by = 15)) +
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 1) +  # Create a plot for each site
  theme_minimal() +   theme(legend.text=element_text(size=7.5), panel.grid.major = element_line(color = "black"),   legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(size=7, angle = 45, hjust = 1)) +   
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") , guide = guide_legend(override.aes = list(alpha = 1)) )  # override alpha 1 for legend
Ca_Mg
#
dev.off()
#
#
ombro <- subset(dat, Ca_Mg <1)

#################################################################################
#### Comparing to ambient background concentrations ####

pb_exceed <- dat %>%
  filter(!is.na(site), !is.na(Pb_ug_g)) %>%
  group_by(site) %>%
  summarise(
    n_exceed = sum(Pb_ug_g > 301),
    n_total = n(),
    prop_exceed = n_exceed / n_total   )

hg_exceed <- dat %>%
  filter(!is.na(site), !is.na(Hg_ug_g)) %>%
  group_by(site) %>%
  summarise(
    n_exceed = sum(Hg_ug_g > 1.1),
    n_total = n(),
    prop_exceed = n_exceed / n_total   )

cu_exceed <- dat %>%
  filter(!is.na(site), !is.na(Cu_ug_g)) %>%
  group_by(site) %>%
  summarise(
    n_exceed = sum(Cu_ug_g > 16),
    n_total = n(),
    prop_exceed = n_exceed / n_total   )

zn_exceed <- dat %>%
  filter(!is.na(site), !is.na(Zn_ug_g)) %>%
  group_by(site) %>%
  summarise(
    n_exceed = sum(Zn_ug_g > 53),
    n_total = n(),
    prop_exceed = n_exceed / n_total   )

#################################################################################
#### Plots by site ####
#
# Loop to create plot
create_element_plot <- function(element_col, title, site_name) {
  
  ggplot(subset(dat, site == site_name & !is.na(site)), aes_string(y = element_col, x = "depth_cm")) + 
    geom_point( size = 1.5, alpha = 0.7) +   
    geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") +
    labs(x = if(element_col %in% c("percent_loi")) "Depth (cm)" else "", 
         title = paste(title)) + #choose when to show y axis label
    scale_x_reverse() + coord_flip() + theme_minimal() +   
    theme( panel.grid.major.y = element_line(color = "grey"),  panel.grid.major.x = element_line(color = "grey"), 
          panel.grid.minor = element_blank(), axis.title.x = element_blank(), 
          panel.border = element_blank(), axis.line.x = element_line(color = "black"), axis.line.y = element_line(color = "black"), plot.title = element_text(hjust = 0.5),
          axis.ticks.x = element_line(), axis.ticks.y = element_line(), 
          axis.text.y = if (element_col == "percent_loi") element_text() else element_blank(), 
          axis.text.x = element_text(angle = 45, hjust = 1, size=6.5))
}

elements <- c("percent_loi",  "Cu_ug_g","Fe_mg_g", "Pb_ug_g", "Hg_ug_g",  "Zn_ug_g", "K_mg_g", "P_mg_g", "Ca_Mg")

titles <- c( "LOI", "Cu", "Fe", "Pb", "Hg", "Zn", "K", "P",  "Ca:Mg")



# Get unique sites
sites <- unique(dat$site)

# Loop over sites to create, display, and save plots
for (site_name in sites) {
  
  # Create plots for this site
  site_plots <- purrr::map2(elements, titles, ~ create_element_plot(.x, .y, site_name))
  
  # Combine with patchwork
  combined_plot <- wrap_plots(site_plots, nrow = 1) +
    plot_layout(
      nrow = 1,
      guides = "collect",
      widths = rep(1, length(site_plots)),
      design = NULL
    ) &
    theme(plot.margin = margin(1, 1, 1, 1)) &
    plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))
  
  # Save each plot as TIFF with site name in filename
  tiff_filename <- paste0("LP3+_peat_core_", site_name, ".tiff")
  tiff(tiff_filename, units = "in", width = 10, height = 6, res = 300)
  print(combined_plot)
  dev.off()
  
  # Optionally print progress
  message("Saved plot for site: ", site_name)
}

#################################################################################
##### Run PCA ####
#
# Select numerical columns (excluding variables with NAs and Netto columns)
dat_subset <- dat %>% select(-contains("netto"))
#
# Also exclude columns with a lot of NAs
#na_percent <- colSums(is.na(dat_subset)) / nrow(dat_subset) * 100
#na_percent_df <- data.frame(Column = names(na_percent), NA_Percentage = na_percent) # Exclude these elements which have high numbers of NAs: NA_NA, Ag_ug_g, Au_ug_g, In_ug_g, Hg_ug_g, Cs_ug_g, La_ug_g, Cd_ug_g, U_ug_g, Co_ug_g, Ce_ug_g, Hf_ug_g, W_ug_g, Te_ug_g, Sb_ug_g, Ge_ug_g, I_ug_g, I_ug_g, Na_mg_g, Na2O_mg_g, Mo_ug_g, Ba_ug_g, Sn_ug_g, Ga_ug_g, Rb_ug_g, Mg_mg_g, Se_ug_g, Cr_ug_g, Zr_ug_g
#
#
dat_subset <- dat_subset %>% select(  site, peat, land_use, percent_loi ,  Cu_ug_g , Fe_mg_g ,  Pb_ug_g ,  Hg_ug_g ,   Zn_ug_g ,  K_mg_g ,  P_mg_g )
#
#
which(is.na(dat_subset), arr.ind = TRUE) # where are the NAs
dat_subset <- dat_subset[!is.na(dat_subset$percent_loi), ] # get rid of rows with NAs
dat_subset <- dat_subset[!is.na(dat_subset$site), ] # get rid of rows with NAs
dat_subset <- dat_subset[!is.na(dat_subset$Cu_ug_g), ] # get rid of rows with NAs

# Also subset other useless columns and non-numeric columns
dat_num <- dat_subset %>% select(percent_loi ,  Cu_ug_g , Fe_mg_g ,  Pb_ug_g ,  Hg_ug_g ,   Zn_ug_g ,  K_mg_g ,  P_mg_g )
#
#
# There are just a couple NA values, fill with mean (2 for Nb_ug_g, 2 for Zn_ug_g, 2 for Pb_ug_g)
dat_num$Zn_ug_g[is.na(dat_num$Zn_ug_g)] <- mean(dat_num$Zn_ug_g, na.rm = TRUE)
dat_num$Pb_ug_g[is.na(dat_num$Pb_ug_g)] <- mean(dat_num$Pb_ug_g, na.rm = TRUE)
#
#
# Remove units from column names
colnames(dat_num) <- gsub("_ug_g|_mg_g", "", colnames(dat_num))
dat_num <- dat_num %>% rename(LOI = percent_loi) #rename LOI
#
# Run PCA (standardizing the data)
pca_result <- prcomp(dat_num, center = TRUE, scale. = TRUE)
#
# View summary statistics
summary(pca_result)
#
# Scree plot showing variance explained by each PC
variance <- fviz_eig(pca_result, addlabels = TRUE, barfill = "steelblue", barcolor = "black") +  labs(title = NULL) + theme(panel.grid = element_blank(), axis.line = element_line(colour = "black") ) +  coord_cartesian(ylim = c(0, 75)) +  labs( x = "Dimensions", y = "% explained variance", colour = NULL ) 
#
# Scree plot showing eigenvalues
eigen <- fviz_eig(pca_result, choice = "eigenvalue", addlabels = TRUE, barfill = "steelblue", barcolor = "black") +  labs(title = NULL) +theme(panel.grid = element_blank(), axis.line = element_line(colour = "black") ) +  coord_cartesian(ylim = c(0, 6))



# Eigenvalues
eig_vals <- pca_result$sdev^2
p <- length(eig_vals)

# Broken-stick model
broken_stick <- sapply(1:p, function(k) {
  sum(1 / (k:p)) / p
})

# Convert to same scale as eigenvalues
broken_stick <- broken_stick * sum(eig_vals)

# Plot scree
plot(eig_vals, type = "b", pch = 19,
     xlab = "Principal Component",
     ylab = "Eigenvalue",
     main = "Scree Plot with Broken-Stick Threshold")

# Add broken-stick curve
lines(broken_stick, col = "red", lwd = 2)

legend("topright", legend = c("Eigenvalues", "Broken-stick"),
       col = c("black", "red"), lty = 1, pch = c(19, NA))
#
#
# ggplot version (Rich's is in base R)
#
dat_num <- data.frame(
  PC = 1:length(eig_vals),
  Eigenvalue = eig_vals,
  BrokenStick = broken_stick)
#
# ggplot scree plot
broken_stick <- ggplot(dat_num, aes(x = PC)) +
  geom_line(aes(y = Eigenvalue, colour = "Eigenvalues"), linewidth = 1) +
  geom_point(aes(y = Eigenvalue, colour = "Eigenvalues"), size = 2) +
  geom_line(aes(y = BrokenStick, colour = "Broken-stick"), linewidth = 1) +
  scale_colour_manual(values = c("Eigenvalues" = "black", "Broken-stick" = "red")) +
  labs( x = "Dimensions", y = "Eigenvalue", colour = NULL ) +
  theme_minimal()  +  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black"), legend.position = c(0.9, 0.9) ) +  scale_x_continuous(breaks = seq_along(pca_result$sdev))
broken_stick
#
#
#
#
#combine variance and eigen plots
jpeg("PCA_scree_plot2.jpg", units="in", width=7, height=8, res=300)
ggarrange(variance, eigen, broken_stick, labels = c("A", "B", "C"),   ncol = 1, nrow = 3)
dev.off()
#
#
#combine variance and eigen plots
jpeg("PCA_scree_plot.jpg", units="in", width=7, height=8, res=300)
ggarrange(variance, eigen, labels = c("A", "B"),   ncol = 1, nrow = 2)
dev.off()
#
# Loadings (variables × PCs)
loadings <- pca_result$rotation
#
# Variance explained
var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
#
# Combine into a table
loadings_df <- as.data.frame(loadings)
loadings_df$Variable <- rownames(loadings_df)
#
write.csv(loadings_df, "PCA_loadings.csv", row.names = FALSE)
#
#############################
#### code from Rich ####
#The eigenvalues expected  under a random model (Broken Stick) are optionally plotted - eigenvalues under this curve may  represent non-significant components (Jackson 1993)." This can highlight the number of components needed to account for variation, example code for scree plot with Broken stick threshold below.... and happy to be told I know and done that already...... 🙂
#
#PCA prcomp is great actually based on Single Value Decomp - so yes only z-score standardised once....

# PCA
data(iris)
iris_numeric <- iris[, -5]

pca <- prcomp(iris_numeric, center = TRUE, scale. = TRUE)

# Eigenvalues
eig_vals <- pca$sdev^2
p <- length(eig_vals)

# Broken-stick model
broken_stick <- sapply(1:p, function(k) {
  sum(1 / (k:p)) / p
})

# Convert to same scale as eigenvalues
broken_stick <- broken_stick * sum(eig_vals)

# Plot scree
plot(eig_vals, type = "b", pch = 19,
     xlab = "Principal Component",
     ylab = "Eigenvalue",
     main = "Scree Plot with Broken-Stick Threshold")

# Add broken-stick curve
lines(broken_stick, col = "red", lwd = 2)

legend("topright", legend = c("Eigenvalues", "Broken-stick"),
       col = c("black", "red"), lty = 1, pch = c(19, NA))
#############
#
#
# Visualize the PCA with site or land-use as a grouping factor
dat_subset$land_use <- as.factor(dat_subset$land_use)
dat_subset$site <- as.factor(dat_subset$site)
levels(dat_subset$land_use)
#
#
my.col.var <- c(  "Arable" = "#9B59B6", "Grassland" = "#E0B800",  "Semi-natural" = "#4CAF50",  "Rewetted" = "#62e8b4"  )
#
#
#
jpeg("LP3+_PCA_peat_core.jpg", units="in", width=8, height=6, res=300)

PCA_land_use <- fviz_pca_biplot(pca_result, 
                           col.ind = dat_subset$land_use,
                           addEllipses = TRUE, label = "var",
                           pointsize=3,
                           alpha.ind=0.9,
                           mean.point=F,
                           palette=my.col.var,
                           col.var = "black", repel = TRUE,
                           legend.title = " ") + ggtitle(NULL) +  
                           scale_shape_manual(values = c( 2, 4, 3, 7)) +
                           xlab("PC1 (63.8%)") +   ylab("PC2 (24.9%)") + #update these values if you change pca
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.text = element_text(color = "black"), axis.title = element_text(color = "black"))
PCA_land_use

dev.off()
#

#tiff("PCA_LP3+.tiff", units="in", width=8, height=6, res=300)
PCA_peat <- fviz_pca_biplot(pca_result, 
                           col.ind = dat_subset$peat,
                           addEllipses = TRUE, label = "var",
                           pointsize=3,
                           alpha.ind=0.25,
                           mean.point=F,
                           col.var = "black", repel = TRUE,
                           legend.title = " ") + ggtitle(NULL) 
PCA_peat
#dev.off()

PCA_site <- fviz_pca_biplot(pca_result, 
                            col.ind = dat_subset$site,
                            addEllipses = TRUE, label = "var",
                            pointsize=3,
                            alpha.ind=0.25,
                            mean.point=F,
                            col.var = "black", repel = TRUE,
                            legend.title = " ") + ggtitle(NULL) 
PCA_site

################################################################################
#### NMDS ####
#
#NMDS allows us to better handle non-normal data, non-linear relationships but also things like missing and null data (which PCA cannot handle).# See: https://jkzorz.github.io/2019/06/06/NMDS.html
#
# #turn data frame into a matrix
m_dat_num <- as.matrix(dat_num)
#
nmds <- metaMDS(m_dat_num, distance = "bray")
nmds
#
#
################################################################################
#### Correlation plot ####
#
# Subset numeric columns
dat_numeric <- select(dat, where(is.numeric))
#
# Drop rows with all or many NAs
dat_numeric <- dat_numeric %>% filter(!if_all(everything(), is.na)) %>%
  filter(!is.na(depth_cm)) %>%
  filter(!is.na(percent_loi)) %>%
  filter(!is.na(LOI_percent)) %>%
  filter(!is.na( percent_moisture)) %>%
  select(where(~ !all(is.na(.))))
#
#
dat_numeric <- dat_numeric %>%
  select("depth_cm", "percent_loi",  "Cu_ug_g", "Fe_mg_g", "Hg_ug_g", "K_mg_g",  "P_mg_g",          "Pb_ug_g",  "Zn_ug_g" ) ## NEW drop the columns we did not cover in the Results, Br, Cl, Si, Ti, Y, Rb, Cl
#
# Remove units from column names
colnames(dat_numeric) <- gsub("_(ug|mg)_g|_cm", "", colnames(dat_numeric))
#
names(dat_numeric)[names(dat_numeric) == 'percent_loi'] <- 'LOI'
#
colSums(is.na(dat_numeric))
# make data frame into matrix
dat_matrix <- rcorr(as.matrix(dat_numeric),  type = "spearman")
#
cor_mat <- dat_matrix$r
p_mat <- dat_matrix$P
#
# Apply FDR (false discovery rate) correction
p_adjusted_fdr <- matrix(p.adjust(as.vector(p_mat), method = "bonferroni"), 
                         ncol = ncol(p_mat), nrow = nrow(p_mat))
colnames(p_adjusted_fdr) <- colnames(p_mat)
rownames(p_adjusted_fdr) <- rownames(p_mat)
#
write.csv(p_adjusted_fdr, "C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/corrplot_p_adjusted_fdr.csv")
#
write.csv(p_mat, "C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/corrplot_p_unadjusted.csv")
#
cor(dat$depth_cm, dat$Cu, use = "complete.obs", method = "spearman")
cor.test(dat$depth_cm, dat$Cu, method = "spearman", exact = FALSE)
#
#
# Make correlation plot
corrplot(cor_mat,  type="upper", diag=FALSE, tl.col = "black", tl.cex = 0.9,
         p.mat = p_adjusted_fdr, sig.level = 0.05, insig = "blank") # doesnt seem to change with correction...
#
#
jpeg("LP3+_peat_core_corrplot_spearman.jpeg", units="in", width=6, height=6, res=300)

corrplot(cor_mat, method="color", type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         p.mat = p_adjusted_fdr, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,  number.cex = 0.8 )

dev.off()

######################################################################
#### Lmer for LOI differences ####
#
# I want to determine any statisitcal differences in LOI content of the top 30 cm between sites. 
#
# first, subset the top 30cm for each site
dat_top30 <- subset(dat, depth_cm <= 30)
#
# check
range(dat_top30$depth_cm, na.rm = TRUE) #1-30
#
# check if any sites have less than 30 cm depth-- SW = 28, ND = 28, should be fine
max_depth_per_site <- dat_top30 %>%
  group_by(site) %>%
  summarise(max_depth = max(depth_cm, na.rm = TRUE))
#
#  next, calculate average LOI per site for the top 30 cm
dat_top30_site <- dat_top30 %>%
  group_by(site, land_use, peat) %>%
  summarise(mean_LOI = mean(percent_loi, na.rm = TRUE), 
            sd_LOI   = sd(percent_loi, na.rm = TRUE) )
#
# average per land use
dat_top30_land_use <- dat_top30 %>%
  group_by(land_use) %>%
  summarise(
    mean_LOI = mean(percent_loi, na.rm = TRUE),
    sd_LOI   = sd(percent_loi, na.rm = TRUE)  )
#
# convert % to proportion
dat_top30$loi_prop <- dat_top30$percent_loi / 100
#
# can't run an lm or lmm on this data bc the samples are not independant... so can only compare means :(
# but you can look at land use differences with site as a random factor
loi_model <- lmer(percent_loi ~ land_use + (1 | site), data = dat_top30)
summary(loi_model)
anova(loi_model)
plot(loi_model)
qqline(resid(loi_model), col = "red")
# lmm isn't the best for percentage data, use glmm
# adjust 100s to make them a bit smaller
n <- nrow(dat_top30)
dat_top30$loi_prop_adj <- (dat_top30$loi_prop * (n - 1) + 0.5) / n
#
# run beta glmm
model_beta <- glmmTMB(loi_prop_adj ~ land_use + (1 | site),  family = beta_family(link = "logit"),  data = dat_top30)
#

#
# Estimated marginal means for land_use
emmeans(loi_model, ~ land_use)
pairs(emmeans(loi_model, ~ land_use))
#
# Pairwise comparisons
pairs(emm, adjust = "fdr")  #try tukey, sidak, fdr none sig
#
# try adding peat type
loi_model2 <- lmer(percent_loi ~ land_use*peat + (1 | site), data = dat_top30)
summary(loi_model2) # because peat type isn't represented in each land use this doesnt work
#
# Other variables
cu_model <- lmer(Cu_ug_g ~ land_use + (1 | site), data = dat)
summary(cu_model)
anova(cu_model)
emmeans(cu_model, ~ land_use*peat)
pairs(emmeans(cu_model, ~ land_use*peat))
#
fe_model <- lmer(Fe_mg_g ~ land_use + (1 | site), data = dat)
summary(fe_model)
anova(fe_model)
emmeans(fe_model, ~ land_use)
pairs(emmeans(fe_model, ~ land_use))
#
pb_model <- lmer(Pb_ug_g ~ land_use + (1 | site), data = dat)
summary(pb_model)
anova(pb_model)
emmeans(pb_model, ~ land_use)
pairs(emmeans(pb_model, ~ land_use))
#
hg_model <- lmer(Hg_ug_g ~ land_use + (1 | site), data = dat)
summary(hg_model)
anova(hg_model)
emmeans(hg_model, ~ land_use)
pairs(emmeans(hg_model, ~ land_use))
#
zn_model <- lmer(Zn_ug_g ~ land_use + (1 | site), data = dat)
summary(zn_model)
anova(zn_model)
emmeans(zn_model, ~ land_use)
pairs(emmeans(zn_model, ~ land_use))
#
k_model <- lmer(K_mg_g ~ land_use + (1 | site), data = dat)
summary(k_model)
anova(k_model)
emmeans(k_model, ~ land_use)
pairs(emmeans(k_model, ~ land_use))
#
p_model <- lmer(P_mg_g ~ land_use + (1 | site), data = dat)
anova(p_model)
summary(p_model)
emmeans(p_model, ~ land_use)
pairs(emmeans(p_model, ~ land_use))
#
#
#
#
#
############
### distance from Manchester ####


manchester <- data.frame(  lon = -2.2426,  lat = 53.4808)

dat$dist_manchester_km <- distHaversine(  cbind(dat$lon, dat$lat),  cbind(manchester$lon, manchester$lat)) / 1000

model_pb <- lm(Pb_ug_g ~ dist_manchester_km, data = dat)
summary(model_pb)

model_pb <- lmer(log(Pb_ug_g) ~ dist_manchester_km + (1|site), data = dat)
summary(model_pb)

ggplot(dat, aes(dist_manchester_km, log(Pb_ug_g))) +
  geom_point() +
  geom_smooth(method = "lm")
