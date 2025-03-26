##### XRF analysis on LP3+ peat cores #####
#
#
# Load necessary packages 
library(dplyr)
library(ggplot2)
library(data.table) # for fread
library(stringr)
#
#
# Site file directory for figures 
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Figures")
#
#
#
# Read in XRF data
dat1 <- fread("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/XRF data/XRF_T.Silverthorn_2025-02-07.txt")
#
head(dat)

# load in corrected and uncorrected and see if any are missing

dat <- fread("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/XRF data/XRF_T.Silverthorn_2025-03-26.txt")

# Make column names based on the first two rows (element, units)
# And make any other adjustments to the column names

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
# Replace non numeric values with NAs (cases below detection limits or insufficient sample)
dat <- dat %>%
  mutate(across(6:77, ~as.numeric(.), .names = "{.col}")) #NAs introduced error is expected
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
  summarize(max_depth = max(depth_cm, na.rm = TRUE))
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
#
#
####################################################################
# LOI corrected data

dat_cor <- fread("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/XRF data/XRF_T.Silverthorn 2025-03-26_CorrectedLOI.txt")

str(dat_cor)

# Make column names based on the first two rows (element, units)
# And make any other adjustments to the column names

dat_cor <- dat_cor %>%
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
# Replace non numeric values with NAs (cases below detection limits or insufficient sample)
dat_cor <- dat_cor %>%
  mutate(across(6:77, ~as.numeric(.), .names = "{.col}")) #NAs introduced error is expected
#
#
# Add a column for land use
#
dat_cor$site <- as.factor(dat_cor$site) # make site a factor
#
dat_cor <- dat_cor %>%
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
max_depth_per_site <- dat_cor %>%
  group_by(site) %>%
  summarize(max_depth = max(depth_cm, na.rm = TRUE))
#
#
dat_cor$site <- factor(dat_cor$site, levels = c("BM", "WSF", "WW", "HF", "SW", "LC", "RGR6", "WFA", "MF", "MOS", "ND", "RV", "WBF"))
#
#
# Correct error in WW, delete the _ after WW
dat_cor <- dat_cor %>%
  mutate(sample_code = str_replace(sample_code, "WW_", "WW"))
#
dat_cor <- dat_cor %>%
  mutate(sample_code = str_replace(sample_code, "MF_", "MF"))


# Find missing values
# Find values in dat but not in dat_cor
missing_in_dat_cor <- anti_join(dat, dat_cor, by = "sample_code")

# Find values in df2 but not in df1
missing_in_dat <- anti_join(dat_cor, dat, by = "sample_code")

# Output
print(missing_in_df2)
print(missing_in_df1)



#
#
#
#
#
# Make column names based on the first two rows (element, units)
# And make any other adjustments to the column names
#
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
# Replace non numeric values with NAs (cases below detection limits or insufficient sample)
dat <- dat %>%
  mutate(across(6:139, ~as.numeric(.), .names = "{.col}")) #NAs introduced error is expected
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
  summarize(max_depth = max(depth_cm, na.rm = TRUE))
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
#
########
#
# Read in the LOI data
dat_LOI <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/LOI/LOI_all_data_combined_2025-03-17.csv")
#
head(dat_LOI)  #612 obs
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
# Fore sites that have LOI data but no matching XRF data, we need to add site, land use and depth
dat <- dat %>%
  mutate(
    site = if_else(is.na(site) & str_detect(sample_code, "HF"), "HF", site),
    land_use = if_else(site == "HF" & is.na(land_use), "Semi-natural fen", land_use),
    depth_cm = if_else(site == "HF" & is.na(depth_cm), 
                       as.numeric(str_extract(sample_code, "[^_]+$")), 
                       depth_cm)  )
#
dat <- dat %>%
  mutate(
    site = if_else(is.na(site) & str_detect(sample_code, "MOS"), "MOS", site),
    land_use = if_else(site == "MOS" & is.na(land_use), "Grassland", land_use),
    depth_cm = if_else(site == "MOS" & is.na(depth_cm), 
                       as.numeric(str_extract(sample_code, "[^_]+$")), 
                       depth_cm)  )
#
dat <- dat %>%
  mutate(
    site = if_else(is.na(site) & str_detect(sample_code, "WSF"), "WSF", site),
    land_use = if_else(site == "WSF" & is.na(land_use), "Semi-natural fen", land_use),
    depth_cm = if_else(site == "WSF" & is.na(depth_cm), 
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


#
# Reorder site levels
dat$site <- factor(dat$site, levels = c("BM", "WSF", "WW", "HF", "SW", "LC", "RGR6", "WFA", "MF", "MOS", "ND", "RV", "WBF"))
#
# Reorder land use levels
dat$land_use <- factor(dat$land_use, levels = c("Rewetted bog", "Semi-natural fen", "Regenerative arable", "Conventional arable", "Grassland" ))
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
tiff("LOI_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
LOI_all <- ggplot(subset(dat, !is.na(site)), aes(y = percent_loi, x = depth_cm)) +
  geom_rect(data = subset(dat, !is.na(site)), aes(fill = land_use), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  geom_point(size=1.5, alpha=0.7) +    #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "LOI (%)", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 7) +  # Create a plot for each site
  theme_minimal() +   theme(legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(angle = 45, hjust = 1)) +   scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") )
LOI_all
#
dev.off()
#
#
#
#### Plot % moisture ####
#

tiff("moisture_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
moisture_all <- ggplot(subset(dat, !is.na(site)), aes(y = percent_moisture, x = depth_cm)) +
  geom_rect(data = subset(dat, !is.na(site)), aes(fill = land_use), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  geom_point(size=1.5, alpha=0.7) +    #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Moisture (%)", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 7) +  # Create a plot for each site
  theme_minimal() +   theme(legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(angle = 45, hjust = 1)) +   scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") )
moisture_all
#
dev.off()
#
##### Plots ##################################
#
# For now, exclude HF, because you only have about 10cm of data....
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
#### Pb plot for all sites ####
#
tiff("Pb_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
Pb_all <- ggplot(subset(dat, site!="HF"), aes(y = Pb_ug_g, x = depth_cm)) +
  geom_rect(data = subset(dat, site!="HF"), aes(fill = land_use), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(size=1.5, alpha=0.7) +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Pb", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal() + 
  theme(legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") )
Pb_all
#
dev.off()
#
#
#
#### Zn plot for all sites ####
#
tiff("Zn_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
Zn_all <- ggplot(subset(dat, site!="HF"), aes(y = Zn_ug_g, x = depth_cm)) +
  geom_rect(data = subset(dat, site!="HF"), aes(fill = land_use), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(size=1.5, alpha=0.7) + 
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Zn", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") )
Zn_all
#
dev.off()
#
#
#
#### Fe plot for all sites ####
#
tiff("Fe_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
Fe_all <- ggplot(subset(dat, site!="HF"), aes(y = Fe_mg_g, x = depth_cm)) +
  geom_rect(data = subset(dat, site!="HF"), aes(fill = land_use), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(size=1.5, alpha=0.7) +   
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Fe", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal()  +
  theme(legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1),  axis.ticks.x = element_line(), axis.ticks.y = element_line()) +   scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") )
Fe_all
#
dev.off()
#
#
#
#### Cu plot for all sites ####
#
tiff("Cu_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
Cu_all <- ggplot(subset(dat, site!="HF"), aes(y = Cu_ug_g, x = depth_cm)) +
  geom_rect(data = subset(dat, site!="HF"), aes(fill = land_use), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  geom_point(size=1.5, alpha=0.7) +  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Cu", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal()  +
  theme(legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank()) +
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") )
Cu_all
#
dev.off()
#
#
#
#### Hg plot for all sites ####
#
tiff("Hg_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
Hg_all <- ggplot(subset(dat, site!="HF"), aes(y = Hg_ug_g, x = depth_cm)) +
  geom_rect(data = subset(dat, site!="HF"), aes(fill = land_use), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +  geom_point(size=1.5, alpha=0.7) +  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Hg", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal()  +
  theme(legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line(), axis.text.x = element_text(angle = 45, hjust = 1), panel.background = element_blank()) +
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") )
Hg_all
#
dev.off()
################################################################################
#
#
#### Elements P and K are associated with agricultural fertilizers (Turner et al 2014) ####
#
#
#### P plot for all sites ####
#
tiff("P_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
P_all <- ggplot(subset(dat, site!="HF"), aes(y = P_mg_g, x = depth_cm)) +
  geom_rect(data = subset(dat, site!="HF"), aes(fill = land_use), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(size=1.5, alpha=0.7) +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "P", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal()  +
  theme(legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line()) +
  scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") )
P_all
#
dev.off()
#
#
#
##### K plot for all sites ####
#
tiff("K_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
K_all <- ggplot(subset(dat, site!="HF"), aes(y = K_mg_g, x = depth_cm)) + 
  geom_rect(data = subset(dat, site!="HF"), aes(fill = land_use), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(size=1.5, alpha=0.7) +   
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "K", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line()) +   scale_fill_manual(values = c("Conventional arable" = "#D8B4F8", "Regenerative arable" =  "#8F90D1", "Grassland" = "#FDE68A",  "Semi-natural fen" ="#B5E48C", "Rewetted bog" = "#A5F2D4") )
K_all
#
dev.off()
#
#
#
#
###############################################################################
#
#### The halogen bromine (Br), which is retained stratigraphically in accumulating peat (RoosBarraclough et al., 2002) ####
#
#
tiff("Br_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
Br_all <- ggplot(subset(dat, site!="HF"), aes(y = Br_ug_g, x = depth_cm)) +
  geom_point(size=1.5, alpha=0.7) +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Br", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal() +
  theme(legend.position = "top", panel.border = element_rect(color = "black", fill = NA, size = 1), axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line())
Br_all
#
dev.off()
#
#
#
##################################################################################
#
#### The soil erosion indicator Ti (Holzer and Holzer, 1998) ####
#
#
tiff("Ti_LP3+_peat_cores_all.tiff", units="in", width=8, height=6, res=300)
#
Ti_all <- ggplot(subset(dat, site!="HF"), aes(y = Ti_ug_g, x = depth_cm)) +
  geom_point(size=1.5, alpha=0.7) + 
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Ti", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal()  +
  theme(legend.position = "top", panel.border = element_rect(color = "black", fill = NA, size = 1), axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks.x = element_line(), axis.ticks.y = element_line())
Ti_all
#
dev.off()
#
#
#
#
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
dat_subset <- dat_subset %>% select(  -NA_NA, -Ag_ug_g, -Au_ug_g, -In_ug_g, -Hg_ug_g, -Cs_ug_g, -La_ug_g, -Cd_ug_g, -U_ug_g, -Co_ug_g, -Ce_ug_g, -Hf_ug_g, -W_ug_g, -Te_ug_g, -Sb_ug_g,   -Ge_ug_g, -I_ug_g, -Na_mg_g, -Na2O_mg_g, -Mo_ug_g,   -Ba_ug_g, -Sn_ug_g, -Ga_ug_g, -Rb_ug_g, -Mg_mg_g, -Se_ug_g, -Cr_ug_g, -Zr_ug_g, -Pd_ug_g, -Rh_ug_g, -Ta_ug_g, -Bi_ug_g, -Ru_ug_g, -Tl_ug_g, -Th_ug_g, -MgO_mg_g)
#
#
# Also subset other useless columns and non-numeric columns
dat_num <- dat_subset %>% select(c(8:33))
#
# There are just a couple NA values, fill with mean (2 for Nb_ug_g, 2 for Zn_ug_g, 2 for Pb_ug_g)
dat_num$Nb_ug_g[is.na(dat_num$Nb_ug_g)] <- mean(dat_num$Nb_ug_g, na.rm = TRUE)
dat_num$Zn_ug_g[is.na(dat_num$Zn_ug_g)] <- mean(dat_num$Zn_ug_g, na.rm = TRUE)
dat_num$Pb_ug_g[is.na(dat_num$Pb_ug_g)] <- mean(dat_num$Pb_ug_g, na.rm = TRUE)
#
#
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
# Visualize the PCA with land-use as a grouping factor
dat_subset$land_use <- as.factor(dat_subset$land_use)
levels(dat_subset$land_use)
#
my.col.var <- c(  "#1f78b4",  "#33a02c", "#91278e", "#e31a1c",  "#ff7f00" )
#
#
#tiff("PCA_LP3+_mesocosm_BW.tiff", units="in", width=8, height=6, res=300)
PCA_fig <- fviz_pca_biplot(pca_result, 
                           col.ind = dat_subset$land_use,
                           addEllipses = TRUE, label = "var",
                           pointsize=3,
                           alpha.ind=0.25,
                           mean.point=F,
                           palette=my.col.var,
                           col.var = "black", repel = TRUE,
                           legend.title = " ") + ggtitle(NULL) 
PCA_fig
#dev.off()










