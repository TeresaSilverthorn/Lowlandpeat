##### XRF analysis on LP3+ peat cores #####
#
#
# Load necessary packages 
library(dplyr)
library(ggplot2)
library(data.table) # for fread
#
#
colnames(dat)
# Site file directory for figures 
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Figures")
#
#
#
# Read in data
dat <- fread("C:/Users/teres/Documents/LowlandPeat3/LP3+ Peat coring/Lab work/XRF data/XRF_T.Silverthorn_2025-02-07.txt")
#
head(dat)
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
    site %in% c("ND", "MF", "WBF", "RV", "MOS") ~ "Grassland/Raised WT",
    site %in% c("HF", "WW", "WSF" ) ~ "Semi-natural fen",
    site %in% c("BM" ) ~ "Semi-natural bog",
    TRUE ~ "Other"  # Assign "Other" to all other sites (modify as needed)
  )) %>%                                               # Remove the first two columns
  select(sample_code, site, land_use, depth_cm, everything())
#
#
#
#
################################################################################
##### Plots #####
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
tiff("Pb_LP3+_peat_cores_all.tiff", units="in", width=6.5, height=4, res=300)
#
Pb_all <- ggplot(subset(dat, site!="HF"), aes(y = Pb_ug_g, x = depth_cm)) +
  geom_point() +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Pb", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
  #geom_hline(yintercept = 0, color = "black")  +   geom_vline(xintercept = 100, color = "black") 
Pb_all
#
dev.off()
#
#
#
#### Zn plot for all sites ####
#
tiff("Zn_LP3+_peat_cores_all.tiff", units="in", width=6.5, height=4, res=300)
#
Zn_all <- ggplot(subset(dat, site!="HF"), aes(y = Zn_ug_g, x = depth_cm)) +
  geom_point() +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Zn", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal() +  # Use a minimal theme for a clean look
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
Zn_all
#
dev.off()
#
#
#
#### Fe plot for all sites ####
#
tiff("Fe_LP3+_peat_cores_all.tiff", units="in", width=6.5, height=4, res=300)
#
Fe_all <- ggplot(subset(dat, site!="HF"), aes(y = Fe_mg_g, x = depth_cm)) +
  geom_point() +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Fe", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal()  +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
Fe_all
#
dev.off()
#
#
#
#### Fe plot for all sites ####
#
tiff("Cu_LP3+_peat_cores_all.tiff", units="in", width=6.5, height=4, res=300)
#
Cu_all <- ggplot(subset(dat, site!="HF"), aes(y = Cu_ug_g, x = depth_cm)) +
  geom_point() +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Cu", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal()  +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
Cu_all
#
dev.off()
#
#
################################################################################
#
#
#### Elements P and K are associated with agricultural fertilizers (Turner et al 2014) ####
#
#
#### P plot for all sites ####
#
tiff("P_LP3+_peat_cores_all.tiff", units="in", width=6.5, height=4, res=300)
#
P_all <- ggplot(subset(dat, site!="HF"), aes(y = P_mg_g, x = depth_cm)) +
  geom_point() +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "P", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal()  +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
P_all
#
dev.off()
#
#
#
##### K plot for all sites ####
#
tiff("K_LP3+_peat_cores_all.tiff", units="in", width=6.5, height=4, res=300)
#
K_all <- ggplot(subset(dat, site!="HF"), aes(y = K_mg_g, x = depth_cm)) +
  geom_point() +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "K", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
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
tiff("Br_LP3+_peat_cores_all.tiff", units="in", width=6.5, height=4, res=300)
#
Br_all <- ggplot(subset(dat, site!="HF"), aes(y = Br_ug_g, x = depth_cm)) +
  geom_point() +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Br", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
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
tiff("Ti_LP3+_peat_cores_all.tiff", units="in", width=6.5, height=4, res=300)
#
Ti_all <- ggplot(subset(dat, site!="HF"), aes(y = Ti_ug_g, x = depth_cm)) +
  geom_point() +  
  #geom_line(aes(group = 1)) + 
  geom_smooth(se = FALSE, method = "loess", span = 0.15, colour = "red") + 
  labs(y = "Br", x = "Depth (cm)") +
  scale_x_reverse() +  # Reverse the x-axis so 0 is on the right
  coord_flip() +  # Rotate the plot 90 degrees counterclockwise
  facet_wrap(~ site, nrow = 2, ncol = 6) +  # Create a plot for each site
  theme_minimal()  +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))
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










