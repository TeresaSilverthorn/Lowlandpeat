########## Script to visulize LP3 ditch DOC concentrations ##########
# Note from Mike Marhc 3, 2025: I forgot to say, I ran the water samples for UV-vis last week. Attached are the worked up DOC concs. First tab uses the default model parameters from Ed Tipping’s model. In the second tab I have re-parametrised the model using Luke’s measured DOC concs. However, his concs are *very* high for winter so I am slightly sceptical – he said their instrument had been drifting a bit. I’ve asked for more samples from him in future so we can compare modelled vs measured again.
#
#For now, use the default model parameters...

# July 16, 2025 from Mike: Here is the new data, using original and updated (calibrated on Wrights) model parameters. I’m tending to believing that original is better but need to spend some time comparing to LP3+ data to see which looks more sensible.
#
#
# Load packages
library(readxl)
library(stringr)
library(dplyr)
library(lubridate)
#
#
# Set WD
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Figures")
#
# Read in DOC data
#
# This is the ancillary data file from the Sharepoint, you can periodically check and download an updated version
dat <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/DOC data/ditch_ancillary_data_2025-08-07.csv")
head(dat)
#
#
#
#
# Rename columns
colnames(dat)[colnames(dat) == "DOC..mg.C.l..1."] <- "DOC_mg_l"
colnames(dat)[colnames(dat) == "TC..mg.C.l..1."] <- "TC_mg_l"
colnames(dat)[colnames(dat) == "DIC..mg.C.l..1."] <- "DIC_mg_l"

#
# # Format date columns
# Replace / with -
dat <- dat %>%
  mutate(date = str_replace_all(date, "/", "-"))
#
#
#
# Remove rows if DOC is NA
dat <- dat %>%
  filter(!is.na(DOC_mg_l))
#
# Make sure sample_codes align
dat <- dat %>%
  mutate(sample_code = ifelse(grepl("-DOC$", sample_code), sample_code, paste0(sample_code, "-DOC")))
#
# Make a new column for site and ditch
#Add a new column for site and ditch #
dat <- dat %>%
  mutate(    sample_code = case_when(str_detect(sample_code, "^(LAN|CHE)-") ~ str_remove(sample_code, "-\\d{8}(?=-DOC$)"), TRUE ~ sample_code),
    site = str_extract(sample_code, "(?<=^[A-Z]-)[A-Z]+\\d*"),
    site = if_else(
      str_detect(sample_code, "^[A-Z]-[A-Z]+-[A-Z]"),
      str_extract(sample_code, "(?<=^[A-Z]-)[A-Z]+-[A-Z]"),
      site ),
    site = ifelse(str_detect(site, "^(LC|SW)"), str_remove(site, "-\\d+$"), site),
    ditch = str_extract(sample_code, "\\d+(\\.\\d+)?(?=-DOC$)"),  
# if site is still NA  replace with letters before first '-'
site = if_else(
  is.na(site) | site == "",  str_extract(sample_code, "^[^-]+"),  site),
# fix "TP" by adding letter after first hyphen
site = if_else(  site == "TP",  paste0(site, "-", str_extract(sample_code, "(?<=TP-)[A-Z]")),  site),
# fix "SSA" or "SSB" by inserting hyphen between SS and A/B
site = if_else(  site == "SS",  paste0("SS-", str_extract(sample_code, "(?<=SS-)[AB]")),  site),
site = if_else(str_detect(site, "^RG(-R)?$"), str_extract(sample_code, "RG-R\\d+"), site)) %>%
  select(date, sample_code, site, ditch, everything())
#
#
#         
# Make a new column for month
dat <- dat %>%  
  mutate(date = dmy(date),
    month = lubridate::month(date, label = TRUE, abbr = TRUE), 
    month = factor(month))
#
# Order sites for plotting
dat$site <- factor(dat$site, levels = c("WF-A", "WF-B" , "RG-R6", "RG-R8", "SS-A", "SS-B", "TP-A" , "TP-B", "GC-A", "GC-B", "CHE", "LAN", "LC", "SW" ))
#
# Add a column for treatment
dat <- dat %>%
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
#
# Re order
dat <- dat %>%
  select(date, month, sample_code, site, ditch, time, treatment, DOC_mg_l, water_temp_c, everything())
#
# Save df as csv
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/DOC data/LP3_ditch_DOC_all.csv")
#
##################################################################################
#### PLOT ####
#
#
#
jpeg("LP3_ditch_DOC_monthly.jpeg", units="in", width=8.5, height=6.5, res=300)

DOC_monthly <- ggplot(dat, aes(x = as.POSIXct(date), y = DOC_mg_l, colour=site )) + 
  #geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75), shape = 18, alpha=0.5, size = 3, aes(group = site)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1, alpha = 0.7) +  
  #geom_jitter(aes(shape=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + 
  labs( y = expression(DOC ~ "(mg L"^-1*")"),   x = NULL) +
  scale_x_datetime(date_labels = "%b", date_breaks = "1 month") + 
  theme_minimal() + # Clean theme
  theme(legend.position = "top",  axis.title = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=10) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ,  panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank() , panel.grid.minor = element_blank() ) 
DOC_monthly

dev.off()


jpeg("LP3_ditch_DOC_all.jpeg", units="in", width=8, height=5, res=300)

DOC_all <- ggplot(dat, aes(x = site, y = DOC_mg_l)) + 
  geom_boxplot(aes(fill = treatment), outlier.shape = NA) + 
  geom_jitter(aes(fill = treatment), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  labs(y = expression(DOC ~ "(mg L"^-1*")"),  x = NULL,   fill = " " ) +
  theme_minimal() + # Clean theme
  theme( axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("#e57e73", "#6497bf")) 
DOC_all

dev.off()

# regarding the high values at Wrights, note that LP3+ DOC range for Wrights is 46.4 to 61.8

###################### Statistical analysis #####################################
#
#
# Summary statistics 
mean(dat$DOC_mg_l, na.rm=T)
sd(dat$DOC_mg_l, na.rm=T)
min(dat$DOC_mg_l, na.rm=T)
max(dat$DOC_mg_l, na.rm=T)
#
# 
# Summary stats by site
summary_stats_by_site <- dat %>%
  group_by(site) %>%
  summarise(
    DOC_mg_l_mean = mean(DOC_mg_l, na.rm = TRUE),
    DOC_sd = sd(DOC_mg_l, na.rm = TRUE) )
#
#
#
#
# 1. Test for normality using shapiro wilks test. If the p-value is less than 0.05, you reject the null hypothesis, suggesting the data is not normally distributed.
shapiro_results <- sapply(dat[, c(5)], shapiro.test) # non-normal, so go with non-parametric
#
# 2. Use Kruskal-Wallis test to see differences between sites. If p<0.05 there is a significant difference between groups
#
kruskal_results <- dat %>%  # for concentration data
  select(c(5)) %>%
  map(~ kruskal.test(.x ~ dat$site))
#
kruskal_p_values_conc <- kruskal_results %>%
  map_dbl(~ .x$p.value)   # significant
#
#
# 3. Run pairwise Wilcoxon rank sum test
#
pairwise_results <- pairwise.wilcox.test(dat$DOC_mg_l, dat$site, p.adjust.method = "bonferroni", exact = FALSE)
# No sig difs between paired sites
#
#
#
#