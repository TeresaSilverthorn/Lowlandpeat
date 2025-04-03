########## Script to visulize LP3 ditch DOC concentrations ##########
# Note form Mike: I forgot to say, I ran the water samples for UV-vis last week. Attached are the worked up DOC concs. First tab uses the default model parameters from Ed Tipping’s model. In the second tab I have re-parametrised the model using Luke’s measured DOC concs. However, his concs are *very* high for winter so I am slightly sceptical – he said their instrument had been drifting a bit. I’ve asked for more samples from him in future so we can compare modelled vs measured again.
#
#For now, use teh default model parameters...
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
#
#
# Read in DOC data
#
dat1 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/DOC data/LP3 ditch water DOC concentrations_TS.xlsx", sheet="Data", skip=1) # This is Mike's UV vis data for the East Anglia samples
head(dat1)
#
dat2 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/DOC data/ditch_ancillary_data_2025-04-01.xlsx")
head(dat2)
#
dat3 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/DOC data/Aquatic C wright farm tidy.csv") # this is the most up to date data from Wrights as of 2024-04-03
head(dat3)
#
# Remove first row, is empty; and empty columns
dat1 <- dat1 %>% 
  slice(-1) %>%       
  select(-c(6:10)) 
# 
# Since Luke sent me his up to date data separately, choose only Liz's Somerset GC data here
dat2 <- dat2 %>%
  filter(sampler == "Liz Flint")
#
#
# For now, exclude Luke's data from Mike's data, and use what he sent to you
dat1 <- dat1 %>%
  filter(Site != "Luke/Wrights")
#
#
# Make a column for sample_code
dat3 <- dat3 %>%
  mutate(sample_code = paste(Sample, toupper(field), replicate, sep = "-")) %>%
  select(sample_code, everything())
#
#
# Rename columns
colnames(dat1)[colnames(dat1) == "Sample"] <- "sample_code"
colnames(dat1)[colnames(dat1) == "Date"] <- "date"
colnames(dat1)[colnames(dat1) == "[DOC]310"] <- "DOC_mg_l"
colnames(dat2)[colnames(dat2) == "DOC (mg C l^-1)"] <- "DOC_mg_l"
colnames(dat2)[colnames(dat2) == "TC (mg C l^-1)"] <- "TC_mg_l"
colnames(dat2)[colnames(dat2) == "DIC (mg C l^-1)"] <- "DIC_mg_l"
colnames(dat3)[colnames(dat3) == "Date"] <- "date"
colnames(dat3)[colnames(dat3) == "DOC..mg.l."] <- "DOC_mg_l"
colnames(dat3)[colnames(dat3) == "TC..mg.l."] <- "TC_mg_l"
colnames(dat3)[colnames(dat3) == "DIC..mg.l."] <- "DIC_mg_l"
#
# # Format date columns
dat1$date <- as.character(dat1$date)
dat2$date <- as.character(dat2$date)
dat3$date <- as.character(dat3$date)
#
# In dat3, replace / with -
dat3 <- dat3 %>%
  mutate(date = str_replace_all(date, "/", "-"))
#
# Combine data
dat <- rbind(
  dat1 %>% select(date, sample_code, DOC_mg_l),  
  dat2 %>% select(date, sample_code, DOC_mg_l), 
  dat3 %>% select(date, sample_code, DOC_mg_l))
#
# Make sure sample_codes align
dat <- dat %>%
  mutate(sample_code = ifelse(grepl("-DOC$", sample_code), sample_code, paste0(sample_code, "-DOC")))
#
# Make a new column for site and ditch
#Add a new column for site and ditch #
dat <- dat %>%
  mutate(site = str_extract(sample_code, "(?<=^\\w-)[A-Za-z0-9-]+")) %>%
  mutate(site = str_extract(site, "^[A-Za-z]+(?:-[A-Za-z0-9]+)?")) %>%
  mutate(site = ifelse(str_detect(site, "^(LC|SW)"), str_remove(site, "-\\d+$"), site)) %>%
  mutate(ditch = str_extract(sample_code, "\\d+(?=-DOC$)"))  %>%
  mutate(ditch = ifelse(str_detect(sample_code, "\\."), str_extract(sample_code, "(\\d+)(?=\\.)"), ditch)) %>%
  mutate(ditch = ifelse(str_detect(site, "^(GC)"), str_extract(sample_code, "\\d+(?=-DOC$)"), ditch)) %>% 
  select(date, sample_code, site, ditch, everything()) 
#
# Make a new column for month
dat <- dat %>%
  mutate(
    date = as.POSIXct(date, tz = "Europe/London"),
    month = month(date, label = TRUE, abbr = TRUE), 
    month = factor(month, levels = c("Dec", "Jan", "Feb", "Mar")))
#
# Order sites for plotting
dat$site <- factor(dat$site, levels = c("WF-A", "WF-B" , "RG-R6", "RG-R8", "SS-A", "SS-B", "TP-A" , "TP-B", "GC-A", "GC-B", "LC", "SW" ))
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
      TRUE ~ NA_character_   )  )
#
#
# Save df as csv
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Data/DOC data/LP3_ditch_DOC_cleaned.csv")
#
##################################################################################
#### PLOT ####
#
#
#
jpeg("LP3_ditch_DOC_monthly.jpeg", units="in", width=8.5, height=6.5, res=300)

DOC_monthly <- ggplot(dat, aes(x = site, y = DOC_mg_l )) + 
  geom_boxplot(outlier.shape = NA) + # Add boxplot without showing outliers
  geom_jitter(aes(colour=as.factor(ditch)), alpha=0.5, size = 3, width = 0.2) + 
  labs( y = expression(DOC ~ "(mg L"^-1*")"),   x = NULL, colour = "Ditch #", shape = "Month") +
  theme_minimal() + # Clean theme
  theme(legend.position = "top",  axis.title = element_text(size = 12), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=10) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ,  panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank() , panel.grid.minor = element_blank() ) +
  facet_wrap(~month)
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

###################### Statistical analysis #####################################
#
#
# Summary statistics 
mean(dat$DOC_mg_l, na.rm=T)
sd(dat$DOC_mg_l, na.rm=T)
min(dat$DOC_mg_l, na.rm=T)
max(dat$DOC_mg_l, na.rm=T)

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



