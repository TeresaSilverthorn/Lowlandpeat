#### R script for water quality data for LP3+ mesocosm experiment ####
#
#
#
## Load in necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(purrr)
library(factoextra)
library(ggpubr)
library(stringr)
library(lubridate)
library(Hmisc)
library(cowplot)
library(lme4)
library(lmerTest)
library(emmeans)
library(scales)
library(patchwork)
library(corrplot)
#
#
#
# Set working directory for figures
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Figures")
#
#
# load in data
dat1 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/Report C110 20250417_TS.xlsx", range= "A10:AF94")  
#
head(dat1) #84 obs of 32 vars
#
#
dat2 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/Report C113 INTERIM 20250330_TS.xlsx", range= "A10:X258") #This report is missing some data for C1-W2 (mesocosm IDs 1,2,4,13 etc so we use C118 which has all in duplicate). Also, note this has duplicate bottle refs for C1W1--take the average
#note we are missing mesocosm 17 for C1 W2
#
head(dat2)
#
dat2 <- dat2  %>%
filter(!grepl("C1- W2", site_label))  #exclude rows which contain "C1- W2" in site_label
#
#
dat3 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/Report C120 INTERIM 20250415_TS.xlsx", range= "A10:T127")  # note this is weirdly missing some samples for C3-W1 (every other and some random ones, so we use C118 instead). Also there are week 4 bin water measurements, which we will also exclude
#
head(dat3)
#
dat3 <- dat3  %>%
  filter(!grepl("C3-W1", site_label))  %>% #exclude rows with "C3- W1" in site_label
  filter(!grepl("C4-W1-BW", site_label))    # exclude bin water measurements

#
#
dat4 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/Report C118 20251124_TS.xlsx", range= "A10:S331")
#
head(dat4)  
#
dat4 <- dat4%>% 
  filter(!grepl("BW", site_label)) %>% # exclude BW sampling here
 filter(!sample_code %in% c("C118-021", "C118-024"))

#remove these two points which are C118-021  C118-024 which are in 113


#
#
#
#
# For dat1 the 61BW to 84BW rows are for the WastedPeat project and can be removed
dat1 <- dat1 %>%
  filter(!grepl("^6[1-9]BW$|^7[0-9]BW$|^8[0-4]BW$", site_label))
#
#
#
#
#
#
########################################################################################################
#### Data cleaning of values below detection limit ####
#
# Remove the "<" symbol across all columns
dat1 <- dat1 %>%
  mutate(across(3:32, ~ as.numeric(str_replace(., "^<", "")))) # Warning message is due to 2 nd NH4 which become NAs
#
#
dat2 <- dat2 %>%
  mutate(across(7:24, ~ str_remove_all(., "\\*"))) %>%    # remove the * after the out of range value "**nn** =
  mutate(across(7:24, ~ as.numeric(str_replace(., "^<", "")))) # warning due to 1 nd in NO2 which becomes NA
#
#
dat3 <- dat3 %>%
  mutate(across(5:20, ~ str_remove_all(., "\\*"))) %>%    # remove the * after the out of range value "**nn** =
  mutate(across(5:20, ~ as.numeric(str_replace(., "^<", ""))))
#
#
dat4 <- dat4 %>%
  mutate(across(8:19, ~ str_remove_all(., "\\*"))) %>%    # remove the * after the out of range value "**nn** =
  mutate(across(8:19, ~ as.numeric(str_replace(., "^<", "")))) %>%
mutate(site_label = if_else( sample_code == "C118-320", "Rainwater", site_label) ) %>%
  mutate(site_label = if_else( sample_code == "C118-321", "Tap water", site_label) ) %>%
mutate(site_label = if_else( sample_code == "C118-259", "Rainwater", site_label) )

#
#
#
# Make a new column for mesocosm
dat1$mesocosmID <- as.numeric(gsub("BW", "", dat1$site_label))
#
dat2 <- dat2 %>%
  mutate(    mesocosmID = str_extract(site_label, "(?<=Tag )\\d+"),
             mesocosmID = if_else( is.na(mesocosmID),str_extract(site_label, "^\\d+(?=\\s[A-Za-z])"),mesocosmID), 
             mesocosmID = if_else( is.na(mesocosmID),str_extract(site_label, "^\\d+(?=-)"),mesocosmID),mesocosmID = as.integer(mesocosmID) ) %>%
  select(sample_code, site_label, mesocosmID, everything())
#    
dat3 <- dat3 %>%
  mutate(mesocosmID = str_extract(site_label, "^\\d+(?=-)"),
         mesocosmID = if_else( is.na(mesocosmID), str_extract(site_label, "\\d+$"),  mesocosmID ), mesocosmID = as.integer(mesocosmID)  )
#
dat4 <- dat4 %>%
  mutate(     mesocosmID = str_extract(site_label, "^\\d+(?=-)"),
    mesocosmID = as.integer(mesocosmID)   )  %>%
select(-c(4:7)) %>%
select(sample_code, site_label, mesocosmID, everything())
#
#
#
#
# take average of bottles for dat2 and dat 4
#
dat2_C1W1 <-  dat2 %>%
  filter(!grepl("BL|Tap", site_label)) %>%  #filter only C1 W1
  group_by(mesocosmID) %>%
  summarise(     across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
                 .groups = "drop"   ) %>%   #average bottle A and B 
  mutate(sample_code = paste0("C113_", mesocosmID)) %>%
  mutate(site_label = paste0(mesocosmID, "-C1- W1"))   %>%  
  select(sample_code, site_label, mesocosmID, everything())
#
# Collect the BL and tap water from dat2
dat2_BL <- dat2 %>% 
filter(grepl("BL|Tap", site_label)) 
#
#
#
#
dat4_C1W2 <- dat4 %>%   #note we are missing mesocosm 17
  filter(grepl("C1- W2", site_label)) %>%  #filter only C1 W2
  group_by(mesocosmID) %>%
  summarise(     across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
                 .groups = "drop"   ) %>%   #average bottle A and B 
  mutate(sample_code = paste0("C118_", mesocosmID)) %>%
  mutate(site_label = paste0(mesocosmID, "-C1- W2"))   %>%  
  select(sample_code, site_label, mesocosmID, everything())
#
#
#get dat4 C2W1, C2W2, C3W1
dat4_rest <- dat4 %>%   #note we are missing mesocosm 17
  filter(!grepl("C1- W2", site_label)) 
#
#
# Combine data files into one dataframe
dat <- bind_rows(dat1, dat2_C1W1, dat2_BL, dat3, dat4_C1W2, dat4_rest)
#
str(dat)  # 590 obs of 42 vars
#
# Remove any columns full of NAs
dat <- dat[, colSums(!is.na(dat)) > 0]
#
str(dat) # 590 obs of 34 vars
#
# Make NaNs NAs
dat <- dat %>%
  mutate(across(where(is.numeric), ~replace(.x, is.nan(.x), NA)))
#
##
############################################################################
# read in the LLD_LOQ ancillary data
LLD_LOQ <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Ancil dat/LLD_LOQ_values.csv")
#
head(LLD_LOQ)
#
# Check if align with dat
setdiff(LLD_LOQ$substance, colnames(dat)) # these are fine, TOC was replaced with NPOC, S is not in dat, and Si is in mg as well as ug
setdiff(colnames(dat), LLD_LOQ$substance)
#
#
#### Loop for LLD ####
# Loop through each row in LLD_LOQ
for (i in 1:nrow(LLD_LOQ)) {
  var_name <- LLD_LOQ$substance[i]
  lld_value <- LLD_LOQ$LLD[i]
  
  # Check if the column exists in dat, and replace values <LLD with LLD/2
  if (var_name %in% colnames(dat)) {
    dat[[var_name]] <- ifelse(
      !is.na(dat[[var_name]]) & dat[[var_name]] < lld_value,
      lld_value / 2,
      dat[[var_name]]
    )
  }
}
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
ancil_dat3 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Cycle 1-W1_new.csv")  # this is associated with C113; issues with date when using excel format
head(ancil_dat3)
#
#
ancil_dat4 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Cycle 1-W2_new.csv")  # this is associated with C113; 
head(ancil_dat4)
#
#
ancil_dat5 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Cycle 3-W1.csv")   # this is associated with C120 
head(ancil_dat5)
#
#
ancil_dat6 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Cycle 4-W1.csv")   # this is associated with C120 
head(ancil_dat6)
# missing week.no
ancil_dat6$Week.no. <- 45
#
#
ancil_dat7 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Cycle 5-W2.csv")   # this is associated with C120 
head(ancil_dat7)
# missing week.no
ancil_dat6$Week.no. <- 47
#
ancil_dat8 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Cycle 2-W1.csv")   # this is associated with C118 
head(ancil_dat8)
#
ancil_dat9 <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Cycle 2-W2.csv")   # this is associated with C118 
head(ancil_dat9)

#
#
# Subet just the LP3+ data
ancil_dat1 <- ancil_dat1 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
ancil_dat2 <- ancil_dat2 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
ancil_dat5 <- ancil_dat5 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
ancil_dat6 <- ancil_dat6 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
ancil_dat7 <- ancil_dat7 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
ancil_dat8 <- ancil_dat8 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
ancil_dat9 <- ancil_dat9 %>% filter(grepl("LP3\\+", Project))  #subset LP3+ data
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
ancil_dat5$Cycle.no. <- as.character(ancil_dat5$Cycle.no.)
#
ancil_dat6$Cycle.no. <- as.character(ancil_dat6$Cycle.no.)
#
ancil_dat7$Cycle.no. <- as.character(ancil_dat7$Cycle.no.)
#
ancil_dat8$Cycle.no. <- as.character(ancil_dat8$Cycle.no.)
#
ancil_dat9$Cycle.no. <- as.character(ancil_dat9$Cycle.no.)
#
#
#
# Merge the ancil dat df's together
ancil_dat <- bind_rows(ancil_dat1, ancil_dat2, ancil_dat3, ancil_dat4, ancil_dat5, ancil_dat6, ancil_dat7, ancil_dat8, ancil_dat9)
str(ancil_dat) #745 obs of 27 vars
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
    grepl("Manchester", source_site) ~ "RV",
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
head(dat) # 425 obs of 40 vars
#
#
# Create a level for BW in the Cycle.no. column
dat <- dat %>% 
mutate(Cycle.no. = case_when(
  str_detect(site_label, "BW") & is.na(Cycle.no.) ~ "BW",
  TRUE ~ as.character(Cycle.no.) ))
#
#
# Make a new column called C.W (cycle week) which combines the cycle and week columns
dat <- dat %>%
  mutate(C.W = if_else(is.na(Week1.2), as.character(Cycle.no.), paste(Cycle.no., Week1.2, sep = "_")))
#
levels(as.factor(dat$C.W))
# Reorder the factor levels
dat <- dat %>%
  mutate(C.W = factor(C.W, levels = c("BW", "BL", "1_1", "1_2", "2_1", "2_2", "3_1", "4_1", "5_2")))
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
# Fill missing data
# Cycle 5 is missing the Week.no., it is 47
dat <- dat %>%
  mutate(Week.no. = ifelse(C.W == "5_2", 47, Week.no.)) 
#
#### convert to per molecule basis ####
dat$NO2_N_mg_l <- dat$NO2_mg_l/46.01*14.01
dat$NO3_N_mg_l <- dat$NO3_mg_l/62*14.01
dat$NH4_N_mg_l <- dat$NH4_mg_l/18.04*14.01
dat$PO4_P_mg_l <- dat$PO4_mg_l/94.97*30.97
dat$SO4_S_mg_l <- dat$SO4_mg_l/96.06*32.06
#
#
# Remove any columns full of NAs
dat <- dat[, colSums(!is.na(dat)) > 0]
#

#
# make a new column for site
dat <- dat %>%
  mutate(
    site_new = case_when(
      site == "TP-A" ~ "Pymoor",
      site == "RV" ~ "Railway View",
      site == "RG-PEF" ~ "Rosedene1",
      site == "RG-R8" ~ "Rosedene2",
      site == "WF-A" ~ "Wrights",
      TRUE ~ site    )   ) 
#
dat$site_new <- factor(dat$site_new,
                       levels = c("Pymoor", "Railway View", "Rosedene1", "Rosedene2", "Wrights"))
levels(dat$site_new)
#
# Reorder columns
#
dat <- dat %>% select(sample_code, site_label, site, site_new, mesocosmID, Group, sample_type, Week.no., Cycle.no., Week1.2, C.W, everything())

##
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Data/LP3+_mesocosm_dat_all.csv")
#
#
#
#
# Check for each unique C.W how many unique mesocosmIDs?
dat %>%
  group_by(C.W) %>%
  summarise(n_mesocosms = n_distinct(mesocosmID))
#
#
#################################################################################
#### tap water and rain water ####
#
# Read in the tap water volume excel sheet
tap_vol <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Mesocosms/Ancillary Data/Copy of tap water additions 2_TS.csv")
#
head(tap_vol)
#
tap_vol <- tap_vol %>%
  rename(
    date = X10_Date,
    site_label = X13_Mesocosm_tag_no  )
#
#
tap_vol$date<-as.POSIXct(tap_vol$date, format="%d/%m/%Y", tz = "GMT")
#
# Select useful columns
tap_vol <- tap_vol %>% 
  select(date, site_label, tap_water_addition_L)
#
# Merge in treatment data from dat
#
# Step 1: Filter rows in dat and extract numeric site numbers, treatment and site
# Unfortunetaly can't use mescossm ID (present for BW) as BW doesn't include the treatments
dat_bw_sub <- dat %>%
  filter(grepl("^\\d+", site_label)) %>%  # keep rows starting with a number
  mutate(
    site_label = as.numeric(sub("^([0-9]+).*", "\\1", site_label))  # extract leading number
  ) %>%
  select(site_label, Group, site) %>%
  filter(!is.na(Group)) %>%
  distinct()

# Step 2: Merge into tap_vol
tap_vol <- tap_vol %>%
  left_join(dat_bw_sub, by = c("site_label"))%>%
  filter(site_label < 61) # There are some site_labels that don't have matches, these are all above 60, must be a part of another experiment, remove
#
##############################################
#### plot tap water additions ####

#jpeg("LP3+_mesocosm_tap_water_additions.jpg", units="in", width=6.5, height=4, res=300)

tap_water <- ggplot(subset(tap_vol, !is.na(Group) ), aes(x = as.Date(date), y = tap_water_addition_L, colour =Group)) + 
  labs(y = "Tap water addition (L)") +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) + 
  #geom_point() +
  theme_minimal() + theme( axis.title.x = element_blank(), legend.position = "top", legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +   scale_colour_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF")) +   scale_x_date(    breaks = date_breaks("1 week"), minor_breaks = date_breaks("1 day"),  date_labels = "%b %d") # can't seem to get ticks for each day
tap_water

#dev.off()

###


tap <- subset(dat, site_label=="Tap water")
rain <- subset(dat, site_label=="Rainwater")


summary_tap <- tap %>%
  summarise(across(    where(is.numeric),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd   = ~sd(.x,   na.rm = TRUE))   )) %>%
  pivot_longer( everything(), names_to = c("variable", ".value"), names_pattern = "^(.*)_(mean|sd)$"  )


summary_rain <- rain %>%
  summarise(across(    where(is.numeric),
                       list(mean = ~mean(.x, na.rm = TRUE),
                            sd   = ~sd(.x,   na.rm = TRUE))   )) %>%
  pivot_longer( everything(), names_to = c("variable", ".value"), names_pattern = "^(.*)_(mean|sd)$"  )





#
mean(rain$Cl_mg_l)
sd(rain$Cl_mg_l)

mean(rain$NO3_mg_l)
sd(rain$NO3_mg_l)

mean(rain$SO4_mg_l)
sd(rain$SO4_mg_l)

mean(rain$NH4_mg_l)
sd(rain$NH4_mg_l)

mean(rain$Mg_mg_l)
sd(rain$Mg_mg_l)

mean(rain$K_mg_l)
sd(rain$K_mg_l)

mean(rain$Ca_mg_l)
sd(rain$Ca_mg_l)
#################################################################################
#### Summary stats ####
# filter out tap water and rain water and bin water with Week.no. >= 38 (week 37 is baseline) 

mean(subset(dat, Week.no. >= 38 )$NO2_N_mg_l, na.rm = TRUE)
sd(subset(dat, Week.no. >= 38 )$NO2_N_mg_l, na.rm = TRUE)

mean(subset(dat, Cycle.no. == "BL")$NO3_N_mg_l, na.rm = TRUE)
sd(subset(dat,  Cycle.no. == "BL")$NO3_N_mg_l, na.rm = TRUE)

mean(subset(dat, Week.no. == 47 )$NO3_N_mg_l, na.rm = TRUE)
sd(subset(dat, Week.no. == 47 )$NO3_N_mg_l, na.rm = TRUE)

mean(subset(dat, Week.no. == 39 & site =="TP-A" )$NH4_N_mg_l, na.rm = TRUE)
sd(subset(dat, Week.no. == 39 & site =="TP-A" )$NH4_N_mg_l, na.rm = TRUE)

mean(subset(dat, Cycle.no. == "BL" & site =="TP-A" )$NH4_N_mg_l, na.rm = TRUE)
sd(subset(dat, Cycle.no. == "BL" & site =="TP-A" )$NH4_N_mg_l, na.rm = TRUE)

mean(subset(dat, Week.no. == 47 & site =="TP-A" & Group =="Rewetted")$NH4_N_mg_l, na.rm = TRUE)
sd(subset(dat, Week.no. == 47 & site =="TP-A" & Group =="Rewetted")$NH4_N_mg_l, na.rm = TRUE)

mean(subset(dat, Week.no. >= 38)$NO2_N_mg_l, na.rm = TRUE)
sd(subset(dat, Week.no. >= 38)$NO2_N_mg_l, na.rm = TRUE)


mean(subset(dat, site == "TP-A" & C.W != "BW" & C.W != "4_1" & site_label != "Tap water" & site_label != "Rainwater")$Ca_mg_l, na.rm = TRUE)

mean(subset(dat, site == "TP-A" & C.W != "BW" & C.W != "4_1" & site_label != "Tap water" & site_label != "Rainwater")$Mg_mg_l, na.rm = TRUE)

mean(subset(dat, site == "TP-A" & C.W != "BW" & C.W != "4_1" & site_label != "Tap water" & site_label != "Rainwater")$Na_mg_l, na.rm = TRUE)

mean(subset(dat, site == "WF-A" & C.W != "BW" & C.W != "4_1" & site_label != "Tap water" & site_label != "Rainwater")$Mg_mg_l, na.rm = TRUE)

mean(subset(dat, C.W != "BW" & C.W != "4_1" & site_label != "Tap water" & site_label != "Rainwater")$NO3_mg_l, na.rm = TRUE)

mean(subset(dat, C.W != "BW" & C.W != "4_1" & site_label != "Tap water" & site_label != "Rainwater")$F_mg_l, na.rm = TRUE)
#
mean(subset(dat,  C.W=="BL" & C.W != "BW")$NO3_mg_l, na.rm = TRUE)

mean(subset(dat,  Week.no.==47 & C.W != "BW")$NO3_mg_l, na.rm = TRUE)

mean(subset(dat, site == "TP-A" & C.W=="BL" & C.W != "BW")$NH4_mg_l, na.rm = TRUE)
sd(subset(dat, Group =="Fluctuating" & site == "TP-A" & Week.no.==47 & C.W != "BW")$NH4_mg_l, na.rm = TRUE)

mean(subset(dat, C.W != "BW" & site_label != "Tap water" & site_label != "Rainwater")$SO4_mg_l, na.rm = TRUE)

mean(subset(dat, site == "TP-A" & C.W=="BL")$Ca_mg_l, na.rm = TRUE)
mean(subset(dat, site == "TP-A" & C.W!="BL" & C.W!="BW")$Ca_mg_l, na.rm = TRUE)

## bin water summary statistics
dat_bw <- subset(dat, C.W=="BW") # subset bin water
mean(dat_bw$PO4_mg_l)
mean(dat_bw$PO4_P_mg_l)
#
mean(subset(dat_bw, site == "TP-A")$Ca_mg_l, na.rm = TRUE) #353.7434
#
mean(subset(dat_bw, site == "RV")$Ca_mg_l, na.rm = TRUE) 
mean(subset(dat_bw, site == "RV")$Mg_mg_l, na.rm = TRUE) 
mean(subset(dat_bw, site == "RV")$EC_us_cm, na.rm = TRUE) 
mean(subset(dat_bw, site == "RV")$NH4_mg_l, na.rm = TRUE)
#
mean(subset(dat_bw, site == "WF-A")$Al_ug_l, na.rm = TRUE) 
mean(subset(dat_bw, site != "WF-A")$Al_ug_l, na.rm = TRUE) 
mean(subset(dat_bw, site == "WF-A")$Fe_ug_l, na.rm = TRUE) 
mean(subset(dat_bw, site != "WF-A")$Fe_ug_l, na.rm = TRUE) 
mean(subset(dat_bw, site == "WF-A")$Cr_ug_l, na.rm = TRUE) 
mean(subset(dat_bw, site != "WF-A")$Cr_ug_l, na.rm = TRUE) 
#
#
# Count of total samples during experimental phase
nrow(subset(dat, Week.no. >= 38 & !is.na(Ca_mg_l)))  #325
sum(dat$NO2_mg_l > 0.055 & dat$Week.no. >= 38, na.rm = TRUE) #226

226/325*100



#################################################################################
#
#
#### TIME SERIES PLOTS ####
#
#
# plot variables by site
#
#subset dat
dat_subset <- dat %>%
  filter(!is.na(C.W), C.W != "BW")
#
plot_variable_by_site <- function(data, variable, site_col = "site_new", group_col = "Group", y_label = NULL, y_breaks = c(0, 10, 20), y_limits = c(0, 20)) {
  
  sites <- levels(data[[site_col]])
  sites <- sites[!is.na(sites)]
  
  plot_site <- function(site_name, bottom = FALSE) {
    site_data <- data[data[[site_col]] == site_name, ]
    site_data[[site_col]] <- factor(site_data[[site_col]],
                                    levels = levels(data[[site_col]]))
    if(nrow(site_data) == 0) return(NULL)
    
    current_groups <- unique(site_data[[group_col]])
    shape_map <- c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)
    shape_map <- shape_map[names(shape_map) %in% current_groups]
    
    ggplot(site_data, aes(
      x = as.numeric(Week.no.),
      y = .data[[variable]],
      shape = .data[[group_col]],
      fill = .data[[site_col]]
    )) +
      stat_summary(fun.data = "mean_se", geom = "errorbar",
                   width = 0, alpha = 0.6, color = "black",
                   position = position_dodge(width = 0.8)) +
      stat_summary(fun = "mean", geom = "point",
                   size = 3, alpha = 0.6, aes(fill = .data[[site_col]]), 
                   position = position_dodge(width = 0.8)) +
      scale_shape_manual(values = shape_map) +
      scale_fill_manual(values = c(  "Pymoor" = "#66A035" ,  "Railway View" = "#4A90E2",   "Rosedene1" = "#F4B400", "Rosedene2" = "#A347F3",   "Wrights" = "#E63978" )) +
      theme_minimal() +
      guides(  shape = guide_legend(order = 1), fill  = guide_legend(order = 2)    ) +
      theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),  
        axis.ticks.y = element_line(color = "black"), 
        axis.text.x  = element_text(size = 12),
        axis.ticks.x = element_line(color = "black"),
        axis.text.y  = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        panel.grid = element_blank()) +
      scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),
        labels = if (bottom) c("BL", "1", "2", "3", "4", "5", "9") else rep("", 7)  ) +
      scale_y_continuous( limits = y_limits,breaks = y_breaks )  }
  
  plots <- lapply(seq_along(sites), function(i) plot_site(sites[i], bottom = (i == length(sites))))
  plots <- plots[!sapply(plots, is.null)]
  
  # Create a blank plot for shared y-axis label
  if(!is.null(y_label)) {
    yaxis_plot <- ggplot() + 
      theme_void() +
      labs(y = y_label) +
      theme(
        axis.title.y = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 0.5)
      )
    combined_plot <- wrap_plots(yaxis_plot, wrap_plots(plots, ncol = 1),                                 ncol = 2, widths = c(0, 0.95)) 
  } else {
    combined_plot <- wrap_plots(plots, ncol = 1) 
  }
  
  return(combined_plot)
}
#


NO3_plot <- plot_variable_by_site(
  dat_subset,  "NO3_N_mg_l",  site_col = "site_new",
  y_label = expression(NO[3]^"-" * "-N" ~ "(mg L"^-1*")"),
  y_breaks = c(0, 10, 20), y_limits = c(0, 20) )
NO3_plot
#
NH4_plot <- plot_variable_by_site(
  dat_subset,  "NH4_N_mg_l",  site_col = "site_new",
  y_label = expression(NH[4]^"+"*"-N" ~ "(mg L"^-1*")"),
  y_breaks = c(0, 2, 4, 6), y_limits = c(0, 7) )
NH4_plot
#
PO4_plot <- plot_variable_by_site(
  dat_subset,  "PO4_P_mg_l",  site_col = "site_new",
  y_label = expression(PO[4]^{"3-"}*"-P (mg L"^-1*")"),
  y_breaks = c(0.1, 0.2, 0.3), y_limits = c(0.08, 0.3) )
PO4_plot
#
# Combine plots
#
jpeg("LP3+_mesocosm_NO3_NH4_PO4.jpeg", units="in", width=11, height=10, res=250)

combined_plot <-   (NO3_plot | NH4_plot | PO4_plot) / plot_spacer() +  
  plot_layout(    guides = "collect",    heights = c(1, 0.05)  ) &   theme( legend.position = "bottom",    legend.text  = element_text(size = 12),    legend.title = element_blank()  )
combined_plot

print(combined_plot) 

dev.off()
#add a, b, c labels in post production




combined_plot <- NO3_plot | NH4_plot | PO4_plot +
  plot_layout(guides = "collect") &  # collect legends across all plots
  theme(legend.position = "bottom",legend.box = "horizontal", legend.box.just = "center", legend.justification = "center", legend.text = element_text(size = 12),  legend.title = element_blank() )  # single shared legend at bottom
combined_plot







#
#### pH ####   


pH_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = pH)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  labs(y = expression("pH")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
pH_time


pH_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = pH, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("pH")) +
  #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )
pH_time2


#### EC_us_cm ####
EC_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = EC_us_cm)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  labs(y = expression("EC (µs cm"^"-1"*")")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
EC_time



EC_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = EC_us_cm, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("EC (µs cm"^"-1"*")")) +
  #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )
EC_time2


#### F_mg_l ####   
F_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = F_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
    scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  labs(y = expression(F^"\u2212"*" (mg L"^"-1"*")")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
F_time


F_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = F_mg_l, shape = Group, fill =site_new)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
    labs(y = expression(F^"\u2212"*" (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
F_by_site_treatment





F_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = F_mg_l, colour =site_new)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site_new) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(F^"\u2212"*" (mg L"^"-1"*")")) +
  # scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1", "Cycle 5\nWeek 2")) +
  #scale_x_discrete(labels = c("Baseline", "39", "40", "43", "47")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL","1",  "2", "3", "4", "5", "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),  legend.text  = element_text(size = 14),  panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 12), axis.title = element_text(size = 13)) +
  scale_colour_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") )  #axis.text.x = element_blank(),
F_by_site

#

F_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = F_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression(F^"\u2212"*" (mg L"^"-1"*")")) +
   # scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1", "Cycle 5\nWeek 2")) +
  #scale_x_discrete(labels = c("Baseline", "39", "40", "43", "47")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
    theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
    scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
F_by_treatment

#### Cl_mg_l ####     

#jpeg("Cl_timeseries_LP3+_mesocosm_.jpg", units="in", width=6.5, height=4, res=300)

Cl_timex <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = Cl_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
    scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  labs(y = expression(Cl^"\u2212"*" (mg L"^"-1"*")")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
Cl_timex

#dev.off()

Cl_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = Cl_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
  #  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  labs(y = expression(Cl^"\u2212"*" (mg L"^"-1"*")")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
Cl_time



Cl_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = Cl_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(Cl^"\u2212"*" (mg L"^"-1"*")")) +
  #  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) 
Cl_time2

Cl_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Cl_mg_l, shape = Group, fill =site_new)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6,  color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  labs(y = expression(Cl^"\u2212"*" (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
Cl_by_site_treatment


Cl_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Cl_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression(Cl^"\u2212"*" (mg L"^"-1"*")")) +
  # scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1", "Cycle 5\nWeek 2")) +
  #scale_x_discrete(labels = c("Baseline", "39", "40", "43", "47")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
Cl_by_treatment


Cl_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Cl_mg_l, colour =site_new)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site_new) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(Cl^"\u2212"*" (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL",  "1", "2", "3", "4", "5", "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), legend.text  = element_text(size = 14),  axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 12), axis.title = element_text(size = 13)) +
  scale_colour_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") )  #axis.text.x = element_blank(),
Cl_by_site

sub_34 <- subset(dat, Week.no. == 43 & site_new=="Rosedene2")

#### NO2_mg_l ####   
NO2_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = NO2_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
    #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_discrete( labels = c("BL", "39", "40", "43", "47")) +
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")")) +
  theme_minimal() +
  scale_y_continuous(trans = 'pseudo_log') +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
NO2_time


NO2_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = NO2_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")"), x = "Week") +
    #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme( legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )
NO2_time2

NO2_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NO2_mg_l, shape = Group, fill =site)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")")) +
  scale_y_log10() + 
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
NO2_by_site_treatment


NO2_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NO2_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")")) +
  # scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1", "Cycle 5\nWeek 2")) +
  #scale_x_discrete(labels = c("Baseline", "39", "40", "43", "47")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
NO2_by_treatment


NO2_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NO2_mg_l, colour =site)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )  #axis.text.x = element_blank(),
NO2_by_site

#### NO3_mg_l #### 
NO3_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = NO3_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
   # scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_discrete(labels = c("BL", "2", "3", "4", "5", "6", "10")) +
  labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
NO3_time

NO3_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = NO3_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")")) +
    #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41,42, 43, 47),  labels = c("BL", "2", "3", "4", "5", "6", "10"))  +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) 
NO3_time2


NO3_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NO3_N_mg_l, shape = Group, fill =site)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  labs(y = expression(NO[3]^"-" * "-N" ~ "(mg L"^-1*")")) +
   # scale_y_log10() + 
  scale_x_continuous( breaks = c(37, 39, 40, 41,42, 43, 47),  labels = c("BL", "2", "3", "4", "5", "6", "10"))  +
  theme_minimal() +
  theme(legend.position = c(.85, .65),  axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) + 
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black"))) #legend.box = "horizontal
NO3_by_site_treatment



NO3_by_site_treatment2 <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NO3_N_mg_l, shape = Group, fill =site_new)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  facet_wrap(~ site_new, ncol = 1) +
  labs(y = expression(NO[3]^"-" * "-N" ~ "(mg L"^-1*")")) +
  # scale_y_log10() + 
  scale_x_continuous( breaks = c(37, 39, 40, 41,42, 43, 47),  labels = c("BL", "2", "3", "4", "5", "6", "10"))  +
  theme_minimal() +
  theme(legend.position = c(.85, .95), panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") ) + 
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black"))) + guides(fill = "none") #legend.box = "horizontal
NO3_by_site_treatment2


NO3_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NO3_N_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression(NO[3]^"-" * "-N" ~ "(mg L"^-1*")")) +
  # scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1", "Cycle 5\nWeek 2")) +
  #scale_x_discrete(labels = c("Baseline", "39", "40", "43", "47")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41,42, 43, 47),  labels = c("BL", "2", "3", "4", "5", "6", "10"))  +
  theme_minimal() +
  theme(legend.position = c(.76, .85), axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
NO3_by_treatment


NO3_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NO3_mg_l, colour =site)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41,42, 43, 47),  labels = c("BL", "2", "3", "4", "5", "6", "10"))  +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )  #axis.text.x = element_blank(),
NO3_by_site

#### PO4_mg_l ####   
PO4_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = PO4_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
      #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_discrete( labels = c("0", "2", "3", "6", "10")) +
  labs(y = expression(PO[4]^"-" ~ "(mg L"^-1*")"),  x = "Week") +
  theme_minimal() +
  theme( legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
PO4_time



PO4_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = PO4_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(PO[4]^"-" ~ "(mg L"^-1*")"), x = "Week") +
    #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  facet_wrap(~ site_new, ncol = 1) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )
PO4_time2

PO4_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = PO4_P_mg_l, shape = Group, fill =site)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  facet_wrap(~ site_new, ncol = 1) +
  labs(y = expression(PO[4]^{"3-"}*"-P (mg L"^-1*")")) +
  # scale_y_log10() + 
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme(legend.position = "top", legend.box = "vertical", legend.spacing.y = unit(-0.5, "cm"),  axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
PO4_by_site_treatment


PO4_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = PO4_P_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression(PO[4]^{"3-"}*"-P (mg L"^-1*")")) +
  # scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1", "Cycle 5\nWeek 2")) +
  #scale_x_discrete(labels = c("Baseline", "39", "40", "43", "47")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme( legend.position = c(.25, .85),   axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
PO4_by_treatment


PO4_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = PO4_mg_l, colour =site)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(PO[4]^"-" ~ "(mg L"^-1*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )  #axis.text.x = element_blank(),
PO4_by_site

#### SO4_mg_l ####

SO4_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = SO4_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
   # scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_discrete( labels = c("BL", "39", "40","41", "42",  "43", "47")) +
  labs(y = expression(SO[4]^"-" ~ "(mg L"^-1*")"),  x = "Week") +
  theme_minimal() +
  #scale_y_continuous(trans = 'pseudo_log') +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
SO4_time


SO4_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = SO4_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(SO[4]^"-" ~ "(mg L"^-1*")"), x = "Week") +
    #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  scale_y_log10() + 
    theme( legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )
SO4_time2

SO4_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = SO4_S_mg_l, shape = Group, fill =site)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  labs(y = expression(SO[4]^"2-"*"-S" ~ "("*mg~L^{-1}*")")) +
  scale_y_log10() + 
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
SO4_by_site_treatment

SO4_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = SO4_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression(SO[4]^"-" ~ "(mg L"^-1*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
SO4_by_treatment


SO4_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = SO4_S_mg_l, colour =site_new)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site_new) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(SO[4]^"2-"*"-S" ~ "("*mg~L^{-1}*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("0", "1", "2", "3", "4", "5", "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(),  legend.text  = element_text(size = 14),  legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 12), axis.title = element_text(size = 13)) +
  scale_colour_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") )  #axis.text.x = element_blank(),
SO4_by_site

#### Li_mg_l #### 
# All 0s

#### Na_mg_l ####
Na_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = Na_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
  #  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  labs(y = expression("Na (mg L"^"-1"*")")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
Na_time

Na_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = Na_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("Na (mg L"^"-1"*")")) +
  #  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )  
Na_time2

Na_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Na_mg_l, shape = Group, fill =site)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  labs(y = expression("Na (mg L"^"-1"*")")) +
  #scale_y_log10() + 
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
Na_by_site_treatment


Na_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Na_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression("Na (mg L"^"-1"*")")) +
  # scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1", "Cycle 5\nWeek 2")) +
  #scale_x_discrete(labels = c("Baseline", "39", "40", "43", "47")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
Na_by_treatment


Na_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Na_mg_l, colour =site_new)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site_new) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("Na (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5",  "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), legend.text  = element_text(size = 14), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 12), axis.title = element_text(size = 13)) +
  scale_colour_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") )  #axis.text.x = element_blank(),
Na_by_site


#### NH4_mg_l ####
NH4_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = NH4_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
    #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_discrete(  labels = c("0", "2", "3", "6", "10")) +
  labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")") ,  x = "Week")+
  theme_minimal() +
  #scale_y_continuous(trans = 'pseudo_log') +
  #scale_y_log10() + 
  theme( legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
NH4_time


NH4_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = NH4_N_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(NH[4]^"+"*"-N" ~ "(mg L"^-1*")") , x= "Week" )+
    #scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme(legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )
NH4_time2


NH4_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NH4_N_mg_l, shape = Group, fill =site)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  facet_wrap(~ site_new, ncol = 1) +
  labs(y = expression(NH[4]^"+"*"-N" ~ "(mg L"^-1*")")) +
  #scale_y_log10() + 
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
NH4_by_site_treatment


NH4_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NH4_N_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression(NH[4]^"+"*"-N" ~ "(mg L"^-1*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme(legend.position="none", axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
NH4_by_treatment


NH4_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = NH4_mg_l, colour =site)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("0", "2", "3", "6", "10")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )  #axis.text.x = element_blank(),
NH4_by_site


#### Mg_mg_l ####
Mg_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = Mg_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
 #   scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_discrete( labels = c("BL", "39", "40", "43", "47")) +
  labs(y = expression("Mg (mg L"^"-1"*")")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
Mg_time


Mg_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = Mg_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("Mg (mg L"^"-1"*")")) +
  #  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) 
Mg_time2


Mg_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Mg_mg_l, shape = Group, fill =site)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  labs(y = expression("Mg (mg L"^"-1"*")")) +
  #scale_y_log10() + 
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
Mg_by_site_treatment


Mg_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Mg_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression("Mg (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
Mg_by_treatment


Mg_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Mg_mg_l, colour =site_new)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site_new) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("Mg (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("0", "1", "2", "3", "4", "5", "10")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), legend.text  = element_text(size = 14), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 12), axis.title = element_text(size = 13)) +
  scale_colour_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") )  #axis.text.x = element_blank(),
Mg_by_site

#### K_mg_l ####
K_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = K_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
  #  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  labs(y = expression("K (mg L"^"-1"*")")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
K_time


K_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = K_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("K (mg L"^"-1"*")")) +
  #  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme(axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") )
K_time2


K_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = K_mg_l, shape = Group, fill =site_new)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0, alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  labs(y = expression("K (mg L"^"-1"*")")) +
  #scale_y_log10() + 
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
K_by_site_treatment

K_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = K_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression("K (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
K_by_treatment


K_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = K_mg_l, colour =site_new)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site_new) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("K (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(),  legend.title = element_blank(),  legend.text  = element_text(size = 14), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 12), axis.title = element_text(size = 14)) +
  scale_colour_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") )  #axis.text.x = element_blank(),
K_by_site

#### Ca_mg_l ####
Ca_time <- ggplot(subset(dat, !is.na(C.W)& C.W != "BW" & C.W != "4_1"), aes(x = as.factor(Week.no.), y = Ca_mg_l)) +
  geom_boxplot(aes(fill=Group), position = position_dodge(width = 0.5),  outlier.shape = NA,   width = 0.45) + 
  geom_jitter(aes(fill=Group), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5), size = 2, alpha = 0.6, shape=21, colour="black") +
  #  scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  labs(y = expression("Ca (mg L"^"-1"*")")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
Ca_time


#dat$site <- factor(dat$site, levels = unique(dat$site)) # attempt to help plot all of the sites

Ca_time2 <- ggplot(subset(dat, !is.na(C.W )& C.W != "BW" & C.W != "4_1"), aes(x = Week.no., y = Ca_mg_l, shape = Group, colour =site)) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("Ca (mg L"^"-1"*")")) +
 #   scale_x_discrete(labels = c("Baseline", "Cycle 1\nWeek 1", "Cycle 1\nWeek 2", "Cycle 3\nWeek 1",   "Cycle 5\nWeek 2")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_colour_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035"), drop = FALSE )
Ca_time2


Ca_by_site_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Ca_mg_l, shape = Group, fill =site)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width = 0.8), 
               width = 0,  alpha=0.6, color = "black") +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(width = 0.8),  alpha=0.6, size = 3) +
  labs(y = expression(Ca^"\u2212"*" (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 43, 47),  labels = c("BL", "39", "40", "43", "47")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_shape_manual(values = c("Conventional drainage" = 21, "Fluctuating" = 24, "Rewetted" = 22)) +
  scale_fill_manual(values = c("RG-R8" = "#A347F3", "RG-PEF" = "#F4B400", "WF-A" = "#E63978",  "RV" = "#4A90E2",  "TP-A" ="#66A035") ) + guides(fill = guide_legend(override.aes = list(shape = 21, size = 4, color = "black")))
Ca_by_site_treatment

Ca_by_treatment <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Ca_mg_l,  fill =Group)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = Group) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7, shape= 21, colour="black") +
  labs(y = expression("Ca (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5" , "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 10.5), axis.title = element_text(size = 12)) +
  scale_fill_manual(values = c("#FF8A65",  "#81C784", "#7C9DBF"))
Ca_by_treatment


Ca_by_site <- ggplot(subset(dat, !is.na(C.W ) & C.W != "BW" & C.W != "4_1"), aes(x = as.numeric(Week.no.), y = Ca_mg_l, colour =site_new)) +
  stat_summary( fun = mean, geom = "line", position = position_dodge(width = 0.5),  linewidth = 0.8, aes(color = site_new) ) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", position = position_dodge(width = 0.5),  alpha=0.6, size=0.7) +
  labs(y = expression("Ca (mg L"^"-1"*")")) +
  scale_x_continuous( breaks = c(37, 39, 40, 41, 42, 43, 47),  labels = c("BL", "1", "2", "3", "4", "5", "9")) +
  theme_minimal() +
  theme( axis.title.x = element_blank(), legend.title = element_blank(), legend.text  = element_text(size = 14), axis.line = element_line(colour = "black"),    panel.grid = element_blank(), axis.ticks = element_line(colour = "black"), axis.text = element_text(size = 12), axis.title = element_text(size = 13)) +
  scale_colour_manual(values = c("Rosedene2" = "#A347F3", "Rosedene1" = "#F4B400", "Wrights" = "#E63978",  "Railway View" = "#4A90E2",  "Pymoor" ="#66A035") )  #axis.text.x = element_blank(),
Ca_by_site

#### P_mg_l #### 
# No data

#### Si_mg_l ####
# No data

#### Al_ug_l ####   
# No data

#### As_ug_l ####
# No data

#### Cd_ug_l #### 
# No data

#### Cr_ug_l ####  
# No data

#### Cu_ug_l #### 
# No data

#### Fe_ug_l ####
# No data

#### Mn_ug_l #### 
# No data

#### Ni_ug_l #### 
# No data

#### Pb_ug_l #### 
# No data

#### Zn_ug_l #### 
# No data

############## COMBINE time series plots ################################
#
# combine by element #
#
jpeg("LP3+_mesocosm_timeseries_N.jpeg", units="in", width=11, height=8, res=250)
combine1 <- plot_grid(   NO3_by_treatment, NO3_by_site_treatment,  
                         NH4_by_treatment + theme(legend.position = "none"), 
                         NH4_by_site_treatment + theme(legend.position = "none"), 
                      ncol = 2, nrow = 2, labels = c("(a)", "(b)", "(c)", "(d)"), rel_widths = c(1, 1.6))  
combine1
dev.off()
#
jpeg("LP3+_mesocosm_timeseries_PO4.jpeg", units="in", width=11, height=4, res=250)
combine1 <- plot_grid(   PO4_by_treatment, PO4_by_site_treatment,  
                         ncol = 2, nrow = 1, labels = c("(a)", "(b)"), rel_widths = c(1, 1.6))  
combine1
dev.off()
#
#
#### Site time series ####
#
# for publication:
jpeg("LP3+_mesocosm_site_timeseries.jpeg", units="in", width=8, height=10, res=250)
combine1 <- ggarrange(K_by_site , SO4_by_site, 
                      Ca_by_site, Mg_by_site, 
                      Na_by_site,   Cl_by_site, 
                      F_by_site,     
                      ncol = 2, nrow = 4, align="hv",common.legend = T, 
                      labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)"))  
combine1
dev.off()
#
jpeg("LP3+_mesocosm_site_timeseries1.jpeg", units="in", width=10, height=8, res=250)
combine1 <- ggarrange(Ca_by_site , Cl_by_site, F_by_site, K_by_site, 
                      Mg_by_site,   Na_by_site,     
                      ncol = 2, nrow = 3, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine1
dev.off()

jpeg("LP3+_mesocosm_site_timeseries2.jpeg", units="in", width=10, height=8, res=250)
combine2 <- ggarrange(NH4_by_site, NO2_by_site, NO3_by_site, PO4_by_site, SO4_by_site,  
                      ncol = 2, nrow = 3, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine2
dev.off()
#
#
#### Treatment time series ####
# for publication
jpeg("LP3+_mesocosm_treatment_timeseries.jpeg", units="in", width=8, height=10, res=250)
combine1 <- ggarrange(K_by_treatment , SO4_by_treatment, 
                      Ca_by_treatment, Mg_by_treatment, 
                      Na_by_treatment,   Cl_by_treatment, 
                      F_by_treatment,     
                      ncol = 2, nrow = 4, align="hv",common.legend = T, 
                      labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)"))  
combine1
dev.off()
#

jpeg("LP3+_mesocosm_treatment_timeseries_boxplots1.jpeg", units="in", width=10, height=8, res=250)
combine1 <- ggarrange(Ca_by_treatment , Cl_by_treatment, F_by_treatment, K_by_treatment, 
                      Mg_by_treatment,   Na_by_treatment,     
                      ncol = 2, nrow = 3, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine1
dev.off()

jpeg("LP3+_mesocosm_treatment_timeseries_boxplots2.jpeg", units="in", width=10, height=8, res=250)
combine2 <- ggarrange(NH4_by_treatment, NO2_by_treatment, NO3_by_treatment, PO4_by_treatment, SO4_by_treatment,  
                      ncol = 2, nrow = 3, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine2
dev.off()

########################################################################################
#### Treatment AND site time series ####
# for publication 
jpeg("LP3+_mesocosm_site_treatment_timeseries_K_Cl_F.jpeg", units="in", width=8.5, height=10, res=250)
combine1 <- ggarrange(K_by_site_treatment , Cl_by_site_treatment, F_by_site_treatment,     
                      ncol = 1, nrow = 3, align="hv",common.legend = T, labels = c("(a)", "(b)", "(c)"))  
combine1
dev.off()
#


jpeg("LP3+_mesocosm_site_treatment_timeseries_1.jpeg", units="in", width=8, height=10, res=250)

combine1 <- ggarrange(Ca_by_site_treatment , Cl_by_site_treatment, F_by_site_treatment,     
                      ncol = 1, nrow = 3, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine1

dev.off()
#
#
jpeg("LP3+_mesocosm_site_treatment_timeseries_2.jpeg", units="in", width=8, height=10, res=250)

combine2 <- ggarrange(K_by_site_treatment, Mg_by_site_treatment, Na_by_site_treatment, 
                      ncol = 1, nrow = 3, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine2

dev.off()


jpeg("LP3+_mesocosm_site_treatment_timeseries_3.jpeg", units="in", width=8, height=10, res=250)

combine3 <- ggarrange(NH4_by_site_treatment, NO2_by_site_treatment, NO3_by_site_treatment, 
                      ncol = 1, nrow = 3, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine3

dev.off()


jpeg("LP3+_mesocosm_site_treatment_timeseries_4.jpeg", units="in", width=8, height=6.5, res=250)

combine4 <- ggarrange(PO4_by_site_treatment, SO4_by_site_treatment,  
                      ncol = 1, nrow = 2, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine4

dev.off()

#####################################
# Point plots:
jpeg("LP3+_mesocosm_timeseries_combined1.jpeg", units="in", width=10, height=12, res=250)

combine1 <- ggarrange(Ca_time2 , Cl_time2, F_time2, K_time2, 
                      Mg_time2,   Na_time2,  NH4_time2, NO2_time2,    
                     ncol = 2, nrow = 4, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine1

dev.off()  # Now that we have Ca data, no need to use illustrator to add all points in legend



jpeg("LP3+_mesocosm_timeseries_combined2.jpeg", units="in", width=10, height=5, res=250)

combine2 <- ggarrange(NO3_time2, PO4_time2, SO4_time2,  
                      ncol = 2, nrow = 2, align="hv",common.legend = T) # labels = c("(a)", "(b)", "(c)")
combine2

dev.off()

##############################################################################
#### STATISTICAL ANALYSIS ####
# We are interested in the effect of time, site, and treatment

lmer_NO3 <- lmer(log(NO3_N_mg_l) ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_NO3)
anova(lmer_NO3)
plot(lmer_NO3) # log improves
qqnorm(resid(lmer_NO3)) 
qqline(resid(lmer_NO3)) 
emmeans(lmer_NO3, ~ Group | site)
emmeans(lmer_NO3, pairwise ~ Group) 
emmeans(lmer_NO3, pairwise ~ site) 
emtrends(lmer_NO3, var = "Week.no.", specs = "Group") # compare slopes
exp(0.161)
exp(0.171)
exp(0.291)

lmer_NH4 <- lmer(log(NH4_N_mg_l)  ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_NH4)
anova(lmer_NH4)
plot(lmer_NH4) 
qqnorm(resid(lmer_NH4)) #log is better
qqline(resid(lmer_NH4))
emmeans(lmer_NH4, pairwise ~ Group)
emmeans(lmer_NH4, pairwise ~ site)
emmeans(lmer_NH4, pairwise ~ site)
emtrends(lmer_NH4, var = "Week.no.", specs = "Group") # compare slopes
exp(0.0523)
exp(0.2128)
exp(0.1944)


lmer_PO4 <- lmer(log(PO4_P_mg_l)  ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_PO4)
anova(lmer_PO4)
plot(lmer_PO4) 
qqnorm(resid(lmer_PO4)) 
qqline(resid(lmer_PO4)) # log improves
emmeans(lmer_PO4, pairwise ~ Group)
emmeans(lmer_PO4, pairwise ~ Group | site)

lmer_K <- lmer(log(K_mg_l) ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_K)
anova(lmer_K) # site sig, group not sig
plot(lmer_K) # log improves, maybe 1 outlier?
qqnorm(resid(lmer_K))
qqline(resid(lmer_K))

lmer_SO4 <- lmer(log(SO4_S_mg_l) ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_SO4)
anova(lmer_SO4) # site sig, group not sig
plot(lmer_SO4) # log improves
qqnorm(resid(lmer_SO4))
qqline(resid(lmer_SO4))

lmer_Ca <- lmer(log(Ca_mg_l) ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_Ca)
anova(lmer_Ca) # site sig, group not sig, week significant
plot(lmer_Ca) # log improves, maybe 1 outlier?
qqnorm(resid(lmer_Ca))
qqline(resid(lmer_Ca))

lmer_Mg <- lmer(log(Mg_mg_l) ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_Mg)
anova(lmer_Mg) # site sig, group not sig
plot(lmer_Mg) # log improves
qqnorm(resid(lmer_Mg))
qqline(resid(lmer_Mg))

lmer_Na <- lmer(log(Na_mg_l) ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_Na)
anova(lmer_Na) # site sig, group not sig
plot(lmer_Na) # log improves
qqnorm(resid(lmer_Na))
qqline(resid(lmer_Na))

lmer_Cl <- lmer(Cl_mg_l ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_Cl)
anova(lmer_Cl) # site sig group not sig
plot(lmer_Cl) # kinda equivalent if not slightly worse after log
qqnorm(resid(lmer_Cl))
qqline(resid(lmer_Cl))

lmer_F <- lmer(F_mg_l ~ site * Group * Week.no. + (1 | mesocosmID), data = subset(dat, Week.no. >= 38))
summary(lmer_F)
anova(lmer_F) # site sig, group sig
plot(lmer_F) # good
qqnorm(resid(lmer_F))
qqline(resid(lmer_F))
emmeans(lmer_F, pairwise ~ Group)
emmeans(lmer_PO4, pairwise ~ Group | site)

##############################################################################
#
#
#### PLOTS FOR BIN WATER ####
#
#
#
colnames(dat)
#### pH ####
#tiff("pH_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

pH <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y = pH, fill=site_new)) + 
  geom_boxplot(width = 0.9, alpha=0.7) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs( y = "pH",  x = NULL ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=13), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
  scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
pH

#dev.off()

#### EC ####

#tiff("EC_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

EC <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y = EC_us_cm, fill=site_new)) + 
  geom_boxplot(width = 0.9, alpha=0.7) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(  y = expression("EC (" * mu * "S cm"^"-1" * ")"),  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +  scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
EC

#dev.off()

#### DOC ####
TOC <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y = TOC_mg_l, fill=site_new)) + 
  geom_boxplot(width = 0.9, alpha=0.7) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("DOC (mg L"^-1*")"),  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +  scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
TOC

#### Fluoride ####

#tiff("Fluoride_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Fluo <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y = F_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 0.15 - 0.14,
               ymax = 0.15 + 0.14,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) + # Add boxplot without showing outliers
  geom_hline(yintercept = 0.15, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("F- (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) +  scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") ) 
Fluo

#dev.off()


#### Chloride ####

#tiff("Chloride_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Cl <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Cl_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 24.7 - 1.3,
               ymax = 24.7 + 1.3,    fill = "#1F51FF",    alpha = 0.15  ) +
    geom_boxplot(width = 0.9) + 
  geom_hline(yintercept = 24.7, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Cl- (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Cl

#dev.off()



#### Nitrite	#### 
#tiff("Nitrite_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NO2 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =NO2_N_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 0.11 - 0.08,
               ymax = 0.11 + 0.08,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) + 
  geom_hline(yintercept = 0.11, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NO[2]^"-" * "-N" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
NO2

#dev.off()


#### Nitrate ####

#tiff("Nitrate_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NO3 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =NO3_N_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 7.1 - 0.19,
               ymax = 7.1 + 0.19,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) +
  geom_hline(yintercept = 7.1, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(NO[3]^"-" * "-N" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=13), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
NO3

#dev.off()

31.61/62*14.01 # 7.14284 is tap water on per mol basis


#### Phosphate	####

#tiff("Phosphate_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

PO4 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =PO4_P_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 0.59 - 0.27,
               ymax = 0.59 + 0.27,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  geom_hline(yintercept = 0.59, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  labs(y = expression(PO[4]^{"3-"}*"-P (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1, 2), labels = scales::label_number() )  +
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=13), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
PO4

2.26/94.97*30.97 #0.7369927 is tap water on per molecule basis


#dev.off()

#### Sulfate	#### 

#tiff("Sulfate_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

SO4 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =SO4_S_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 10 - 0.70,
               ymax = 10 + 0.70,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) + # Add boxplot without showing outliers
  # geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  geom_hline(yintercept = 10, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  labs(y = expression(SO[4]^"2-"*"-S" ~ "("*mg~L^{-1}*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=13), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
SO4

28.94/96.06*32.06 # is tap water on per mol basis

#dev.off()

#### Lithium	####
## all 0

#### Sodium	####

#tiff("Sodium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Na <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Na_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 17.04 - 0.80,
               ymax = 17.04 + 0.80,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) + # Add boxplot without showing outliers
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  geom_hline(yintercept = 17.04, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  labs(y = expression("Na (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11) , axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Na

#dev.off()


#### Ammonium	####


#tiff("Ammonium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

NH4 <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =NH4_N_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 0.13 - 0.10,
               ymax = 0.13 + 0.10,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) + # 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  geom_hline(yintercept = 0.13, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  labs(y = expression(NH[4]^"+"*"-N" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  #scale_y_log10() + 
  scale_y_continuous(trans = 'pseudo_log') +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
NH4

#dev.off()
0.079/18.04*14.01 #0.061352 is tap water conc on per mol basis
0.29/18.04*14.01
#### Magnesium	#### 

#tiff("Magnesium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Mg <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Mg_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 2.7 - 0.11,
               ymax = 2.7 + 0.11,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  geom_hline(yintercept = 2.7, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  labs(y = expression("Mg (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Mg

#dev.off()


#### Potassium	#### 

#tiff("Potassium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

K <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =K_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 2.4 - 0.20,
               ymax = 2.4 + 0.20,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  geom_hline(yintercept = 2.4, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  labs(y = expression("K (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
K

#dev.off()


#### Calcium	#### 

#tiff("Calcium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Ca <- ggplot(subset(dat, Cycle.no. == "BW"),aes(x = site_new, y =Ca_mg_l, fill=site_new)) + 
  annotate(    "rect",    xmin = -Inf, xmax = Inf,    ymin = 112.3 - 1.4,
               ymax = 112.3 + 1.4,    fill = "#1F51FF",    alpha = 0.15  ) +
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  geom_hline(yintercept = 112.3, linetype = "dashed", linewidth=1, colour="#1F51FF") +  #dashed line for tap water mean
  labs(y = expression("Ca (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(axis.text.x = element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14),  axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") ) #axis.text.x = element_text(angle = 45, hjust = 1, size=11),
Ca

#dev.off()

#### Phosphorous ####


#tiff("Phosphorous_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

P <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =P_mg_l, fill=site_new)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(P ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  #scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=13), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
P

#dev.off()

#### Silicon ####

#tiff("Si_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Si <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Si_mg_l, fill=site_new)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression(Si ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
 # scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=13), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Si

#dev.off()


#### Aluminium	#####

#tiff("Aluminium_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Al <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Al_ug_l, fill=site_new)) + 
  geom_boxplot(width = 0.9) + #
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Al (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  #scale_y_continuous(trans = 'pseudo_log') +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Al

#dev.off()


#### As ####
# all NA 

#### Cd	####
# all NA 

#### Cr	#### 
Cr <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Cr_ug_l, fill=site_new)) + 
  geom_boxplot(width = 0.9) + #
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Cr (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  #scale_y_log10() + 
  scale_y_continuous(trans = 'pseudo_log') +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Cr


#### Copper	####

#tiff("Copper_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Cu <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Cu_ug_l, fill=site_new)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Cu (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Cu

#dev.off()


#### Iron	#### 

#tiff("Iron_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Fe <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Fe_ug_l, fill=site_new)) +
  geom_boxplot(width = 0.9) + #
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Fe (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  #scale_y_continuous(trans = 'pseudo_log') +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Fe

#dev.off()

#### Fe:P ####

dat$Fe_P_ratio <- (dat$Fe_ug_l / 1e6 / 55.845) / (dat$P_mg_l / 1000 / 30.974)

Fe_P <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y = Fe_P_ratio , fill=site_new)) +
  geom_boxplot(width = 0.9) + #
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Fe:P"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  #scale_y_continuous(trans = 'pseudo_log') +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=11), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Fe_P

dat %>%
  filter(Cycle.no. == "BW") %>%
  group_by(site) %>%
  dplyr::summarize(mean_Fe_P_ratio = mean(Fe_P_ratio), na.rm = TRUE)

#### Manganese	####

#tiff("Manganese_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Mn <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Mn_ug_l, fill=site_new)) + 
  geom_boxplot(width = 0.9) + 
  #geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Mn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_log10() + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Mn

#dev.off()


#### Nickel	#### 

#tiff("Nickel_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Ni <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Ni_ug_l, fill=site_new)) + 
  geom_boxplot(width = 0.9) + 
 # geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Ni (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_blank(), axis.text.y = element_text(size=11), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Ni

#dev.off()


#### Pb ####
# all NA 


#### Zn	#### 
#tiff("Zinc_LP3+_mesocosm_BW.tiff", units="in", width=6.5, height=4, res=300)

Zn <- ggplot(subset(dat, Cycle.no. == "BW"), aes(x = site_new, y =Zn_ug_l, fill=site_new)) + 
  geom_boxplot(width = 0.9) + 
 # geom_jitter(alpha=0.5, size = 3, width = 0.2) + # Jitter points to show individual observations
  labs(y = expression("Zn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_log10() + 
  theme( panel.border = element_rect(color = "black", fill = NA, size = 1), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none", axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=12), axis.text.y = element_text(size=13), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Rosedene2" = "#D8B4F8", "Rosedene1" = "#FDE68A", "Wrights" = "#F8C8DC",  "Railway View" = "#A2D2FF",  "Pymoor" ="#B5E48C") )
Zn

#dev.off()

#### combine BW plots ####

jpeg("LP3+_mesocosm_BW_combined2.jpeg", units="in", width=11, height=13, res=200)

combine <- ggarrange(Al, Ca, Cl, Cu, Cr, EC, Fluo, Fe, K, 
                     Mg, Mn, Na, NH4, Ni, NO2, NO3, P, pH, 
                     PO4, Si, SO4, TOC, Zn, 
                     ncol = 4, nrow = 6, align="hv",common.legend = F) # labels = c("(a)", "(b)", "(c)")
combine

dev.off()

#### combine the BW plots that have tap water comparison data ####
jpeg("LP3+_mesocosm_BW_combined_tap_water.jpeg", units="in", width=11, height=8.5, res=200)

combine <- ggarrange(Ca, Cl, Fluo, K, 
                     Mg, Na, NH4, NO2, 
                     NO3,  PO4,  SO4,   
                     ncol = 4, nrow = 3, align="hv",   common.legend = F,  padding = 0, heights = c(0.7, 0.7, 1.1), widths = c(1.2, 1, 1.2, 1), labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)", "(j)", "(k)" )) # 
combine

dev.off()


jpeg("LP3+_mesocosm_BW_combined_tap_water.jpeg", units="in", width=8, height=11, res=200)

combine <- plot_grid(
  Ca, Cl, Fluo, K,
  Mg, Na, NH4, NO2,
  NO3, PO4, SO4, NULL,  # NULL fills the empty spot in 3x4
  ncol = 3, align = "hv",
  rel_heights = c(1.5, 1.5, 1.5),
  labels = letters[1:11] )
combine

dev.off()







#### combine the BW plots that DO NOThave tap water comparison data ####

jpeg("LP3+_mesocosm_BW_combined_other.jpeg", units="in", width=11, height=8, res=200)

combine <- ggarrange(Al, Cu, Cr, TOC, EC, 
                     Fe, Mn, Ni, P, 
                     pH, Si,   Zn,  
                     ncol = 4, nrow = 3, align="hv",common.legend = F, labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)", "(i)", "(j)", "(k)", "l" )) # 
combine

dev.off()

###############################################################################
#### test for differences between sites ####
#
#
# 1. Test for normality using shapiro wilks test. If the p-value is less than 0.05, you reject the null hypothesis, suggesting the data is not normally distributed.
dat_bw <- subset(dat, C.W=="BW") # subset bin water
#
# remove rows with same number
dat_num <- dat_bw %>% select(where(is.numeric))%>%
  select(-mesocosmID, -box_number, -Week.no., -Week1.2, -As_ug_l, -Cd_ug_l, -Pb_ug_l, Li_mg_l) 
#
#
shapiro_results <- sapply(dat_num, shapiro.test)
# the majority are non-normal, so go with non-parametric tests
#
#
# 2. Use Kruskal-Wallis test to see differences between sites. If p<0.05 there is a significant difference between groups
#
#kruskal_results <- sapply(dat[, c(4:11, 13:18, 22:25, 27)], function(x) kruskal.test(x ~ dat$site))
#
kruskal_results <- dat_num %>%
  map(~ kruskal.test(.x ~ dat_bw$site))
#
kruskal_p_values <- kruskal_results %>%
  map_dbl(~ .x$p.value)
#
# Significant differences for: pH, EC, TOC, F, Cl, PO4, SO4, Na, NH4, Mg, K, Ca, Al, Cu, Fe, Mn
# Not sig for NO3 (0.169)
#
#
# 3. Run pairwise Wilcoxon rank sum test
#
# Identify significant variables
significant_vars <- names(kruskal_p_values)[kruskal_p_values < 0.05]

# Run pairwise Wilcoxon tests on significant variables
pairwise_results <- dat_bw %>%
  select(all_of(significant_vars)) %>%
  map(~ pairwise.wilcox.test(.x, dat_bw$site, p.adjust.method = "bonferroni", exact = FALSE))
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
##### Run PCA for BW ####
#
# Select numerical columns (excluding variables with many Na's and lot of 0s) like exclude NH4, Ni, and Zn
#dat_num <- dat_bw[, c(8, 10:15, 17, 19:24, 28:30)]   # also exclude pH and EC, as they're not in the other weeks
# Now that we are using LLD/2, you can just select the numerical columns
dat_bw <- subset(dat, C.W=="BW") # subset bin water
#
dat_num <- dat_bw %>% select(where(is.numeric))%>%
  select(-mesocosmID, -box_number, -Week.no., -Week1.2, -As_ug_l, -Cd_ug_l, -Pb_ug_l, -Li_mg_l) %>% # and get rid of columns with all or most values <LLD
select(-SO4_mg_l, -NH4_mg_l, -NO3_mg_l, -NO2_mg_l, -PO4_mg_l ) #get rid of non per molecular basis versions of N, P, S
#
# There are just a couple NA values, fill with mean (2 in F_mg_l, 2 in Fe_ug>l, 1 SO4_mg_l, 1 Cu_mg_l, 1 in NH4)
#dat_num$SO4_mg_l[is.na(dat_num$SO4_mg_l)] <- mean(dat_num$SO4_mg_l, na.rm = TRUE)
#dat_num$NH4_mg_l[is.na(dat_num$NH4_mg_l)] <- mean(dat_num$NH4_mg_l, na.rm = TRUE)
dat_num$SO4_S_mg_l[is.na(dat_num$SO4_S_mg_l)] <- mean(dat_num$SO4_S_mg_l, na.rm = TRUE)
dat_num$NH4_N_mg_l[is.na(dat_num$NH4_N_mg_l)] <- mean(dat_num$NH4_N_mg_l, na.rm = TRUE)
#
#
# Remove units from column names
colnames(dat_num) <- gsub("_ug_l|_mg_l", "", colnames(dat_num))
dat_num <- dat_num %>% rename(EC = EC_us_cm) #rename EC
dat_num <- dat_num %>% rename(DOC = TOC) #rename TOC
dat_num <- dat_num %>% rename("NO3-N" = NO3_N) 
dat_num <- dat_num %>% rename("NO2-N" = NO2_N)
dat_num <- dat_num %>% rename("NH4-N" = NH4_N)
dat_num <- dat_num %>% rename("PO4-P" = PO4_P)
dat_num <- dat_num %>% rename("SO4-S" = SO4_S)
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
# Visualize the PCA with site as a grouping factor
#
my.col.var <- c( "#66A035", "#4A90E2", "#F4B400", "#A347F3",  "#E63978") #set colour palette
#
#
#
tiff("PCA_LP3+_mesocosm_BW.tiff", units="in", width=8, height=6, res=300)

PCA_fig_bw <- fviz_pca_biplot(pca_result, 
                           col.ind = dat_bw$site_new,
                           addEllipses = TRUE, label = "var",
                           pointsize=3, alpha.ind=0.7,
                           mean.point=F, palette=my.col.var,
                           col.var = "black", repel = T, labelsize=4,
                           legend.title = " ") + ggtitle(NULL) +
                           xlab("PC1 (35.2%)") +   ylab("PC2 (18.4%)") + #update these values if you change pca    
                           scale_shape_manual(values = c(16, 17, 15, 18, 8)) +
                           theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.text = element_text(color = "black", size=14), axis.title = element_text(color = "black"), legend.position = c(.105, .22), legend.text = element_text(size = 14), axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14) )
PCA_fig_bw

dev.off()
#
# Interpretation: If sites cluster tightly in PCA space, it suggests they have similar water quality profiles. The further apart the clusters are, the more distinct the site differences.
#
# The longer the arrow for a variable, the more it influences the principal component. The direction of the arrow shows the correlation with the component, and the length indicates how much it "drives" that component.
#
# Variables that point in the same direction (or are aligned) are positively correlated with each other, while those in opposite directions are negatively correlated
#
#
##
##### PCA for baseline pore water ####
### Subset baseline data
dat_bl <- subset(dat, C.W=="BL") # subset bin water
#
## Select numerical columns (excluding variables with Na lot of 0s
dat_num_bl <- dat_bl[, c(10:15, 17:21)]
#
# previously when there were NAs, now updated with 0s: 
#
# There are just a couple NA values, fill with mean 
#dat_num$pH[is.na(dat_num$pH)] <- mean(dat_num$pH, na.rm = TRUE)
#dat_num$EC_us_cm[is.na(dat_num$EC_us_cm)] <- mean(dat_num$EC_us_cm, na.rm = TRUE)
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
#tiff("PCA_LP3+_mesocosm_BL.tiff", units="in", width=8, height=6, res=300)
PCA_fig_bl <- fviz_pca_biplot(pca_result, 
                           col.ind = dat_bl$site,
                           addEllipses = TRUE, label = "var",
                           pointsize=3, alpha.ind=0.5, labelsize = 3, 
                           mean.point=F, palette=my.col.var,
                           col.var = "black", repel = TRUE,
                           legend.title = " ") + ggtitle(NULL) +
  theme( plot.background = element_rect(color = "black", fill = NA, linewidth = 1), text = element_text(size = 9) )
PCA_fig_bl
#dev.off()
#
#
#
##
### PCA for Cycle 1_1 
dat_C1_1 <- subset(dat, C.W=="1_1") # subset Cycle 1 Week 1
#
## Select numerical columns (excluding variables with Na lot of 0s
dat_num <- dat_C1_1[, c(6:7, 9:11, 13:17)]
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
# Visualize the PCA with site as a grouping factor
#
my.col.var <- c("#4A90E2", "#A347F3", "#F4B400", "#66A035", "#E63978") #set colour palette

#
#
#tiff("PCA_LP3+_mesocosm_BL.tiff", units="in", width=8, height=6, res=300)
PCA_fig_C1_W1 <- fviz_pca_biplot(pca_result, 
                              col.ind = dat_C1_1$site,
                              addEllipses = TRUE, label = "var",
                              pointsize=3, alpha.ind=0.5, labelsize = 3, 
                              mean.point=F, palette=my.col.var,
                              col.var = "black", repel = TRUE,
                              legend.title = " ") + ggtitle(NULL) +
  theme( plot.background = element_rect(color = "black", fill = NA, linewidth = 1), text = element_text(size = 9) )
PCA_fig_C1_W1
#dev.off()
#
#
####################################################################
### PCA for Cycle 1 Week 2 
dat_C1_2 <- subset(dat, C.W=="1_2") # subset Cycle 1 Week 1
#
## Select numerical columns (excluding variables with Na lot of 0s
dat_num <- dat_C1_2[, c(6:7, 9:11, 13:17)]
#
# There are just anNA value, fill with mean 
dat_num$PO4_mg_l[is.na(dat_num$PO4_mg_l)] <- mean(dat_num$PO4_mg_l, na.rm = TRUE)
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
#tiff("PCA_LP3+_mesocosm_BL.tiff", units="in", width=8, height=6, res=300)
PCA_fig_C1_W2 <- fviz_pca_biplot(pca_result, 
                                 col.ind = dat_C1_2$site,
                                 addEllipses = TRUE, label = "var", labelsize = 3, 
                                 pointsize=3, alpha.ind=0.5,
                                 mean.point=F, palette=my.col.var,
                                 col.var = "black", repel = TRUE,
                                 legend.title = " ") + ggtitle(NULL) +
  theme( plot.background = element_rect(color = "black", fill = NA, linewidth = 1) ,text = element_text(size = 10))
PCA_fig_C1_W2
#dev.off()
#################################################################################
##### COMBINE PCAs #########################
#
# 
jpeg("LP3+_mesocosm_PCAs.jpeg", units="in", width=7, height=7, res=200)

combine_PCA <- ggarrange(PCA_fig_bl , PCA_fig_C1_W1, PCA_fig_C1_W2,     
                      ncol = 2, nrow = 2, align="hv",common.legend = T, labels = c("BL", "C1 W1", "C1 W2"))
combine_PCA

dev.off()

################################################################################
#### correlation plot for BW ####
#
# subset BW
dat_bw <- subset(dat, C.W=="BW") # subset bin water
#
# Subset numeric columns
dat_numeric <- select(dat_bw, where(is.numeric))
#
# Drop columns with all NAs
dat_numeric <- dat_numeric %>% select(where(~ !all(is.na(.))))
#
# Drop non-relevant columns
dat_numeric <- dat_numeric %>% 
  select(-mesocosmID,  -As_ug_l, -Cd_ug_l, -Pb_ug_l, -Li_mg_l, -Fe_P_ratio, -NO3_N_mg_l, -NH4_N_mg_l, -PO4_P_mg_l, -NO2_N_mg_l, -SO4_S_mg_l )
#
#
#
# Remove units from column names
colnames(dat_numeric) <- gsub("_ug_l|_mg_l|_us_cm", "", colnames(dat_numeric))
#
# Update labels
names(dat_numeric)[names(dat_numeric) == 'NP_ratio'] <- 'N:P'
#
# make data frame into matrix
dat_matrix <- rcorr(as.matrix(dat_numeric),  type = "spearman")
#
cor_mat <- dat_matrix$r
p_mat <- dat_matrix$P
#
# Make correlation plot
jpeg("LP3+_mesocosm_BW_corrplot_spearman.jpeg", units="in", width=6, height=5.5, res=300)

corrplot(cor_mat,  type="upper", diag=FALSE, tl.col = "black", tl.cex = 0.9, order = "hclust", tl.srt=45, p.mat = p_mat, sig.level = 0.05, insig = "blank") 

dev.off()




