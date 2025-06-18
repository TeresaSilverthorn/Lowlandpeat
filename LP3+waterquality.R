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
library(lme4)
library(lmerTest)
library(tidyr)
library(ggtern)
library(factoextra)
library(data.table)
library(maps)
#
#
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
(303/324)*100
#
# load in data
#
dat1 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C111 20250311_TS.xlsx", range= "A10:AJ54") # Figure out what these WF site codes refer to (WF-A vs WF-B)
#
head(dat1)  
#
dat2 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C112 20250325_TS.xlsx", range= "A10:AJ48")   # I am not sure about the dates on this one, need to double check
#
head(dat2)
#
dat3 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C102 20250306_TS.xlsx", range= "A10:AK64")
#

#
#
dat4 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C114 20250331_TS.xlsx", range= "A10:AK45")
#
head(dat4)
#
dat4$coordinates <- as.character(dat4$coordinates) #change to match others
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
dat7 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C109 20250424_TS.xlsx", range= "A10:AJ47")
#
head(dat7)
#
#
#
dat8 <- read_excel("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Sites added/Report C115 INTERIM 20250424_TS.xlsx", range= "A10:AK49")
#
head(dat8)
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
  mutate(across(8:36, ~ as.numeric(case_when(
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
dat7 <- dat7 %>%
  mutate(across(7:36, ~ as.numeric(case_when(
    str_detect(., "<") ~ "0",  # Replace any cell containing "<" with "0" per Mike's suggestions
    TRUE ~ as.character(.))))) 
#
#
dat8 <- dat8 %>%
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
dat8 <- dat8 %>% filter(!grepl("Not present", notes)) 
#
# Rename TOC to NPOC from Mike's comment "Any data titled "TOC" must be NPOC (total OC would imply unfiltered which would be crazy to do with these samples so must be a typo)"
colnames(dat3)[colnames(dat3) == "TOC_mg_l"] <- "NPOC_mg_l"
colnames(dat4)[colnames(dat4) == "TOC_mg_l"] <- "NPOC_mg_l"
#
# dat7 and dat8 use Si ug/l while all the rest use mg/L--convert 
dat7$Si_ug_l <- dat7$Si_ug_l/1000
dat8$Si_ug_l <- dat8$Si_ug_l/1000
#
colnames(dat7)[colnames(dat7) == "Si_ug_l"] <- "Si_mg_l"
colnames(dat8)[colnames(dat8) == "Si_ug_l"] <- "Si_mg_l"
#
#combine
#
dat <- bind_rows(dat1, dat2, dat3, dat4, dat5, dat6, dat7, dat8)
#
str(dat) #323 obs of 44 vars
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
# For plotting, remove the site=NA, for "Adney South?" until you confirm this site
dat <- dat %>%
  filter(!is.na(site))
#
# For plotting, having separate sites for RG-R6/RG-R8 and WF-A/WF-B adds to the business of the plots, and pre-rewetting these should effectively be the same, so group them
dat$site <- as.factor(dat$site)
#
levels(dat$site)[levels(dat$site) == "WF-A"] <- "WF"
levels(dat$site)[levels(dat$site) == "WF-B"] <- "WF"
levels(dat$site)[levels(dat$site) == "RG-R6"] <- "RG"
levels(dat$site)[levels(dat$site) == "RG-R8"] <- "RG"
levels(dat$site)[levels(dat$site) == "GC-A"] <- "GC"
levels(dat$site)[levels(dat$site) == "GC-B"] <- "GC"
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
#
#
site_summary <- dat %>%
  mutate(month = format(date, "%b"),
         month = factor(month, levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"))) %>%
  group_by(site) %>%
  filter(n_distinct(date) >= 3) %>%
  group_by(site, month) %>%
  summarise(n_samples = n(), .groups = "drop") %>%
  pivot_wider(names_from = month, values_from = n_samples, values_fill = 0) %>%
  select(site, Apr, May, Jun, Jul, Aug, Sep, Oct) %>%
  mutate(total_samples = rowSums(across(Apr:Oct)))
#
#
#data summary
#
#dat_sum <- dat %>%
#  group_by(site, month, land_use) %>%
#  summarise(Count = n(), .groups = "drop") 
#
#
#
# To account for repeated measures, the site_labels need to be align
# trim leading and trailing spaces
dat$site_label <- trimws(dat$site_label)
#
dat$site_label <- sapply(dat$site_label, function(x) {
  switch(x,
         "5 Eller Brook" = "Eller Brook",
         "Eller Brook June" = "Eller Brook",
         "Burwell Lode July" = "Burwell Fen Burwell Lode",
         "Burwell lode" = "Burwell Fen Burwell Lode",
         "Burwell Mere July" = "Burwell Fen Burwell Mere",
         "Burwell Mere" = "Burwell Fen Burwell Mere",
         "Burwell Ditch 1" = "Burwell Fen Ditch 1",
         "Burwell Ditch 2" = "Burwell Fen Ditch 2",
         "Blakemere June" = "Blakemere",
         "Alvanley June" = "Alvanley",
         "Alvaney" = "Alvanley",
         "Foresters west August" = "Foresters West",
         "Foresters north August" = "Foresters North",
         "Foresters internal August" = "Foresters Internal",
         "Foresters south August" = "Foresters South",
         "FF West" = "Foresters West",
         "FF North" = "Foresters North",
         "FF Internal" = "Foresters Internal",
         "Foresters North June" = "Foresters North",
         "Foresters West June" = "Foresters West",
         "Foresters Internal June" = "Foresters Internal",
         "Caldecote July" = "Great Fen Caldecote Drain",
         "Trundle Pond July" = "Great Fen Trundle Mere Pond",
         "Great Fen Trundle Mead Pond" = "Great Fen Trundle Mere Pond",
         "Long Mere July" = "Great Fen Long Mere",
         "Great Fen Longmere" = "Great Fen Long Mere",
         "Blackham Sluice July" = "Great Fen Blackham Sluice",
         "Great Fen Blackham sluice" = "Great Fen Blackham Sluice",
         "Hindmere July" = "Great Fen Hind Mere",
         "Great Fen Hindmere" = "Great Fen Hind Mere",
         "Wet Farm July" = "Great Fen Wet Farm",
         "New Decoy July" = "Great Fen New Decoy",
         "Great Fen Middle Farm D61A July" = "Great Fen Middle Farm D61a",
         "Great Fen Middle Farm D1021 July" = "Great Fen Middle Farm D102i",
         "Great Fen New Dyke July" = "Great Fen New Dyke",
         "Great Fen New dyke" = "Great Fen New Dyke",
         "Great Fen Mid Farm ?????" = "Great Fen Middle Farm",
         "Holme Fen Burnhams Mere" = "Holme Fen Burnham's Mere",
         "Holcroft Pool" = "Holcroft Moss Pool",
         "Holcroft Pore" = "Holcroft Moss Pore",
         "Holcroft June" = "Holcroft Moss",
         "Holiday Moss May" = "Holiday Moss",
         "Little Common July" = "Little Common Field ditch",
         "2. CL Ditch" = "Cheshire Lines ditch",
         "Cheshire Lines Ditch June" = "Cheshire Lines ditch",
         "Cheshire Lines Brook June" = "Cheshire Lines Brook",
         "Cheshire Lines" = "Cheshire Lines Brook",
         "6 ML (Meadow Lane Ditch)" = "Meadow Lane Ditch",
         "Meadow Lane Ditch June" = "Meadow Lane Ditch",
         "3 RBS (Rufford Boundary Sluice)" = "Rufford boundary sluice",
         "Rufford Boundary Sluice" = "Rufford boundary sluice",
         "Rufford Boundary Sluice June" = "Rufford boundary sluice",
         "Rufford Boundary Field Ditch June" = "Rufford boundary ditch",
         "RBS Field Ditch" = "Rufford boundary ditch",
         "RBS Smaller Field Ditch 4" = "Rufford boundary smaller field ditch",
         "LP3 Lanfley street wet field NE Ditch" = "LP3 Langley street wet field NE Ditch",
         "LP3 Lanfley street control  field N Ditch SW1" = "LP3 Langley street control  field N Ditch SW1",
         "LWM Lake August" = "LWM Lake",
         "LWM Lake June" =  "LWM Lake",
         "LWM Pore August" = "LWM Pore",
         "LWM West/north outflow August" = "LWM West outflow north",
         "LWM West/south outflow August" = "LWM West outflow south",
         "LWM West N June" = "LWM West outflow north",
         "LWM West S June" =  "LWM West outflow south",
         "LWM East outflow August" = "LWM East outflow",
         "LWM East June" =  "LWM East outflow",
         "Moss Side Central June" = "Moss Side Central",
         "Moss Side North June"  = "Moss Side North",
         "MM North E"  = "Railway View East", 
         "MM North NE" =  "Railway View NE", 
         "MM North NC" = "Railway View Central", 
         "MM North NW" = 	"Railway View NW",
         "MM North W" =   "Railway View West",
         "MM South N" = "Foresters North",
         "MM South W" =  "Foresters West", 
         "MM South Internal" = "Foresters Internal", 
        "Rosedean R6 D1" = "Rosedene R6 Ditch 1",
         "Rosedean R8 D1" = "Rosedene R8 Ditch 1",
         "Rosedean R8 D2" = "Rosedene R8 Ditch 2",
         "Rosedene N1 July" ="Rosedene R6 Ditch 1",
         "Rosedene N2 July" =  "Rosedene R6 Ditch 2",
         "Rosedene S1 July" = "Rosedene R8 Ditch 1",
         "Rosedene S2 July" ="Rosedene R8 Ditch 2",
         "Rosedean R6 D2" =  "Rosedene R6 Ditch 2",
         "Risley main 5 July" = "Risley Main",
         "Risley mini 8 July" = "Risley Mini",
         "Railway View Central June" = "Railway View Central",
         "Railway view East August" = "Railway View East",
         "Railway View East June" = "Railway View East",
         "Railway view N August" = "Railway View North",
         "Railway view NW August" = "Railway View NW",
         "Railway View NW June" = "Railway View NW",
         "Railway view pore August" = "Railway View pore",
         "RV East" = "Railway View East",
         "RV Inversion pore" = "Railway View pore",
         "RV NE" = "Railway View NE",
         "RV North" = "Railway View North",
         "RV NW" = "Railway View NW",
         "RV West" = "Railway View West",
         "Roughs Farm mid-level catchwater west" = "Roughs Farm Catchwater west",
         "Roughs Pump Field Ditch" = "Roughs Farm Field ditch",
         "Rough's Pump July" = "Roughs Farm Field ditch",
         "Roughs Farm  Ditchwater East" = "Roughs Farm Catchwater east",
         "Roughs Farm  ??? Ditch" ="Roughs Farm Field ditch",
         "Roughs Flux July" = "Roughs Farm Flux ditch",
         "Tubney Ditch July" = "Tubney Fen Ditch",
         "Tubney Fen ditch"  =  "Tubney Fen Ditch",
         "Tubney Mere" = "Tubney Fen Tubney Mere",
         "Tubney Mere July" = "Tubney Fen Tubney Mere",
         "Baker's Fen BF1 (East)" = "Baker's Fen East",
         "Baker's Fen BF4 (West)" = "Baker's Fen West",
         "Bakers Fen BF4 East" =  "Baker's Fen East",
         "Bakers Fen BF4 West" = "Baker's Fen West",
         "Wrights Farm 2" = "Wrights west D2",
         "Wrights Farm 5" = "Wrights east D1",
         "Wrights Farm 8" = "Wrights east D4",
         "Wrights East D1 June" = "Wrights east D1",
         "Wrights East D2 June" = "Wrights east D2",
         "Wrights East D3 June" = "Wrights east D3",
         "Wrights East D4 June" = "Wrights east D4",
         "Wrights West D1 June"  = "Wrights west D1",
         "Wrights West D2 June"  = "Wrights west D2",
         "Wrights West D3 June"  = "Wrights west D3",
         "Wrights West D4 June"  = "Wrights west D4",
         "Wrights East D1 May"  = "Wrights east D1",
         "Wrights East D2 May"  = "Wrights east D2",
         "Wrights East D3 May"  = "Wrights east D3",
         "Wrights East D4 May"  = "Wrights east D4",
         "Wrights West D1 May" = "Wrights west D1",
         "Wrights West D2 May" = "Wrights west D2",
         "Wrights West D3 May" = "Wrights west D3",
         "Wrights West D4 May" = "Wrights west D4",
         "Wright's west D2" = "Wrights west D2",
         "Wright's east D1"  ="Wrights east D1",
         "Wright's east D4" = "Wrights east D4",
         "Wright's west D4" = "Wrights west D4",
         "Adney North June"  = "Adney North",
         "HA2 June" = "HA2",
         "Monks Lode" = "Wicken Sedge Fen Monk's Lode",
         "Wicken Sedge Fen Monks Lode July" = "Wicken Sedge Fen Monk's Lode",
         "Sedge Fen Lode East" = "Wicken Sedge Fen Wicken Lode East",
         "West Fen Lode west"  ="Wicken Sedge Fen Wicken Lode West",
         "Wicken Sedge Fen Drainer's Dyke July" = "Wicken Sedge Fen SF2 (Drainers Dyke)",
         "West Fen Dyke drain end" = "Wicken Sedge Fen SF2 (Drainers Dyke)",
         "Wicken Sedge Fen SP4" = "Wicken Sedge Fen SF4",
         "Wicken Sedge Fen SF4 July" = "Wicken Sedge Fen SF4",
         "Wicken Sedge Fen WL East July" = "Wicken Sedge Fen Wicken Lode East",
         "Wicken Sedge Fen WL West July" = "Wicken Sedge Fen Wicken Lode West",
         "Woodwalton 1 July" = "Woodwalton Fen Ditch 1",
         "Woodwalton 2 July" ="Woodwalton Fen Ditch 2",
         "Woodwalton Fen ditch 1" = "Woodwalton Fen Ditch 1",
         "Woodwalton Fen ditch 2" = "Woodwalton Fen Ditch 2",
          x)  # Default case: returns original value if no match
})
# Ask Mike about Roughs Roughs Catchwater Upper July and Catchwater Lower July (which is east/west)
#
#
# Load in rough site level coordinates, and match them to dat by site 
coords <- fread("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Ancil dat/sample_site_coordinates_and_peat_type.csv", encoding = "Latin-1")
#
# Join coords onto dat by 'Site'
dat <- merge(dat, coords[, .(site, lat, lon, peat_type)], by = "site", all.x = TRUE)
#
#
# Remove pore water data for analysis, but keep in public dataset
dat <- dat %>%
  filter(!grepl("pore", site_label, ignore.case = TRUE))
#
# reorder columns 
#
dat <- dat %>% select(sample_code, site_label, site, date, month, land_use, peat_type, sampling_frequency, coordinates, lat, lon, notes, everything())
#

#
#
###################################################################################
# Read in Hg data
#
mercury <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/Mercury/Mercury Hg results_TS.csv")
#
# Make values below LoD as "0"
mercury <- mercury %>%
  mutate(Hg_ug_l = if_else(FINAL <= 0.010, 0, FINAL))
#
# Add coordinates so you can spatially plot Hg
mercury <- merge(mercury, coords[, .(site, lat, lon)], by = "site", all.x = TRUE)
#
# What percentage are <LOD? 
mercury %>%
  summarise(across(everything(), ~ sum(. == 0, na.rm = TRUE) / n() * 100)) -> zero_percent
#
####################################################################
#
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
# Number of sites
n_distinct(dat$site)
#
dat %>%
  group_by(sampling_frequency) %>%
  summarise(
    n_sites = n_distinct(site),
    n_observations = n()   )
#
# lake/pond/mere samples
non_ditch <- subset(dat, grepl("lake|pond", site_label, ignore.case = TRUE))
sum(grepl("lake|pond", dat$site_label, ignore.case = TRUE))


#### pH ####


#tiff("LP3+_water_quality_pH.tiff", units="in", width=6.5, height=4, res=300)

pH <- ggplot(dat, aes(x= fct_reorder (land_use, pH,  .fun = median, .na_rm = TRUE), y=pH, fill = land_use) ) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs( y = "pH",  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),, panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +
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
  geom_text_repel(data = dat_labels_pH, aes(label = site, colour = land_use), size = 3, box.padding = 0.35, 
                  max.overlaps = 40) 
pH_time2

#dev.off()


#### EC ####

#tiff("LP3+_water_quality_EC.tiff", units="in", width=6.5, height=4, res=300)

EC <-   ggplot(dat, aes(x= fct_reorder (land_use, EC_us_cm,  .fun = median, .na_rm = TRUE), y=EC_us_cm, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(  y = expression("Conductivity (" * mu * "S cm"^"-1" * ")"),  x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme(legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   ) +  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #
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
  geom_text_repel(data = dat_labels_EC, aes(label = site, colour = land_use), size = 3, box.padding = 0.35, 
                  max.overlaps = 10) 
EC_time2
#
#
#### Fluoride ####

#tiff("LP3+_water_quality_Fluoride.tiff", units="in", width=6.5, height=4, res=300)
Fluo <-   ggplot(dat, aes(x= fct_reorder (land_use, F_mg_l,  .fun = median, .na_rm = TRUE), y=F_mg_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("F- (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x= element_text(angle = 45, hjust = 1, size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")   )  + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #axis.text.x = element_text(angle = 45, hjust = 1, size=12), 
Fluo
#sig diffs: none
#dev.off()
#
#
#
#### Chloride ####

#tiff("LP3+_water_quality_Chloride.tiff", units="in", width=6.5, height=4, res=300)

Cl <-   ggplot(dat, aes(x= fct_reorder (land_use, Cl_mg_l,  .fun = median, .na_rm = TRUE), y=Cl_mg_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Cl- (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) +  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) +   
  annotate("text", x =1, y = 360 , label = "a", size=3) + 
  annotate("text", x =2, y = 360 , label = "ab", size=3) +
  annotate("text", x =3, y = 360 , label = "ab", size=3) +
  annotate("text", x =4, y = 360 , label = "b", size=3) +
  annotate("text", x =5, y = 360 , label = "ab", size=3) +
  annotate("text", x =6, y = 360 , label = "ab", size=3) +
  annotate("text", x =7, y = 360 , label = "b", size=3) 
Cl
# (Semi-natural bog) - Grassland ; semi natural bog - cropland

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
geom_text_repel(data = dat_labels_Cl, aes(label = site, colour = land_use), size = 3, box.padding = 0.35, 
 max.overlaps = 10) 
Cl_time2

#### Nitrite	#### 
#tiff("LP3+_water_quality_NO2.tiff", units="in", width=6.5, height=4, res=300)

NO2 <-   ggplot(dat, aes(x= fct_reorder(land_use, NO2_mg_l, .fun = median, .na_rm = TRUE), y = NO2_mg_l, fill = land_use)) + 
    geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression(NO[2]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  #scale_y_log10() + # the 0s cause an error with the log transformation
  scale_y_continuous(trans = 'pseudo_log') +
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
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
  geom_text_repel(data = dat_labels_NO2, aes(label = site, colour = land_use), size = 3,  box.padding = 0.35, 
                  max.overlaps = 10) 
NO2_time2

#### Nitrate ####

#("LP3+_water_quality_NO3.tiff", units="in", width=6.5, height=4, res=300)

NO3 <-   ggplot(dat, aes(x= fct_reorder(land_use, NO3_mg_l, .fun = median, .na_rm = TRUE), y = NO3_mg_l, fill = land_use)) + 
    geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression(NO[3]^"-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))   + annotate("text", x =1, y = 160 , label = "a", size=3) + 
  annotate("text", x =2, y = 160 , label = "a", size=3) +
  annotate("text", x =3, y = 160 , label = "a", size=3) +
  annotate("text", x =4, y = 160 , label = "a", size=3) +
  annotate("text", x =5, y = 160 , label = "a", size=3) +
  annotate("text", x =6, y = 160 , label = "b", size=3) +
  annotate("text", x =7, y = 160 , label = "b", size=3)
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
  geom_text_repel(data = dat_labels_NO3, aes(label = site, colour = land_use), size = 3,  box.padding = 0.35, max.overlaps = 15) 
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

PO4 <-   ggplot(dat, aes(x= fct_reorder(land_use, PO4_mg_l, .fun = median, .na_rm = TRUE), y = PO4_mg_l, fill = land_use)) + 
    geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression(PO[4]^"3-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
 # scale_y_log10() + 
  scale_y_continuous(trans = 'pseudo_log') +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + 
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
geom_text_repel(data = dat_labels_PO4, aes(label = site, colour = land_use), size = 3, box.padding = 0.35, max.overlaps = 30) 
PO4_time2

#### Sulfate	#### 

#tiff("LP3+_water_quality_SO4.tiff", units="in", width=6.5, height=4, res=300)

SO4 <-   ggplot(dat, aes(x= fct_reorder (land_use, SO4_mg_l,  .fun = median, .na_rm = TRUE), y=SO4_mg_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression(SO[4]^"2-" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12), , panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  +
  annotate("text", x =1, y = 3100 , label = "a", size=3) + 
  annotate("text", x =2, y = 3100 , label = "a", size=3) +
  annotate("text", x =3, y = 3100 , label = "a", size=3) +
  annotate("text", x =4, y = 3100 , label = "ac", size=3) +
  annotate("text", x =5, y = 3100 , label = "b", size=3) +
  annotate("text", x =6, y = 3100 , label = "bc", size=3) +
  annotate("text", x =7, y = 3100 , label = "abc", size=3) 
SO4
# Sig diffs: 
#Grassland - Semi-natural bog 
#Grassland - Rewetted bog
#Grassland - Rewetted extraction
#Cropland - Semi-natural bog
#Cropland - Rewetted bog 
#Cropland -Rewetted extraction 
#Cropland - (River/HLC)
#
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
  geom_text_repel(data = dat_labels_SO4, aes(label = site, colour = land_use), size = 3,  box.padding = 0.35, max.overlaps = 20) 
SO4_time2
#
dev.off()
#
#
#### DOC (measured as NPOC) ####      

DOC <-   ggplot(dat, aes(x= fct_reorder(land_use, NPOC_mg_l, .fun = median, .na_rm = TRUE), y = NPOC_mg_l, fill = land_use)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("DOC (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(10, 100, 600), labels = scales::number)+
  theme( axis.text.x = element_text(angle = 45, hjust = 1, size=12), legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) + annotate("text", x =1, y = 700 , label = "a", size=3) + 
  annotate("text", x =2, y = 700 , label = "ab", size=3) +
  annotate("text", x =3, y = 700 , label = "ab", size=3) +
  annotate("text", x =4, y = 700 , label = "ab", size=3) +
  annotate("text", x =5, y = 700 , label = "ab", size=3) +
  annotate("text", x =6, y = 700 , label = "b", size=3) +
  annotate("text", x =7, y = 700 , label = "b", size=3) 
DOC  # pairwise sig diffs: Grassland - (River/HLC) p = 0.0017        Rewetted bog - (River/HLC) p = 0.028

# NPOC_time only 2 dates, so wait until more data 

#### Total Carbon ####  # Don't include this plot as per Mike's comment

#TC <- ggplot(dat, aes(x = land_use, y =TC_mg_l,  fill = land_use)) + # Use fill for land use categories
 # geom_boxplot(outlier.shape = NA) + 
  #geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
#  labs(y = expression("TC (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
#  theme_minimal() + # Clean theme
#  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12), 
#TC


#### Total Nitrogen #####


TN <- ggplot(dat, aes(x = land_use, y =TN_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("TN (mg L"^-1*")"), x = NULL,   fill = "Land Use" ) +
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0,1, 2,5,10,20,40), labels = scales::number)+
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")  ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12),  
TN



#### Total organic carbon ##### This is a type and should actually be NPOC


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

Na <-   ggplot(dat, aes(x= fct_reorder (land_use, Na_mg_l,  .fun = median, .na_rm = TRUE), y=Na_mg_l, fill = land_use) ) +
    geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Na (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
Na
# no sig diffs 
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
  geom_text_repel(data = dat_labels_Na, aes(label = site, colour = land_use), size = 3,  box.padding = 0.35, max.overlaps = 20) 
Na_time2

#### Ammonium	####

#tiff("LP3+_water_quality_NH4.tiff", units="in", width=6.5, height=4, res=300)

NH4 <-   ggplot(dat, aes(x= fct_reorder(land_use, NH4_mg_l, .fun = median, .na_rm = TRUE), y = NH4_mg_l, fill = land_use)) +
    geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression(NH[4]^"+" ~ "(mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  #scale_y_log10() + 
  #scale_y_continuous(trans = 'pseudo_log') +
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 20, 30, 40), labels = scales::number)+
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
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
  geom_text_repel(data = dat_labels_NH4, aes(label = site, colour = land_use), size = 3, box.padding = 0.35, max.overlaps = 20) 
NH4_time2

#### Magnesium	#### 

#tiff("LP3+_water_quality_Mg.tiff", units="in", width=6.5, height=4, res=300)

Mg <-   ggplot(dat, aes(x= fct_reorder (land_use, Mg_mg_l,  .fun = median, .na_rm = TRUE), y=Mg_mg_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Mg (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) +
  annotate("text", x =1, y = 105 , label = "a", size=3) + 
  annotate("text", x =2, y = 105 , label = "a", size=3) +
  annotate("text", x =3, y = 105 , label = "ab", size=3) +
  annotate("text", x =4, y = 105 , label = "b", size=3) +
  annotate("text", x =5, y = 105 , label = "b", size=3) +
  annotate("text", x =6, y = 105 , label = "b", size=3) +
  annotate("text", x =7, y = 105 , label = "b", size=3) 
Mg
# sig diffs: 
#Semi-natural bog - Semi-natural fen; 
#Semi-natural bog -Grassland; 
#Semi-natural bog - Cropland; 
#Semi-natural bog - River/HLC; 
#Rewetted bog- Semi-natural fen ; 
#Rewetted bog - grassland; 
#Rewetted bog - Cropland; 
#Rewetted bog - (River/HLC); 

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
geom_text_repel(data = dat_labels_Mg, aes(label = site, colour = land_use), size = 3, box.padding = 0.35, max.overlaps = 40) 
Mg_time2


#### Potassium	#### 

#tiff("LP3+_water_quality_K.tiff", units="in", width=6.5, height=4, res=300)

K <-   ggplot(dat, aes(x= fct_reorder (land_use, K_mg_l,  .fun = median, .na_rm = TRUE), y=K_mg_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("K (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 100), labels = scales::number)+
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))   +
  annotate("text", x =1, y = 150 , label = "a", size=3) + 
  annotate("text", x =2, y = 150 , label = "ab", size=3) +
  annotate("text", x =3, y = 150 , label = "ab", size=3) +
  annotate("text", x =4, y = 150 , label = "ab", size=3) +
  annotate("text", x =5, y = 150 , label = "b", size=3) +
  annotate("text", x =6, y = 150 , label = "ab", size=3) +
  annotate("text", x =7, y = 150 , label = "b", size=3) 
K
# sig diffs: semi-natural bog - grassland; semi-natural bog - cropland; 
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
  geom_text_repel(data = dat_labels_K, aes(label = site, colour = land_use), size = 3, box.padding = 0.35, max.overlaps = 20) 
K_time2
#
#### Calcium	#### 

#tiff("LP3+_water_quality_Ca.tiff", units="in", width=6.5, height=4, res=300)

Ca <-   ggplot(dat, aes(x= fct_reorder (land_use, Ca_mg_l,  .fun = median, .na_rm = TRUE), y=Ca_mg_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Ca (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) +
  annotate("text", x =1, y = 700 , label = "a", size=3) + 
  annotate("text", x =2, y = 700 , label = "a", size=3) +
  annotate("text", x =3, y = 700 , label = "ac", size=3) +
  annotate("text", x =4, y = 700 , label = "b", size=3) +
  annotate("text", x =5, y = 700 , label = "bc", size=3) +
  annotate("text", x =6, y = 700 , label = "bc", size=3) +
  annotate("text", x =7, y = 700 , label = "b", size=3) 
Ca
# semi-natural bog - fen 0.01
# semi-natural bog - grassland 0.002
# semi-natural bog - cropland 0.001
# semi-natural bog - river 0.01
# rewetted bog - fen  0.04
# rewetted bog - grassland - 0.01
# rewetted bog - cropland 0.01
# rewetted bog - river
# rewetted extraction - grassland -  0.01
# rewetted extraction - cropland 0.001
# cropland - river 0.04

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
  geom_text_repel(data = dat_labels_Ca, aes(label = site, colour=land_use), size = 3, box.padding = 0.35, max.overlaps = 20) 
Ca_time2
#
#### Al	#####

#tiff("LP3+_water_quality_Al.tiff", units="in", width=6.5, height=4, res=300)

Al <-   ggplot(dat, aes(x= fct_reorder (land_use, Al_ug_l,  .fun = median, .na_rm = TRUE), y=Al_ug_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Al (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 100, 1000), labels = scales::number)+
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) +
  annotate("text", x =1, y = 3400 , label = "a", size=3) + 
  annotate("text", x =2, y = 3400 , label = "ab", size=3) +
  annotate("text", x =3, y = 3400 , label = "a", size=3) +
  annotate("text", x =4, y = 3400 , label = "ab", size=3) +
  annotate("text", x =5, y = 3400 , label = "ab", size=3) +
  annotate("text", x =6, y = 3400 , label = "ab", size=3) +
  annotate("text", x =7, y = 3400 , label = "b", size=3) 
Al  # sig difs: semi-natural fen - rewetted bog; rewetted bog - River/HLC
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
  geom_text_repel(data = dat_labels_Al, aes(label = site, colour=land_use), size = 3, box.padding = 0.35, max.overlaps = 40) 
Al_time2
#
#### As ####

#tiff("LP3+_water_quality_As.tiff", units="in", width=6.5, height=4, res=300)

As <-   ggplot(dat, aes(x= fct_reorder (land_use, As_ug_l,  .fun = median, .na_rm = TRUE), y=As_ug_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("As (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme  
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10, 50, 100), labels = scales::number)+
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
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

Cr <-   ggplot(dat, aes(x= fct_reorder (land_use, Cr_ug_l,  .fun = median, .na_rm = TRUE), y=Cr_ug_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Cr (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
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
  geom_text_repel(data = dat_labels_Cr, aes(label = site, colour=land_use), size = 3, box.padding = 0.35, max.overlaps = 20) 
Cr_time2
#
#### Cu	####

#tiff("LP3+_water_quality_Cu.tiff", units="in", width=6.5, height=4, res=300)

Cu <-   ggplot(dat, aes(x= fct_reorder (land_use, Cu_ug_l,  .fun = median, .na_rm = TRUE), y=Cu_ug_l, fill = land_use) ) +
    geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Cu (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",   axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
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
  geom_text_repel(data = dat_labels_Cu, aes(label = site, colour=land_use), size = 3, box.padding = 0.35, max.overlaps = 30) 
Cu_time2


#### Fe	#### 

#tiff("LP3+_water_quality_Fe.tiff", units="in", width=6.5, height=4, res=300)

Fe <-   ggplot(dat, aes(x= fct_reorder (land_use, Fe_ug_l,  .fun = median, .na_rm = TRUE), y=Fe_ug_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
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
  geom_text_repel(data = dat_labels_Fe, aes(label = site, colour=land_use), size = 3, box.padding = 0.35, max.overlaps = 20) 
Fe_time2

#### Inorganic Carbon ####

#tiff("LP3+_water_quality_IC.tiff", units="in", width=6.5, height=4, res=300)
 
DIC <- ggplot(dat, aes(x = land_use, y =IC_mg_l,  fill = land_use)) + # Use fill for land use categories
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("IC (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  #scale_y_log10() + 
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 3, 10, 30), labels = scales::number) +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12)
DIC

#dev.off()


#### Mn	####

#tiff("LP3+_water_quality_Mn.tiff", units="in", width=6.5, height=4, res=300)

Mn <-   ggplot(dat, aes(x= fct_reorder (land_use, Mn_ug_l,  .fun = median, .na_rm = TRUE), y=Mn_ug_l, fill = land_use) ) +
    geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Mn (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 10, 100, 1000, 10000), labels = scales::number)+
  #scale_y_continuous(trans = 'pseudo_log') +
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #
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
geom_text_repel(data = dat_labels_Mn, aes(label = site, colour=land_use), size = 3, box.padding = 0.35, max.overlaps = 20) 
Mn_time2
#
#### Ni	#### 

#tiff("LP3+_water_quality_Ni.tiff", units="in", width=6.5, height=4, res=300)

Ni <-   ggplot(dat, aes(x= fct_reorder (land_use, Ni_ug_l,  .fun = median, .na_rm = TRUE), y=Ni_ug_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Ni (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 5, 10, 20, 50), labels = scales::number)+
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #
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
  geom_text_repel(data = dat_labels_Ni, aes(label = site, colour=land_use), size = 3, box.padding = 0.35, max.overlaps = 20) 
Ni_time2
#
#### Pb ####	

#tiff("LP3+_water_quality_Pb.tiff", units="in", width=6.5, height=4, res=300)

Pb <-   ggplot(dat, aes(x= fct_reorder (land_use, Pb_ug_l,  .fun = median, .na_rm = TRUE), y=Pb_ug_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Pb (µg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none", axis.title = element_text(size = 14), axis.text.y = element_text(size=12),  axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  #element_text(angle = 45, hjust = 1, size=12),
Pb

#dev.off()


#### Zn	#### 
#tiff("LP3+_water_quality_Zn.tiff", units="in", width=6.5, height=4, res=300)

Zn <-   ggplot(dat, aes(x= fct_reorder (land_use, Zn_ug_l,  .fun = median, .na_rm = TRUE), y=Zn_ug_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
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

TP <-   ggplot(dat, aes(x= fct_reorder(land_use, P_mg_l, .fun = median, .na_rm = TRUE), y = P_mg_l, fill = land_use)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("P (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  #scale_y_log10(labels = label_number()) +
  scale_y_continuous(trans = 'pseudo_log',   breaks = c(0, 1, 10), labels = scales::number)+
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.y = element_text(size=12), axis.text.x = element_text(angle = 45, hjust = 1, size=12),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black")    ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" ))  
TP

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
  geom_text_repel(data = dat_labels_P, aes(label = site, colour=land_use), size = 3, box.padding = 0.35, max.overlaps = 20) 
P_time2
#
#### Si ####
#tiff("LP3+_water_quality_Si.tiff", units="in", width=6.5, height=4, res=300)

# for units can use ug or mg

Si <-   ggplot(dat, aes(x= fct_reorder (land_use, Si_mg_l,  .fun = median, .na_rm = TRUE), y=Si_mg_l, fill = land_use) ) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(aes(fill = land_use), colour="black", alpha=0.5, size = 3, width = 0.2, shape=21) + 
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, stroke =1.2, color = "black") +
  labs(y = expression("Si (mg L"^-1*")"), x = NULL, fill = "Land Use") + 
  theme_minimal() + # Clean theme
  theme( legend.position = "none",  axis.title = element_text(size = 14), axis.text.x = element_text(angle = 45, hjust = 1, size=12),  axis.text.y = element_text(size=12),   panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = "black"), axis.ticks = element_line(color = "black") ) + scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", "Semi-natural bog" = "#6DA34D" )) 
Si   # no sig diffs
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
geom_text_repel(data = dat_labels_Si, aes(label = site, colour=land_use), size = 3, box.padding = 0.35, max.overlaps = 30) 
Si_time2
#
#### combine plots ####
# use cowplot bc ggarrange spacing is weird (too much white space)


jpeg("LP3+_wq_key_elements.jpeg", units="in", width=8, height=10, res=300)

combine1 <- ggarrange(DOC ,
                      NO3 , 
                      NO2 , 
                      NH4 , 
                      PO4 , 
                      TP ,
                      nrow = 3, ncol = 2, align = "v", labels = c("A", "B", "C", "D", "E", "F"),
                      heights = c( 1,1,1))  
combine1

dev.off()


# ions
jpeg("LP3+_wq_ions.jpeg", units="in", width=9, height=14, res=300)

combine1 <- plot_grid(Al, Ca, Cl, Fluo, K, Mg, Na, SO4, Si, 
                       ncol = 2, align = "h" ) 
combine1

dev.off()



jpeg("LP3+_water_quality1.jpeg", units="in", width=10, height=12, res=300)

combine1 <- plot_grid(Al, As, 
                      Ca, Cl, Cr, Cu, 
                      DOC, EC, Fluo, Fe, 
                      ncol = 2, align = "v", rel_heights = c( 1,1,1, 1, 1.6) )   # Excluded Cd bc very few data points
combine1

dev.off()

#
#
#
#
jpeg("LP3+_water_quality2.jpeg", units="in", width=10, height=12, res=300)

combine2 <- plot_grid( IC, K, Mg, Mn, Na, NH4,  
                      Ni, NO2, NO3, P,
                      ncol = 2, align = "v", rel_heights = c( 1,1,1,1, 1.6)) 
combine2

dev.off()
#
#
#
#
jpeg("LP3+_water_quality3.jpeg", units="in", width=10, height=12, res=300)

combine3 <- plot_grid(pH, Pb,
                      PO4, Si, SO4, TN, Zn,  
                      ncol = 2, align = "v", rel_heights = c( 1,1,1, 1.6)) 
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
                          P_time, pH_time, PO4_time, Si_time, SO4_time,
                          ncol = 2, nrow=5,  align = "v", common.legend=T )
time_series_b
dev.off()  # the semi natural bog colour does not appear, might have to add manually in Illustrator
#
#
#
# Time series with site labels
#
jpeg("LP3+_water_quality_timeseries_site1.jpeg", units="in", width=10, height=14, res=250)
time_series_1 <- ggarrange(Al_time2, Ca_time2, Cr_time2, 
                           ncol = 1, nrow=3,  align = "v", common.legend=T )
time_series_1
dev.off() 


jpeg("LP3+_water_quality_timeseries_site2.jpeg", units="in", width=10, height=11, res=200)
time_series_2 <- ggarrange(Cu_time2, EC_time2, Fe_time2,
                           ncol = 1, nrow=3,  align = "v", common.legend=T )
time_series_2
dev.off() 


jpeg("LP3+_water_quality_timeseries_site3.jpeg", units="in", width=12, height=14, res=200)
time_series_3 <- ggarrange(  K_time2, Mg_time2, Mn_time2, 
                          ncol = 1, nrow=3,  align = "v", common.legend=T )
time_series_3
dev.off()

jpeg("LP3+_water_quality_timeseries_site4.jpeg", units="in", width=12, height=14, res=200)
time_series_4 <- ggarrange(  Na_time2,  NH4_time2, Ni_time2, 
                             ncol = 1, nrow=3,  align = "v", common.legend=T )
time_series_4
dev.off()


jpeg("LP3+_water_quality_timeseries_site5.jpeg", units="in", width=12, height=14, res=200)
time_series_5 <- ggarrange( NO2_time2, NO3_time2, P_time2, 
                             ncol = 1, nrow=3,  align = "v", common.legend=T )
time_series_5
dev.off()


jpeg("LP3+_water_quality_timeseries_site6.jpeg", units="in", width=12, height=14, res=200)
time_series_6 <- ggarrange( pH_time2, PO4_time2, Si_time2, 
                            ncol = , nrow=3,  align = "v", common.legend=T )
time_series_6
dev.off()


jpeg("LP3+_water_quality_timeseries_site7.jpeg", units="in", width=12, height=5, res=200)
time_series_7 <- ggarrange( SO4_time2,
                            ncol = 1, nrow= 1,  align = "v", common.legend=T )
time_series_7
dev.off()
unique(dat$site)
#######################################################################################
#### Ternary plots ####
#From Peacock et al 2022 Ecosystems
#Total inorganic nitrogen (TIN) was calculated as the sum of NH4 and NO2 +NO3
dat$NH4_N_mg_l <- dat$NH4_mg_l * (14.01 / 18.04)
dat$NO2_N_mg_l  <- dat$NO2_mg_l  * (14.01 / 46.01)
dat$NO3_N_mg_l  <-  dat$NO3_mg_l  * (14.01 / 62.01)
#
dat$TIN_mg_l <-  dat$NH4_N_mg_l +  dat$NO2_N_mg_l + dat$NO3_N_mg_l
#
# Not sure if I should use P or PO4? we have a bit more PO4 data, can we extrapolate P based on the linear relationship?
plot(dat$P_mg_l, dat$PO4_mg_l) #linear relationship
lm_model <- lm(P_mg_l ~ PO4_mg_l, data = dat)
summary(lm_model)
# Predict P values from PO₄
#dat$predicted_P_mg_l <- predict(lm_model, newdata = dat)
#
#
dat$PO4_P_mg_l <- dat$PO4_mg_l * (30.97 / 94.97)
#
#From Smith and others (2017): after converting to moles, TN concentrations and TP concentrations are multiplied by 6.625 and 106, respectively. This conversion places the Redfield Ratio (C:N:P 106:16:1) in the center of the ternary plot as a reference point.
#
#
# As a side bar here, calculate CN, CP, NP to see relationships with GHGs later
dat$CN_ratio <- dat$NPOC_mg_l / dat$TIN_mg_l
dat$CP_ratio <- dat$NPOC_mg_l / dat$PO4_P_mg_l
dat$NP_ratio <- dat$TIN_mg_l / dat$PO4_P_mg_l
#
dat$CN_ratio[is.infinite(dat$CN_ratio)] <- NA # Make Inf into NAs
dat$CP_ratio[is.infinite(dat$CP_ratio)] <- NA
dat$NP_ratio[is.infinite(dat$NP_ratio)] <- NA
dat$NP_ratio[is.nan(dat$NP_ratio)] <- NA      # mae NaN into NAs
#
#
# Convert to mol/L or µmol/L (micromoles per liter)
# Molecular weights:
# Organic C -> 12.01 g/mol
# TIN is in mg N/L -> 14.01 g/mol
# Inorganic P -> 30.97
#
dat$C_umol_L <- dat$NPOC_mg_l/ 12.01 / 1000  * 1.0E6   # ( molar weight / mg to g  / mol to umol)
dat$N_umol_L <- dat$TIN_mg_l / 14.01 / 1000  * 1.0E6
dat$P_umol_L <- dat$PO4_P_mg_l / 30.97 / 1000  * 1.0E6
#
#
dat$N_redfield <- dat$N_umol_L * 6.625  
dat$P_redfield <- dat$P_umol_L * 106  
#
#
# Scale to percentage of total molar content
dat$sum_CNP <- dat$C_umol_L + dat$N_redfield + dat$P_redfield
#
dat$C_tern <- dat$C_umol_L / dat$sum_CNP * 100
dat$N_tern <- dat$N_redfield / dat$sum_CNP * 100
dat$P_tern <- dat$P_redfield / dat$sum_CNP * 100
#
#
# Ternary plot
tern_all <- ggtern(data = dat, aes(x = P_tern  , y = C_tern  ,  z =  N_tern  )) +
  theme_bw() +                          
  theme_showarrows() +
  geom_segment(data = data.frame(x = 20, y = 80, z = 0, xend = 20, yend = 0, zend = 80),
    mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "blue", alpha=0.7 ,  linewidth = 1) +
  geom_segment(data = data.frame(x = 80, y = 0, z = 20, xend = 0, yend = 80, zend = 20), mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "yellow", alpha=0.7, linewidth = 1) +
  geom_segment(    data = data.frame(x = 80, y = 20, z = 0, xend = 0, yend = 20, zend = 80),
    mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),
    inherit.aes = FALSE,  color = "red", alpha=0.7, linewidth = 1) +
  geom_point(aes(fill=land_use), shape=21, colour="black", size =3, alpha=0.7) +    
  geom_point(aes(x = 106, y = 106, z = 106), color = "red", size = 3) +  # Redfield Ratio
    theme( tern.axis.title.T = element_blank(), tern.axis.title.L = element_blank(), tern.axis.title.R = element_blank() ) + theme_clockwise()  +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" )) +
  theme( tern.axis.text = element_text(size = 10), legend.title = element_blank(), tern.panel.mask.show = FALSE, tern.axis.arrow.T = element_line(size = 2), tern.axis.arrow.L = element_line(size = 2), tern.axis.arrow.R = element_line(size = 2)) +  labs(T = "%C", L = "%P", R = "%N")
tern_all
#
#
# Cropland ternary plot
tern_cropland <- ggtern(data = subset(dat, land_use=="Cropland"), aes(x = P_tern  , y = C_tern  ,  z =  N_tern  )) +   theme_bw() +theme_showarrows() +
  geom_segment(data = data.frame(x = 20, y = 80, z = 0, xend = 20, yend = 0, zend = 80),
               mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "blue", alpha=0.7,  linewidth = 1) +
  geom_segment(data = data.frame(x = 80, y = 0, z = 20, xend = 0, yend = 80, zend = 20), mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "yellow",alpha=0.7,  linewidth = 1) +
  geom_segment(    data = data.frame(x = 80, y = 20, z = 0, xend = 0, yend = 20, zend = 80),
                   mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),
                   inherit.aes = FALSE,  color = "red", alpha=0.7,  linewidth = 1) +
  geom_point(aes(fill=land_use), shape=21, colour="black", size =3, alpha=0.7) +    
  geom_point(aes(x = 106, y = 106, z = 106), color = "red", size = 3) +  # Redfield Ratio
  theme( tern.axis.title.T = element_blank(), tern.axis.title.L = element_blank(), tern.axis.title.R = element_blank() ) + theme_clockwise()  +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                               "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                               "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                               "Semi-natural bog" = "#6DA34D" ), guide = "none") +
  theme(tern.axis.text = element_text(size = 10), tern.axis.arrow.T = element_line(size = 2), tern.axis.arrow.L = element_line(size = 2), tern.axis.arrow.R = element_line(size = 2), legend.title = element_blank(), tern.panel.mask.show = FALSE) +  labs(T = "%C", L = "%P", R = "%N") + coord_tern(expand = TRUE) 
tern_cropland
#
#
# Testing with labels for site since there is so much dispersion
tern_cropland_sites <- ggtern(data = subset(dat, land_use=="Cropland"), aes(x = P_tern  , y = C_tern  ,  z =  N_tern  )) +   theme_bw() +theme_showarrows() +
  geom_segment(data = data.frame(x = 20, y = 80, z = 0, xend = 20, yend = 0, zend = 80),
               mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "blue", alpha=0.7,  linewidth = 1) +
  geom_segment(data = data.frame(x = 80, y = 0, z = 20, xend = 0, yend = 80, zend = 20), mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "yellow", alpha=0.7,  linewidth = 1) +
  geom_segment(    data = data.frame(x = 80, y = 20, z = 0, xend = 0, yend = 20, zend = 80),
                   mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),
                   inherit.aes = FALSE,  color = "red", alpha=0.7,  linewidth = 1) +
  geom_point(aes(fill=land_use, shape=site), size =3, alpha=0.7, colour="black") +    
  geom_point(aes(x = 106, y = 106, z = 106), color = "red", size = 3) +  # Redfield Ratio
  theme( tern.axis.title.T = element_blank(), tern.axis.title.L = element_blank(), tern.axis.title.R = element_blank() ) + theme_clockwise()  +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" ), guide = "none") +
  theme(tern.axis.text = element_text(size = 10), tern.axis.arrow.T = element_line(size = 2), tern.axis.arrow.L = element_line(size = 2), tern.axis.arrow.R = element_line(size = 2), legend.title = element_blank(), tern.panel.mask.show = FALSE) +  labs(T = "%C", L = "%P", R = "%N") + coord_tern(expand = TRUE) + 
  scale_shape_manual(values = c("LC" = 13, "LM" = 7, "RG" = 22, "RIN"=3, "SW"=24, "WF"=8, "WM"=21)) 
tern_cropland_sites
#
#
# Grassland ternary plot
tern_grassland <- ggtern(data = subset(dat, land_use=="Grassland"), aes(x = P_tern  , y = C_tern  ,  z =  N_tern  )) +   theme_bw() +theme_showarrows() +
  geom_segment(data = data.frame(x = 20, y = 80, z = 0, xend = 20, yend = 0, zend = 80),
               mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "blue", alpha=0.7,  linewidth = 1) +
  geom_segment(data = data.frame(x = 80, y = 0, z = 20, xend = 0, yend = 80, zend = 20), mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "yellow", alpha=0.7,  linewidth = 1) +
  geom_segment(    data = data.frame(x = 80, y = 20, z = 0, xend = 0, yend = 20, zend = 80),
                   mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),
                   inherit.aes = FALSE,  color = "red",  alpha=0.7, linewidth = 1) +
  geom_point(aes(fill=land_use), size =3, alpha=0.7, shape=21, colour="black") +    
  geom_point(aes(x = 106, y = 106, z = 106), color = "red", size = 3) +  # Redfield Ratio
  theme(tern.axis.arrow.T = element_line(size = 2), tern.axis.arrow.L = element_line(size = 2), tern.axis.arrow.R = element_line(size = 2), tern.axis.title.T = element_blank(), tern.axis.title.L = element_blank(), tern.axis.title.R = element_blank() ) + theme_clockwise()  +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D", guide = "none" )) +
  theme(tern.axis.text = element_text(size = 10), tern.axis.arrow.T = element_line(size = 2), tern.axis.arrow.L = element_line(size = 2), tern.axis.arrow.R = element_line(size = 2),legend.position = "none",  tern.panel.mask.show = FALSE) +  labs(T = "%C", L = "%P", R = "%N") +
  #geom_text(    data = subset(dat, land_use == "Grassland" & C_tern < 60),    aes(x = P_tern, y = C_tern, z = N_tern, label = site),    inherit.aes = FALSE,    size = 3) +
  annotate("text", x = 43, y = 29, z = 11, label = "GF", size = 3, color = "black") +
  annotate("text", x = 79, y = 33, z = 17, label = "GF", size = 3, color = "black") +
  annotate("text", x = 80, y = 21, z = 25, label = "RV", size = 3, color = "black") +
  annotate("text", x = 7, y = 8, z = 95, label = "GF", size = 3, color = "black") +
  annotate("text", x = 9, y = 15, z = 95, label = "BUF", size = 3, color = "black") +
  annotate("text", x = 13, y = 91, z = 72, label = "GF", size = 3, color = "black") 
tern_grassland
#
#
# Combine semi naural bog and fen, because very few bog points
tern_natural <- ggtern(data = subset(dat, land_use=="Semi-natural bog"| land_use=="Semi-natural fen"), aes(x = P_tern  , y = C_tern  ,  z =  N_tern  )) +
  theme_bw() +                          
  theme_showarrows() +
  geom_segment(data = data.frame(x = 20, y = 80, z = 0, xend = 20, yend = 0, zend = 80),
               mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "blue", alpha=0.7,  linewidth = 1) +
  geom_segment(data = data.frame(x = 80, y = 0, z = 20, xend = 0, yend = 80, zend = 20), mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "yellow", alpha=0.7,  linewidth = 1) +
  geom_segment(    data = data.frame(x = 80, y = 20, z = 0, xend = 0, yend = 20, zend = 80),
                   mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),
                   inherit.aes = FALSE,  color = "red", alpha=0.7,  linewidth = 1) +
  geom_point(aes(fill=land_use), shape=21, colour="black", size =3, alpha=0.7) +    
  geom_point(aes(x = 106, y = 106, z = 106), color = "red", size = 3) +  # Redfield Ratio
  theme(tern.axis.text = element_text(size = 10), tern.axis.arrow.T = element_line(size = 2), tern.axis.arrow.L = element_line(size = 2), tern.axis.arrow.R = element_line(size = 2), tern.axis.title.T = element_blank(), tern.axis.title.L = element_blank(), tern.axis.title.R = element_blank() ) + theme_clockwise()  +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" )) +
  theme(legend.title = element_blank(), tern.panel.mask.show = FALSE) +  labs(T = "%C", L = "%P", R = "%N") +
  #geom_text(    data = subset(dat,  land_use=="Semi-natural bog"| land_use=="Semi-natural fen"),    aes(x = P_tern, y = C_tern, z = N_tern, label = site),    inherit.aes = FALSE,    size = 3) +
  annotate("text", x = 15, y = 21, z = 80, label = "WSF", size = 3, color = "black") +
  annotate("text", x = 8, y = 10, z = 95, label = "WSF", size = 3, color = "black") 
tern_natural
#
#
# Rewetted
tern_rewetted <- ggtern(data = subset(dat, land_use=="Rewetted extraction"| land_use=="Rewetted bog"), aes(x = P_tern  , y = C_tern  ,  z =  N_tern  )) +
  theme_bw() +                          
  theme_showarrows() +
  geom_segment(data = data.frame(x = 20, y = 80, z = 0, xend = 20, yend = 0, zend = 80),
               mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "blue", alpha=0.7,  linewidth = 1) +
  geom_segment(data = data.frame(x = 80, y = 0, z = 20, xend = 0, yend = 80, zend = 20), mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "yellow", alpha=0.7,  linewidth = 1) +
  geom_segment(    data = data.frame(x = 80, y = 20, z = 0, xend = 0, yend = 20, zend = 80),
                   mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),
                   inherit.aes = FALSE,  color = "red", alpha=0.7,  linewidth = 1) +
  geom_point(aes(fill=land_use), shape=21, colour="black", size =3, alpha=0.7) +    
  geom_point(aes(x = 106, y = 106, z = 106), color = "red", size = 3) +  # Redfield Ratio
  theme(tern.axis.text = element_text(size = 10), tern.axis.arrow.T = element_line(size = 2), tern.axis.arrow.L = element_line(size = 2), tern.axis.arrow.R = element_line(size = 2),  tern.axis.title.T = element_blank(), tern.axis.title.L = element_blank(), tern.axis.title.R = element_blank() ) + theme_clockwise()  +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" )) +
  theme(legend.title = element_blank(), tern.panel.mask.show = FALSE) +  labs(T = "%C", L = "%P", R = "%N") 
tern_rewetted
#
#
#
# River/HLC
# Ternary plot
tern_RiverHLC <- ggtern(data = subset(dat,land_use=="River/HLC"), aes(x = P_tern  , y = C_tern  ,  z =  N_tern  )) +  theme_bw() +                          
  theme_showarrows() +
  geom_segment(data = data.frame(x = 20, y = 80, z = 0, xend = 20, yend = 0, zend = 80),
               mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "blue", alpha=0.7,  linewidth = 1) +
  geom_segment(data = data.frame(x = 80, y = 0, z = 20, xend = 0, yend = 80, zend = 20), mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend), inherit.aes = FALSE, color = "yellow", alpha=0.7,  linewidth = 1) +   geom_segment(    data = data.frame(x = 80, y = 20, z = 0, xend = 0, yend = 20, zend = 80),                    mapping = aes(x = x, y = y, z = z, xend = xend, yend = yend, zend = zend),  inherit.aes = FALSE,  color = "red", alpha=0.7,  linewidth = 1) +
  geom_point(aes(fill=land_use), shape=21, colour="black", size =3, alpha=0.7) +    
  geom_point(aes(x = 106, y = 106, z = 106), color = "red", size = 3) +  # Redfield Ratio
  theme(tern.axis.arrow.T = element_line(size = 2), tern.axis.arrow.L = element_line(size = 2), tern.axis.arrow.R = element_line(size = 2), tern.axis.title.T = element_blank(), tern.axis.title.L = element_blank(), tern.axis.title.R = element_blank() ) + theme_clockwise()  +
  scale_fill_manual(values = c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                                 "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                                 "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                                 "Semi-natural bog" = "#6DA34D" )) +
  theme(tern.axis.text = element_text(size = 10), legend.position = "none", tern.panel.mask.show = FALSE) +  labs(T = "%C", L = "%P", R = "%N") +
 #geom_text(    data = subset(dat,  land_use=="River/HLC" & N_tern<40),    aes(x = P_tern, y = C_tern, z = N_tern, label = site),    inherit.aes = FALSE,    size = 3) +
  annotate("text", x = 28, y = 72.5, z = 10, label = "GF", size = 3, color = "black") +
  annotate("text", x = 15, y = 87, z = 6, label = "WSF", size = 3, color = "black") +
  annotate("text", x = 6, y = 95, z = 3.5, label = "GF", size = 3, color = "black") +
  annotate("text", x = -8, y = 100, z = 7.5, label = "WSF", size = 3, color = "black") 
tern_RiverHLC
#
#
#### Combine ternary plots ####
# Grid plot not really working with the spacing so try ggarrange

jpeg("LP3+_tern_combined.jpeg", units="in", width=9, height=7, res=350)
combine2 <- ggarrange(tern_all  + theme(axis.title = element_blank(), axis.text = element_blank(), plot.margin = margin(t = -8, b = -8, l = -0.8, r = -0.8, unit = "cm") ), 
  tern_cropland  + theme(axis.title = element_blank(), axis.text = element_blank(), plot.margin = margin(t = -8, b = -8, l = -0.8, r = -0.8, unit = "cm") ),                                 
                      tern_grassland + theme(axis.title = element_blank(), axis.text = element_blank(), plot.margin = margin(t = -8, b = -8, l = -0.8, r = -0.8, unit = "cm")),  
                      tern_natural + theme(legend.position="none", axis.title = element_blank(), axis.text = element_blank(), plot.margin = margin(t = -8, b = -8, l = -0.8, r = -0.8, unit = "cm")), 
                      tern_rewetted + theme(legend.position="none", axis.title = element_blank(), axis.text = element_blank(), plot.margin = margin(t = -8, b = -8, l =-0.8, r = -0.8, unit = "cm")), 
                      tern_RiverHLC + theme(axis.title = element_blank(), axis.text = element_blank(),  plot.margin = margin(t = -8, b = -8, l = -0.8, r = -0.8, unit = "cm")), 
  ncol = 3, nrow = 2,  common.legend = TRUE, legend = "bottom", heights=1, widths=1,   labels = c("A", "B", "C", "D", "E", "F"))
combine2
dev.off()

#######################################################################################
#### Statistical analysis ####
#
sum(dat$NO3_mg_l == 0, na.rm = TRUE) / sum(!is.na(dat$NO3_mg_l)) * 100

## Summary stats ##
summary <- dat %>%
  group_by(site) %>%
  summarise(lat, lon)
#
# Means by land use 
summary_table <- dat %>%
  group_by(land_use) %>%
  summarise(across(where(is.numeric), 
                   list(mean = ~mean(. , na.rm = TRUE), 
                        sd = ~sd(. , na.rm = TRUE)), 
                   .names = "{col}_{fn}")) %>%
  arrange(land_use)

#
# Make site_label a factor for the stats
dat$site_label <- as.factor(dat$site_label)
levels(dat$site_label)
levels(dat$site)
#
#### Lmer for land use effects ####
#
#Since you have repeated measures, you need to account for the non-independence of those repeated samples.
#
pH_lmer <- lmer(pH ~ land_use + (1 | site/site_label), data = dat)
anova(pH_lmer)
summary(pH_lmer)
plot(pH_lmer) 
qqnorm(resid(pH_lmer))
qqline(resid(pH_lmer))
emmeans(pH_lmer, pairwise ~ land_use) # pairwise differences
#
#
EC_lmer <- lmer(log(EC_us_cm) ~ land_use + (1 | site/site_label), data = subset(dat, EC_us_cm <= 1500))
anova(EC_lmer)
summary(EC_lmer)
plot(EC_lmer) # has a bit of fanning untransformed, one outlier with log transformation
qqnorm(resid(EC_lmer))
qqline(resid(EC_lmer))
emmeans(EC_lmer, pairwise ~ land_use) # pairwise differences
#
#
NPOC_lmer <- lmer(log(NPOC_mg_l) ~ land_use + (1 | site/site_label), data = subset(dat, NPOC_mg_l <= 400))
anova(NPOC_lmer)
summary(NPOC_lmer)
plot(NPOC_lmer) # looks like one outlier and fanned
qqnorm(resid(NPOC_lmer))
qqline(resid(NPOC_lmer))
emmeans(NPOC_lmer, pairwise ~ land_use) # pairwise differences
#
#
F_lmer <- lmer(sqrt(F_mg_l) ~ land_use + (1 | site/site_label), data = dat)
anova(F_lmer)
summary(F_lmer)
plot(F_lmer) # a bit fanned but can't log transform due to 0s
qqnorm(resid(F_lmer))
qqline(resid(F_lmer))
emmeans(F_lmer, pairwise ~ land_use) # pairwise differences
#
#
Cl_lmer <- lmer(log(Cl_mg_l) ~ land_use + (1 | site/site_label), data = dat)
anova(Cl_lmer)
summary(Cl_lmer)
plot(Cl_lmer) # fanned
qqnorm(resid(Cl_lmer))
qqline(resid(Cl_lmer))
emmeans(Cl_lmer, pairwise ~ land_use) # pairwise differences
#
#
NO2_lmer <- lmer(sqrt(NO2_mg_l) ~ land_use + (1 | site/site_label), data = dat)
anova(NO2_lmer)  # p = 0.009783
summary(NO2_lmer)
plot(NO2_lmer) #unequal
qqnorm(resid(NO2_lmer))
qqline(resid(NO2_lmer))
emmeans(NO2_lmer, pairwise ~ land_use) # pairwise differences
#
#
NO3_lmer <- lmer(sqrt(NO3_mg_l) ~ land_use + (1 | site/site_label), data = dat)
anova(NO3_lmer)
summary(NO3_lmer)
plot(NO3_lmer)  # fanned
qqnorm(resid(NO3_lmer))
qqline(resid(NO3_lmer))
emmeans(NO3_lmer, pairwise ~ land_use) # pairwise differences
#
#
PO4_lmer <- lmer(PO4_mg_l^(1/3) ~ land_use + (1 | site/site_label), data = dat)
anova(PO4_lmer)
summary(PO4_lmer)
plot(PO4_lmer) # very fanned
qqnorm(resid(PO4_lmer))
qqline(resid(PO4_lmer))
emmeans(PO4_lmer, pairwise ~ land_use) # pairwise differences
#
#
SO4_lmer <- lmer(SO4_mg_l^(1/3) ~ land_use + (1 | site/site_label), data = dat)
anova(SO4_lmer)
summary(SO4_lmer)
plot(SO4_lmer)  # fanned
qqnorm(resid(SO4_lmer))
qqline(resid(SO4_lmer))
emmeans(SO4_lmer, pairwise ~ land_use) # pairwise differences
#
#
# Li has very few data points, did not run stats
#
#
Na_lmer <- lmer(Na_mg_l^(1/3) ~ land_use + (1 | site/site_label), data = dat)
anova(Na_lmer)
summary(Na_lmer)
plot(Na_lmer)  # fanned
qqnorm(resid(Na_lmer))
qqline(resid(Na_lmer))
emmeans(Na_lmer, pairwise ~ land_use) # pairwise differences
#
#
NH4_lmer <- lmer(NH4_mg_l^(1/3) ~ land_use + (1 | site/site_label), data = dat)
anova(NH4_lmer)
summary(NH4_lmer)
plot(NH4_lmer)  # very fanned
qqnorm(resid(NH4_lmer))
qqline(resid(NH4_lmer))
emmeans(NH4_lmer, pairwise ~ land_use) # pairwise differences
#
#
Mg_lmer <- lmer(log(Mg_mg_l) ~ land_use + (1 | site/site_label), data = dat)
anova(Mg_lmer)
summary(Mg_lmer)
plot(Mg_lmer)  # fanned
qqnorm(resid(Mg_lmer))
qqline(resid(Mg_lmer))
emmeans(Mg_lmer, pairwise ~ land_use) # pairwise differences
#
#
K_lmer <- lmer(K_mg_l^(1/3) ~ land_use + (1 | site/site_label), data = dat)
anova(K_lmer)
summary(K_lmer)
plot(K_lmer)  # very fanned
qqnorm(resid(K_lmer))
qqline(resid(K_lmer))
emmeans(K_lmer, pairwise ~ land_use) # pairwise differences
#
#
Ca_lmer <- lmer(Ca_mg_l^(1/3) ~ land_use + (1 | site/site_label), data = dat)
anova(Ca_lmer)
summary(Ca_lmer)
plot(Ca_lmer)  # fanned
qqnorm(resid(Ca_lmer))
qqline(resid(Ca_lmer))
emmeans(Ca_lmer, pairwise ~ land_use) # pairwise differences
#
#
P_lmer <- lmer(P_mg_l^(1/3) ~ land_use + (1 | site/site_label), data = dat)
anova(P_lmer)
summary(P_lmer)
plot(P_lmer)  # very fanned
qqnorm(resid(P_lmer))
qqline(resid(P_lmer))
emmeans(P_lmer, pairwise ~ land_use) # pairwise differences
#
#
Si_lmer <- lmer(sqrt(Si_mg_l) ~ land_use + (1 | site/site_label), data = dat)
anova(Si_lmer)
summary(Si_lmer)
plot(Si_lmer)  # fanned
qqnorm(resid(Si_lmer))
qqline(resid(Si_lmer))
emmeans(Si_lmer, pairwise ~ land_use) # pairwise differences
#
#
Al_lmer <- lmer(sqrt(Al_ug_l) ~ land_use + (1 | site/site_label), data = dat)
anova(Al_lmer)
summary(Al_lmer)
plot(Al_lmer)  # fanned
qqnorm(resid(Al_lmer))
qqline(resid(Al_lmer))
emmeans(Al_lmer, pairwise ~ land_use) # pairwise differences
#
#
As_lmer <- lmer(As_ug_l^(1/3) ~ land_use + (1 | site/site_label), data = dat)
anova(As_lmer)
summary(As_lmer)
plot(As_lmer)  # uneven
qqnorm(resid(As_lmer))
qqline(resid(As_lmer)) # really quite horrible even after transforming...
emmeans(As_lmer, pairwise ~ land_use) # pairwise differences
#
#
# Cd has too few points for stat analysis 
#
#
Cr_lmer <- lmer(Cr_ug_l^(1/3) ~ land_use + (1 | site/site_label), data = dat)
anova(Cr_lmer)
summary(Cr_lmer)
plot(Cr_lmer)  # uneven
qqnorm(resid(Cr_lmer))
qqline(resid(Cr_lmer)) # pretty bad even after transforming...
emmeans(Cr_lmer, pairwise ~ land_use) 
#
#
Cu_lmer <- lmer(sqrt(Cu_ug_l) ~ land_use + (1 | site/site_label), data = dat)
anova(Cu_lmer)
summary(Cu_lmer)
plot(Cu_lmer)  # fanned
qqnorm(resid(Cu_lmer))
qqline(resid(Cu_lmer))
emmeans(Cu_lmer, pairwise ~ land_use) # pairwise differences
#
#
Fe_lmer <- lmer(Fe_ug_l^(1/3) ~ land_use + (1 | site/site_label), data = subset(dat, Fe_ug_l <= 20000))
anova(Fe_lmer)
summary(Fe_lmer)
plot(Fe_lmer)  # very fanned and maybe 2 outliers
qqnorm(resid(Fe_lmer))
qqline(resid(Fe_lmer))
emmeans(Fe_lmer, pairwise ~ land_use)
#
#
Mn_lmer <- lmer(Mn_ug_l^(1/3) ~ land_use + (1 | site/site_label),  data = subset(dat, Mn_ug_l <= 10000))
anova(Mn_lmer)
summary(Mn_lmer)
plot(Mn_lmer)  # very fanned
qqnorm(resid(Mn_lmer))
qqline(resid(Mn_lmer))
emmeans(Fe_lmer, pairwise ~ land_use)
#
#
Ni_lmer <- lmer(sqrt(Ni_ug_l) ~ land_use + (1 | site/site_label),  data = dat)
anova(Ni_lmer)
summary(Ni_lmer)
plot(Ni_lmer)  # very fanned, maybe 2 outliers
qqnorm(resid(Ni_lmer))
qqline(resid(Ni_lmer))
emmeans(Fe_lmer, pairwise ~ land_use)
#
#
Pb_lmer <- lmer(Pb_ug_l ~ land_use + (1 | site/site_label),  data = dat)
anova(Pb_lmer)
summary(Pb_lmer)
plot(Pb_lmer)  #a bit uneven, not many data points
qqnorm(resid(Pb_lmer))
qqline(resid(Pb_lmer))
emmeans(Pb_lmer, pairwise ~ land_use) # pairwise differences
#
#
Zn_lmer <- lmer(sqrt(Zn_ug_l) ~ land_use + (1 | site/site_label),  data = subset(dat, Zn_ug_l <= 1000))
anova(Zn_lmer)
summary(Zn_lmer)
plot(Zn_lmer)  # one outlier, fanned
qqnorm(resid(Zn_lmer))
qqline(resid(Zn_lmer))
emmeans(Zn_lmer, pairwise ~ land_use)# pairwise differences
#
#
# IC not enough data 
#
#
# TN not enough data
#
#
P_lmer <- lmer(P_ug_l^(1/3)  ~ land_use + (1 | site/site_label),   data = subset(dat, P_ug_l <= 18))
anova(P_lmer)
summary(P_lmer)
plot(P_lmer)  # fanned and an outlier
qqnorm(resid(Zn_lmer))
qqline(resid(Zn_lmer))
emmeans(Zn_lmer, pairwise ~ land_use)# pairwise differences
#
#
Si_lmer <- lmer(Si_mg_l^(1/3)  ~ land_use +  (1 | site/site_label),   data = dat)
anova(Si_lmer)
summary(Si_lmer)
plot(Si_lmer)  # fanned
qqnorm(resid(Si_lmer))
qqline(resid(Si_lmer))
emmeans(Si_lmer, pairwise ~ land_use)# pairwise differences
#############################################################################
#### Lmer for peat type effects on ions  ####
# pH
pH_peat_type <- lmer(pH ~ peat_type + (1 | site/site_label), data = dat)
anova(pH_peat_type)
summary(pH_peat_type)
plot(pH_peat_type) 
qqnorm(resid(pH_peat_type))
qqline(resid(pH_peat_type))
#
# EC
EC_peat_type <- lmer(log(EC_us_cm) ~ peat_type + (1 | site/site_label), data = subset(dat, EC_us_cm <= 1500))
anova(EC_peat_type)
summary(EC_peat_type)
plot(EC_peat_type) 
qqnorm(resid(EC_peat_type))
qqline(resid(EC_peat_type))
#
# Al
Al_peat_type <- lmer(sqrt(Al_ug_l) ~ peat_type + (1 | site/site_label), data = dat)
anova(Al_peat_type)
summary(Al_peat_type)
plot(Al_peat_type) 
qqnorm(resid(Al_peat_type))
qqline(resid(Al_peat_type))
#
# Ca
Ca_peat_type <- lmer(Ca_mg_l^(1/3) ~ peat_type + (1 | site/site_label), data = dat)
anova(Ca_peat_type)
summary(Ca_peat_type)
plot(Ca_peat_type)  
qqnorm(resid(Ca_peat_type))
qqline(resid(Ca_peat_type))
#
# Cl
Cl_peat_type <- lmer(log(Cl_mg_l) ~ peat_type + (1 | site/site_label), data = dat)
anova(Cl_peat_type)
summary(Cl_peat_type)
plot(Cl_peat_type) 
qqnorm(resid(Cl_peat_type))
qqline(resid(Cl_peat_type))
#
# F
F_peat_type <- lmer(sqrt(F_mg_l) ~ peat_type + (1 | site/site_label), data = dat)
anova(F_peat_type)
summary(F_peat_type)
plot(F_peat_type) 
qqnorm(resid(F_peat_type))
qqline(resid(F_peat_type))
#
# K
K_peat_type<- lmer(K_mg_l^(1/3) ~ peat_type + (1 | site/site_label), data = dat)
anova(K_peat_type)
summary(K_peat_type)
plot(K_peat_type)  
qqnorm(resid(K_peat_type))
qqline(resid(K_peat_type))
#
# Mg
Mg_peat_type <- lmer(log(Mg_mg_l) ~ peat_type + (1 | site/site_label), data = dat)
anova(Mg_peat_type)
summary(Mg_peat_type)
plot(Mg_peat_type) 
qqnorm(resid(Mg_peat_type))
qqline(resid(Mg_peat_type))
#
# Na
Na_peat_type <- lmer(Na_mg_l^(1/3) ~ peat_type + (1 | site/site_label), data = dat)
anova(Na_peat_type)
summary(Na_peat_type)
plot(Na_peat_type)  
qqnorm(resid(Na_peat_type))
qqline(resid(Na_peat_type))
#
# SO4 
SO4_peat_type <- lmer(SO4_mg_l^(1/3) ~ peat_type + (1 | site/site_label), data = dat)
anova(SO4_peat_type)
summary(SO4_peat_type)
plot(SO4_peat_type)  
qqnorm(resid(SO4_peat_type))
qqline(resid(SO4_peat_type))
#
# Si
Si_peat_type <- lmer(Si_mg_l^(1/3)  ~ peat_type +  (1 | site/site_label),   data = dat)
anova(Si_peat_type)
summary(Si_peat_type)
plot(Si_peat_type)  
qqnorm(resid(Si_peat_type))
qqline(resid(Si_peat_type))
#
################################################################################
#### Correlation plot ####
#
# Subset numeric columns
dat_numeric <- select(dat, where(is.numeric))
#
# Drop columns with all NAs
dat_numeric <- dat_numeric %>% select(where(~ !all(is.na(.))))
#
# Drop P ug L
dat_numeric <- dat_numeric %>% select(-P_ug_l, -Li_mg_l, -lat, -lon) # not enough data points
#
#
# make data frame into matrix
dat_matrix <- rcorr(as.matrix(dat_numeric),  type = "spearman")
#
cor_mat <- dat_matrix$r
p_mat <- dat_matrix$P
#
# Make correlation plot
jpeg("LP3+_WQ_corrplot_spearman.jpeg", units="in", width=6, height=6, res=300)

corrplot(cor_mat,  type="upper", diag=FALSE, tl.col = "black", tl.cex = 0.9,
         p.mat = p_mat, sig.level = 0.05, insig = "blank") 

dev.off()

################################################################################
##### Run PCA for heavy metals ####
#
metals <- dat %>% select(site, land_use, month, Pb_ug_l, As_ug_l, Fe_ug_l, Ni_ug_l, Cr_ug_l, Cu_ug_l, Mn_ug_l) # Zn has too many NAs
# 2 rows are all NAs, filter out
metals <- metals %>% filter(!is.na(Mn_ug_l))
#
# Filter out rows where all metals are 0
metals <- metals %>%
  filter(rowSums(across(c("Pb_ug_l", "As_ug_l", "Fe_ug_l", "Ni_ug_l", "Cr_ug_l", "Cu_ug_l", "Mn_ug_l"))) != 0)
#40 rows filtered out
#
# subset numerical variables
metals_num <- select(metals, where(is.numeric))
#
# How many 0 values in each column?
metals_num %>%
  summarise(across(everything(), ~ sum(. == 0, na.rm = TRUE) / n() * 100)) -> zero_percent

#
#
# Run PCA (standardizing the data)
pca_result <- prcomp(metals_num, center = TRUE, scale. = TRUE)
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
my.col.var <- c("Cropland" = "#D8B4F8", "Grassland" = "#FDE68A", 
                            "Rewetted extraction"= "#D16BA5" , "Rewetted bog" = "#A5F2D4" , 
                            "River/HLC" ="#FFB347",  "Semi-natural fen" ="#B5E48C", 
                            "Semi-natural bog" = "#6DA34D" ) #set colour palette
#
#
#
tiff("PCA_LP3+_metals_pca.tiff", units="in", width=8, height=6, res=300)
PCA_fig <- fviz_pca_biplot(pca_result, 
                              col.ind = metals$land_use,
                              addEllipses = TRUE, label = "var",
                              pointsize=3,
                              alpha.ind=0.5,
                              mean.point=F,
                              palette=my.col.var,
                              col.var = "black", repel = TRUE,
                              legend.title = " ") + ggtitle(NULL) 
PCA_fig
dev.off()
#################################################################################
#### Map of heavy metals ####
#
# Get UK map data
uk_map <- map_data("world", region = "UK")

# Plot Pb 
Pb_map <- ggplot() +
  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(dat, Pb_ug_l > 0), aes(x = lon, y = lat, size = Pb_ug_l), color = "#66B2FF", alpha = 0.6) +
  geom_point(data = data.frame(city = c("London", "Manchester"),
                               lon = c(-0.1276, -2.2426),
                               lat = c(51.5074, 53.4808)),
             aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"),
                              lon = c(-0.1276, -2.2426),
                              lat = c(51.5074, 53.4808)),
            aes(x = lon, y = lat, label = city),
            nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("Pb " * mu * "g " * L^{-1}),  range = c(2, 10),  breaks = c(10, 30, 80)) +
  coord_fixed(1.3) +  # Fix aspect ratio for maps
  theme_minimal() +   labs( x = "Longitude", y = "Latitude")
Pb_map


# Plot As
As_map <- ggplot() +
  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(dat, As_ug_l > 0), aes(x = lon, y = lat, size = As_ug_l), color = "#8F9779", alpha = 0.6) +
  geom_point(data = data.frame(city = c("London", "Manchester"),
                               lon = c(-0.1276, -2.2426),
                               lat = c(51.5074, 53.4808)),
             aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"),
                              lon = c(-0.1276, -2.2426),
                              lat = c(51.5074, 53.4808)),
            aes(x = lon, y = lat, label = city),
            nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("As " * mu * "g " * L^{-1}),  range = c(2, 10),  breaks = c(20, 80, 200)) +
  coord_fixed(1.3) +  # Fix aspect ratio for maps
  theme_minimal() +   labs( x = "Longitude", y = "Latitude")
As_map


# Plot Fe
Fe_map <- ggplot() +
  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(dat, Fe_ug_l > 0), aes(x = lon, y = lat, size =Fe_ug_l), color = "orange", alpha = 0.6) +   geom_point(data = data.frame(city = c("London", "Manchester"),
                               lon = c(-0.1276, -2.2426),
                               lat = c(51.5074, 53.4808)),
             aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"),
                              lon = c(-0.1276, -2.2426),
                              lat = c(51.5074, 53.4808)),
            aes(x = lon, y = lat, label = city),
            nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("Fe " * mu * "g " * L^{-1}),  range = c(2, 10), breaks = c(10, 10000, 50000)) +
  coord_fixed(1.3) +  # Fix aspect ratio for maps
  theme_minimal() +   labs( x = "Longitude", y = "Latitude")
Fe_map


# Plot Ni
Ni_map <- ggplot() +
  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(dat, Ni_ug_l > 0), aes(x = lon, y = lat, size =Ni_ug_l), color = "#C8A2C8", alpha = 0.6) +   geom_point(data = data.frame(city = c("London", "Manchester"),
                                                                                                                                                    lon = c(-0.1276, -2.2426),
                                                                                                                                                    lat = c(51.5074, 53.4808)),
                                                                                                                                  aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"),
                              lon = c(-0.1276, -2.2426),
                              lat = c(51.5074, 53.4808)),
            aes(x = lon, y = lat, label = city),
            nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("Ni " * mu * "g " * L^{-1}),  range = c(2, 10), breaks=c(10, 40, 80)   ) +
  coord_fixed(1.3) +  # Fix aspect ratio for maps
  theme_minimal() +   labs( x = "Longitude", y = "Latitude")
Ni_map

# Plot Cr
Cr_map <- ggplot() +
  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(dat, Cr_ug_l > 0), aes(x = lon, y = lat, size =Cr_ug_l), color = "#db4437", alpha = 0.6) +   geom_point(data = data.frame(city = c("London", "Manchester"),lon = c(-0.1276, -2.2426), lat = c(51.5074, 53.4808)), aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"), lon = c(-0.1276, -2.2426), lat = c(51.5074, 53.4808)),  aes(x = lon, y = lat, label = city),  nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("Cr " * mu * "g " * L^{-1}),  range = c(2, 10), breaks=c(4, 8, 12)   ) +
  coord_fixed(1.3) +  # Fix aspect ratio for maps
  theme_minimal() +   labs( x = "Longitude", y = "Latitude")
Cr_map

# plot Cu
Cu_map <- ggplot() +
  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(dat, Cu_ug_l > 0), aes(x = lon, y = lat, size =Cu_ug_l), color = "#40E0D0", alpha = 0.6) +   geom_point(data = data.frame(city = c("London", "Manchester"),lon = c(-0.1276, -2.2426), lat = c(51.5074, 53.4808)), aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"), lon = c(-0.1276, -2.2426), lat = c(51.5074, 53.4808)),  aes(x = lon, y = lat, label = city),  nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("Cu " * mu * "g " * L^{-1}),  range = c(2, 10)  , breaks=c(10, 30, 60) ) +   coord_fixed(1.3) +  # Fix aspect ratio for maps
  theme_minimal() +   labs( x = "Longitude", y = "Latitude")
Cu_map

# plot Cd
Cd_map <- ggplot() +  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(dat, Cd_ug_l > 0), aes(x = lon, y = lat, size = Cd_ug_l), color = "#00008B", alpha = 0.6) +   geom_point(data = data.frame(city = c("London", "Manchester"),
                               lon = c(-0.1276, -2.2426),
                               lat = c(51.5074, 53.4808)),
             aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"),
                              lon = c(-0.1276, -2.2426),
                              lat = c(51.5074, 53.4808)),
            aes(x = lon, y = lat, label = city),
            nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("Cd " * mu * "g " * L^{-1}),  range = c(2, 10),  breaks = c(1, 5, 10)) +   coord_fixed(1.3) +    theme_minimal() +   labs( x = "Longitude", y = "Latitude")
Cd_map


# plot Mn
Mn_map <- ggplot() +
  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(dat, Mn_ug_l > 0), aes(x = lon, y = lat, size =Mn_ug_l), color = "#DAA520", alpha = 0.6) +   geom_point(data = data.frame(city = c("London", "Manchester"),lon = c(-0.1276, -2.2426), lat = c(51.5074, 53.4808)), aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"), lon = c(-0.1276, -2.2426), lat = c(51.5074, 53.4808)),  aes(x = lon, y = lat, label = city),  nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("Mn " * mu * "g " * L^{-1}),  range = c(2, 10) ,  , breaks=c(10, 100, 1000, 9000) ) +   coord_fixed(1.3) +  # Fix aspect ratio for maps
  theme_minimal() +   labs( x = "Longitude", y = "Latitude")
Mn_map



# Plot Zn
Zn_map <- ggplot() +
  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(dat, Zn_ug_l > 0), aes(x = lon, y = lat, size = Zn_ug_l), color = "#DE3163", alpha = 0.6) +   geom_point(data = data.frame(city = c("London", "Manchester"),
                               lon = c(-0.1276, -2.2426),
                               lat = c(51.5074, 53.4808)),
             aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"),
                              lon = c(-0.1276, -2.2426),
                              lat = c(51.5074, 53.4808)),
            aes(x = lon, y = lat, label = city),
            nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("Zn " * mu * "g " * L^{-1}),  range = c(2, 10), breaks = c(10, 100, 1000)) +
  coord_fixed(1.3) +  # Fix aspect ratio for maps
  theme_minimal() +
  labs( x = "Longitude", y = "Latitude")
Zn_map

#plot Hg # not this is from a separate data frame

Hg_map <- ggplot() +
  geom_polygon(data = uk_map, aes(x=long, y=lat, group=group),
               fill="gray90", color="black") +
  geom_point(data = subset(mercury, Hg_ug_l > 0), aes(x = lon, y = lat, size =Hg_ug_l), color = "#A020F0", alpha = 0.6) +   geom_point(data = data.frame(city = c("London", "Manchester"),lon = c(-0.1276, -2.2426), lat = c(51.5074, 53.4808)), aes(x = lon, y = lat), color = "black", size = 2) +
  geom_text(data = data.frame(city = c("London", "Manchester"), lon = c(-0.1276, -2.2426), lat = c(51.5074, 53.4808)),  aes(x = lon, y = lat, label = city),  nudge_y = 0.3, color = "black", size = 3) +
  scale_size_continuous(name = expression("Hg " * mu * "g " * L^{-1}),  range = c(2, 10) ,  , breaks=c(0.02, 0.05) ) +   coord_fixed(1.3) +  # Fix aspect ratio for maps
  theme_minimal() +   labs( x = "Longitude", y = "Latitude")
Hg_map



#combine
jpeg("LP3+_heavy_metals_maps.jpeg", units="in", width=10, height=15, res=350)

combine1 <- ggarrange(
As_map+ theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = 0, b = 0, l = -0.8, r = -0.8, unit = "cm")),
Cd_map+ theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = 0, b = 0, l = -0.8, r = -0.8, unit = "cm")),
Cr_map+ theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = 0, b = 0, l = -0.8, r = -0.8, unit = "cm")),
Cu_map + theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = -0.5, b = 0, l = -0.8, r = -0.8, unit = "cm")), 
Fe_map+ theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = -0.5, b = 0, l = -0.8, r = -0.8, unit = "cm")),  
Hg_map+ theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = -0.5, b = 0, l = -0.8, r = -0.8, unit = "cm")),  
Mn_map+ theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = -0.5, b = 0, l = -0.8, r = -0.8, unit = "cm")),
Ni_map+ theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = -0.5, b = 0, l = -0.8, r = -0.8, unit = "cm")), 
Pb_map + theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = -0.5, b = 0, l = -0.8, r = -0.8, unit = "cm")),  
Zn_map+ theme(axis.title = element_blank(), axis.text = element_blank(),  panel.grid = element_blank(), plot.margin = margin(t = -0.5, b = 0, l = -0.8, r = -0.8, unit = "cm")),  
ncol = 2, nrow = 5,  align = "v",  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J") )   
combine1

dev.off()

##################################################
#### Save dat to csv file ####
#
#
write.csv(dat, "C:/Users/teres/Documents/LowlandPeat3/LP3+ Water quality data/Data/LP3+_wq_dat_combined.csv")
#
#