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
# Remove first row, is empty; and empty columns
dat1 <- dat1 %>% 
  slice(-1) %>%       
  select(-c(6:10)) 
# 
# Choose just rows with DOC data
dat2 <- dat2 %>%
  filter(!is.na(`DOC (mg C l^-1)`))  
#
# For now, exclude Luke's data from Mike's data, and use what he reported in the Sharepoint
dat1 <- dat1 %>%
  filter(Site != "Luke/Wrights")
#
#
# Rename columns
colnames(dat1)[colnames(dat1) == "Sample"] <- "sample_code"
colnames(dat1)[colnames(dat1) == "Date"] <- "date"
colnames(dat1)[colnames(dat1) == "[DOC]310"] <- "DOC_mg_l"
colnames(dat2)[colnames(dat2) == "DOC (mg C l^-1)"] <- "DOC_mg_l"
colnames(dat2)[colnames(dat2) == "TC (mg C l^-1)"] <- "TC_mg_l"
colnames(dat2)[colnames(dat2) == "DIC (mg C l^-1)"] <- "DIC_mg_l"
#
# # Format date columns
dat1$date <- as.character(dat1$date)
dat2$date <- as.character(dat2$date)
#
# Combine data
dat <- rbind(
  dat1 %>% select(date, sample_code, DOC_mg_l),  
  dat2 %>% select(date, sample_code, DOC_mg_l) )
#
#
#
# Make a new column for site and ditch
#Add a new column for site and ditch #
datx <- dat %>%
  mutate(site = str_extract(sample_code, "(?<=^\\w-)[A-Za-z0-9-]+")) %>%
  mutate(site = str_extract(site, "^[A-Za-z]+(?:-[A-Za-z0-9]+)?")) %>%
  mutate(site = ifelse(str_detect(site, "^(LC|SW)"), str_remove(site, "-\\d+$"), site)) %>%
  mutate(ditch = str_extract(sample_code, "\\d+(?=-DOC$)"))  %>%
  mutate(ditch = ifelse(str_detect(sample_code, "\\."), str_extract(sample_code, "(\\d+)(?=\\.)"), ditch)) %>%
  mutate(ditch = ifelse(str_detect(site, "^(GC)"), str_extract(sample_code, "\\d+(?=$)"),  ditch)) %>%  ##help
  select(date, sample_code, site, ditch, everything()) 




