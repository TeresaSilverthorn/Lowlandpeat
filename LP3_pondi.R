#### Script to measure GHG flux of POndi deployments ####
#
# Load necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(gasfluxes)
library(data.table)
#
# Set wd() for figures
setwd("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Figures")
#
# Load in pondi data
dat <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Floating chamber (Pondi)/Data/SENS-0001-02-0006_0.csv")
head(dat)
# Convert timestamp to posit format
dat$Time <- as.POSIXct(dat$Time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
#
# Subset just data from the sampling data, June 25, 2025
#
dat <- dat[dat$Time >= as.POSIXct("2025-06-25"), ]
#
#
# Load in ancillary data (start and end times)
ancil_dat <- read.csv("C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Floating chamber (Pondi)/Data/pondi_start_end_times_25-06-2025.csv")
head(ancil_dat)
#
#add the date to the start and end time columns for 
ancil_dat$start_time <- parse_date_time(paste(ancil_dat$date, ancil_dat$start_time), 
                                         orders = c("dmy HMS", "dmy HM"), tz = "UTC")
#
ancil_dat$end_time <- parse_date_time(paste(ancil_dat$date, ancil_dat$end_time), 
                                        orders = c("dmy HMS", "dmy HM"), tz = "UTC")
#
#
# Plot CO2
plot <- ggplot(dat, aes(x = Time, y = CO2..ppm.)) +
  geom_point() +   geom_line() +   labs(x = "Time",  y = "CO2 (ppm)") +   theme_minimal()
plot
#
#
##########################################################################
#### Clip the data by start and end times ####
###########################################################

ID <- ancil_dat2$site
startT<-ancil_dat2$start_time #start times
endT<-ancil_dat2$end_time  # end times 

for(i in 1:length(startT)){
  st<-startT[i]
  se<-endT[i]
  id<-ID[i]
  data<-dat[dat$Time >= st & dat$Time <= se,]
  data$ID<-id
  
  if(i==1){
    assign(paste("data",i, sep="_"),data)
  } else {
    assign(paste("data",i, sep="_"),data)
    assign(paste("data",i, sep="_"),rbind(get(paste("data",i, sep="_")),get(paste("data",i-1, sep="_"))))
  }
}

dat_clipped<-get(paste("data",length(startT),sep="_"))

str(dat_clipped) 

################################################################################

# Plot CO2
jpeg("LP3_pondi_25-06-2025.jpg", units="in", width=10, height=3, res=300)

plot <- ggplot(dat_clipped, aes(x = Time, y = CO2..ppm., colour=ID)) +
  geom_point() +   geom_line() +   labs(x = "Time",  y = "CO2 (ppm)") +   theme_minimal()
plot

dev.off()

# Observations:
#RG-R8-1 clip 1st points
#RG-R6-2 clip 1st point
#TP-B-2 clip last point
#SS-A-1 clip first to points

ancil_dat2 <- ancil_dat %>%
  mutate(start_time = ifelse(site %in% c("RG-R6-2", "SS-A-1"), 
                             start_time + 60,  start_time)) %>%
  mutate(start_time = ifelse(site %in% c("RG-R8-1"), 
                             start_time + 90,  start_time)) %>%
  mutate(end_time = ifelse(site == "TP-B-2", end_time - 60,  end_time)) %>%
  mutate(start_time = as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC"))

# Go back and re-run clipping with ancil_dat2
#
######  Convert Picarro_dat concentration from ppm to ug-N/L using the ideal gas law (PV=nRT) for input to gasfluxes package.  #############  Note that ppm = uL/L

# ug-N/L = ((LGR_dat concentration in ppm  * molecular mass of carbon *1 atm )) / (0.08206	L·atm/K·mol * temperature in K )

head(dat_clipped)

dat_clipped$CO2_mg_L <- ((dat_clipped$CO2..ppm.  * 44.01 * 1 )/1000) / (0.08206 *(dat_clipped$CO2.Temp..deg.C. + 273.15))

#Note here that we use the specific temperatures but assume 1 atm for pressure (could refine later)
#
# From the pondi publication: 𝑉 is the headspace volume in the chamber (typically 0.0131 m3--13.1 L); and 𝐴 is the area of the chamber exposed to the water (typically 0.1282 m2)

# from Rebecca McKenzie website says external measurements are diameter 31.5cm and height 13.5 cm , but you will need to measures how low the bowl sits in the water and take that off the height. So considering a height of 10cm, (which would be 7.79311328 L and 0.078 m2)

# make a new column for volume and area
dat_clipped$volume_L <- 7.79
dat_clipped$area_m2 <- 0.078
#
#then set  the initial time of each measure to 0h 
#use Naiara's function to rest the min time to each time

rescale <- function(x) (x-min(x))

#apply this function to all epoch_time (seconds) of each measure, 
#and divide by 36000 (hours)
dat_clipped <- setDT(dat_clipped)[,c("flux_time"):=.(rescale(Timestamp/3600)),by=.(ID)]
##
#
#
#Run the package to calculate the gas flux rate
CO2.results <- gasfluxes(dat_clipped, .id = "ID", .V = "volume_L", .A = "area_m2",.times = "flux_time", .C = "CO2_mg_L",method = c("linear"), plot = T) #can turn plot to FALSE if the number of plots was getting out of hand

CO2.results #linear.f0 units are in mg CO2 m2 h

head(CO2.results)

# rename linear.fo
names(CO2.results)[names(CO2.results) == "linear.f0"] <- "mg_CO2_m2_h"

CO2.results$g_CO2_m2_d <- CO2.results$mg_CO2_m2_h/1000*24

write.csv (CO2.results, "C:/Users/teres/Documents/LowlandPeat3/LP3 aquatic GHG sampling/Floating chamber (Pondi)/Data/CO2_fluxes.csv")



