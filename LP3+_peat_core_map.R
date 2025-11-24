# Make map of peat core locations for LowlandPeat3+ 
# The peaty soil data is a very heavy file, so it is quite slow to load, both here in R and in QGIS. In QGIS there are added issues with the British National Grid Coordinate system not allowing a border grid with lat lon coordinates... which is why I came to R. 
#
# Load necessary packages
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(ggspatial)
library(here)
library(terra)
#
#
here::i_am("Here.txt")
here()
###
# Load in data
#
# Read your site points
sites <- read.csv(here("LP3+ Final report", "Maps", "peat core coordinates.csv"))
sites <- st_as_sf(sites, coords = c("Longitude", "Latitude"), crs = 4326)

# Get UK extent
uk <- ne_countries(scale = "medium", country = "United Kingdom", returnclass = "sf")
#
england <- ne_states(country = "United Kingdom", returnclass = "sf") %>%
  filter(geonunit == "England") 
#
england <- england %>% 
  st_union() %>%       # dissolve all subunits
  st_as_sf() 
#
cities <- data.frame(
  name = c("Manchester", "London"),
  lon  = c(-2.2426, -0.1276), 
  lat  = c(53.4808, 51.5074) )
#
cities_sf <- st_as_sf(cities, coords = c("lon", "lat"), crs = 4326)
#
# Read your peat soil map
peat <- st_read("C:/Users/teres/Documents/LowlandPeat3/LP3+ Final report/Maps/peaty_soil_extent_v1_8859678099411592851.gpkg") # faster than geodatabase
#
peat_vect <- vect(peat) # make terra vector #takes about 3 min
#
# 1. Create a raster template at coarse resolution (e.g., 500 m)
r <- rast(peat_vect, resolution = 500)

# 2. Rasterize the polygons
r <- rasterize(peat_vect, r, field = 1)

# 3. Convert raster back to a single polygon (dissolve all cells)
peat_union <- as.polygons(r, dissolve = TRUE)

# 4. Transform to WGS84 for mapping
peat_union <- project(peat_union, "EPSG:4326")

# 5. Plot
plot(peat_union)
#
# Make sf object for plotting
peat_union_sf <- st_as_sf(peat_union)
#
#
#
###
# 
#tiff("peat_core_map_v1.tiff", units="in", width=6.5, height=4, res=300)
jpeg("peat_core_map_v1.jpeg", units="in", width=12, height=8, res=300)

core_map <- ggplot() +
    geom_sf(data = uk, fill = "grey60", color = "grey60") +
  #geom_sf(data = england, fill = NA, color = "white", size = 1) +
  geom_sf(data = peat_union_sf, fill = "#b05c2c", color = NA, alpha = 0.8) +
  geom_sf(data = sites, colour="black", fill = "#57B9FF", size = 3, shape=21, alpha=0.75) +
  geom_sf(data = cities_sf, color = "black", size = 1.5) +  
  geom_text(data = cities, aes(x = lon, y = lat, label = name), nudge_y = 0.1, color = "black", size = 4.5) +
  coord_sf(xlim = c(-3.8, 1), ylim = c(51.15, 54.1), expand=FALSE) + 
  scale_x_continuous(    breaks = seq(-3, 1, 1),  labels = function(x) paste0(x, "°")   ) +
  scale_y_continuous(    breaks = seq(52, 54, 1),  labels = function(x) paste0(x, "°")  ) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme_minimal(base_family = "sans", base_size = 12) + theme(axis.title = element_blank(), panel.border = element_rect(color = "black", fill = NA, linewidth = 1, ),  axis.ticks = element_line(), panel.grid = element_blank() ) 
core_map

dev.off()

#
#
#

# plot with terra
uk_vect <- vect(uk)
england_vect <- vect(england)
cities_vect <- vect(cities_sf)


plot(uk_vect, col="grey80", border="grey50")
plot(england_vect, add=TRUE, border="white", lwd=1)
plot(peat_union, add=TRUE, col="#b05c2c", border=NA)
plot(cities_vect, add=TRUE, col="black", pch=19, cex=1.5)
