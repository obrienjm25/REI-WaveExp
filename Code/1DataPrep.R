#######################################################################
########## Prepare spatial inputs for fetch calculations ##############
#######################################################################

# Description: ----

# This script prepares the points features and land polygons inputs
# for fetch calculations. A digital elevation model (50 m resolution)
# is used as the basis for the creation of a fishnet grid and to contrain
# input points to the 50-m depth contour

# 1) Crop land polygons to within 300 km of input grid and create 5-km buffer
# 2) Resample raster to coarser resolution
# 3) Convert raster to points, filter by depth, and exclude features > 5km from coast
#

# Requirements: ----

# Shapefile with land polygons of extended study area
# digital elevation model in raster format

#### Load Packages ####

library(dplyr)
library(tidyr)
library(sf)
library(raster)

# 1) Crop and buffer land polygons ====

# Speed up raster processes
rasterOptions(chunksize = 1e+05, maxmemory = 1e+09)

# Read in digital elevation model (35 m)

dem_35 <- raster("Data/Rasters/dem35c_5c.tif") # 35 m digital elevation model

# Crop coastline for extended NW Atlantic to within 300 km of raster extent

coast_atl_50k <- st_read(dsn = "Data/Coast50k_ext/Coastline50k.gdb", 
                         layer = "Land_AtlCanada_ESeaboardUS") %>%  #50k coastline
  st_transform(., crs = 26920) %>% # project to UTM20
  st_crop(., st_buffer(st_as_sfc(st_bbox(dem_35)), 300000)) %>% # crop to buffered extent
  st_cast(., "MULTIPOLYGON") # cast to multipolygon

# Write cropped coast polygon to shapefile
st_write(coast_atl_50k,
         dsn = "Data/Shapefiles",
         layer = "coast50k_crop",
         driver = "ESRI Shapefile")

# Create 5-km land buffer
coast_5km_buffer <- st_buffer(coast_atl_50k, 5000)

# Write to shapefile
st_write(coast_5km_buffer,
         dsn = "Data/Shapefiles",
         layer = "coast50k_buff_5km.shp",
         driver = "ESRI Shapefile")

# 2) Resample raster to coarser resolution ====

# 50-m res template
raster50 <- raster(extent(dem_35), res = 50, crs = crs(dem_35)) 

# resample 35-m to 50-m
dem_50 <- resample(dem_35, raster50) 

# write to geotiff
writeRaster(dem_50, filename = "Data/dem50c_5c.tif")

# 3) Raster to points, filter by depth, apply 5-km land buffer

# Extract raster values to tibble
depth_sf_50 <- tibble(Lon = coordinates(dem_50)[,1], 
                      Lat = coordinates(dem_50)[,2],
                      depth = raster::values(dem_50)) %>% 
  #Confine points to 50 m or shallower
  filter(., depth <= 50) %>% # 7844271 pts
  st_as_sf(., coords = c("Lon","Lat"), crs = 26920) # convert to sf object

# Remove points further than 5 km from coast
depth_sf_50buff <- depth_sf_50[st_intersects(depth_sf_50, coast_5km_buff) %>% 
                                 lengths > 0,] # 5797332 pts

# Check if any points intersect land and discard points on land

on_land_50 <- st_intersects(depth_sf_50buff, coast_atl_50k) %>%
  lengths(.) == 0
depth_sf_50buff <- depth_sf_50buff[on_land_50,] # 5659941 pts

# Save pts as .rds files and Shapefiles
saveRDS(depth_sf_50buff, "Data/REI_MAR_pts_50m.rds")

st_write(obj = depth_sf_50buff, 
         dsn = "Data/Shapefiles", 
         layer = "REI_MAR_pts_50m",
         driver = "ESRI Shapefile")

