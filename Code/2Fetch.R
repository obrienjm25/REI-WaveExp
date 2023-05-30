#######################################################################
########## Calculate unweighted fetch lengths ###########
#######################################################################

# Description: ----

# This script calculates fetch distances in 32 directions around sites,
# using grid points and land shapefiles generated in 1DataPrep.R
#

# Requirements: ----

# 1) Shapefile with polygons delineating coastline
# 2) Shapefile(s) with points delineating sites

# NOTE: Both shapefiles should be in the same projected coordinate
# reference system

# Load packages ----

library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(vroom)

# Load functions ----

source("Code/fetch_parallel.R") # fetch function - parallel processing
source("Code/roll_recycle_fun.R") # circular vector indexing function

# Import land polygons ----

land <- st_read("Data/Shapefiles/coast50k_buff_5km.shp")

# Import study site locations ----

fetch_sites <- st_read("Data/Shapefiles/REI_MAR_pts_50m.shp")

fetch_sites$site_names <- seq(1:nrow(fetch_sites))

# Fetch calculations ----

# Calculate fetch in 11.25 deg. increments for each Site.Location

my_fetch_proj = fetch_parallel(polygon_layer = land,
                               site_layer = fetch_sites,
                               max_dist = 300,
                               n_directions = 8,
                               site_names = site_names)

# Project back to lat/lon

my_fetch_latlon <- st_transform(my_fetch_proj, 4326) %>% # WGS84
  bind_cols(X = st_coordinates(.)[,1],
            Y = st_coordinates(.)[,2],
            .) %>% 
  dplyr::select(-geometry)

# Write object with fetch to csv and RDS files ----

vroom_write(my_fetch_latlon, 
            file = "Data/fetch_unweighted.csv", 
            delim = ",")

saveRDS(my_fetch_latlon, 
        file = "Data/fetch_unweighted.rds")

# Move onto wind data interpolation: (3EffectiveFetch.R)