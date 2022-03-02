#######################################################################
##########Environmental Data Layers for SG sdm#########################
#######################################################################

#### Load Packages ####

library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(raster)
library(rgis)

# Housekeeping ----

# Speed up raster processes

rasterOptions(chunksize = 1e+05, maxmemory = 1e+09)

# Read in interpolated wind data ----

# Raster stack of wind frequency by direction

wind_freq <- list.files(path = "Data/spline_era5/",
                   pattern = "^freq.+tif$",
                   full.names = T) %>% 
  stack(.) 

# Raster stack of average daily maximum wind speed by direction

wind_spd <- list.files(path = "Data/spline_era5/",
                  pattern = "^mx.+tif$",
                  full.names = T) %>% 
  stack(.) 

# Combine fetch data and wind data ----

# Read in effective fetch data

Fetch_eff <- readRDS("Data/fetch_effective.rds")

# Extract frequency and speed values for fetch locations

wind_combine <- fast_extract(ras = wind_freq, 
                                   sf = Fetch_eff) %>% 
  fast_extract(ras = wind_spd, sf = .)

# Calculate Relative Exposure Index ----

expo_by_site <- wind_combine %>%
  rowwise() %>% 
  mutate(freq_total = list(cbind(c_across(contains("freq")))),
         max_wind = list(cbind(c_across(contains("mx")))),
         wind_dir_freq = list(tibble(max_wind, 
                                freq_total, 
                                direction = c(135,180,225,270,315,360,45,90)))) %>%
  ungroup() %>% 
  mutate(., wind_dir_freq = map(wind_dir_freq, function(.data) {
    .data %>% 
      dplyr::arrange(., direction)
  }),
  fetch_eff = map(fetch_eff, function(.data){
    .data %>% 
      dplyr::arrange(., direction)
  })) %>% 
  mutate(REI = map2_dbl(wind_dir_freq, fetch_eff, # calculate relative exposure
                        ~sum((.x$max_wind*.x$freq_total*.y$fetch), na.rm = T))) %>%  
  dplyr::select(site_names, REI) # keep site, REI, and geometry

# Save REI object

saveRDS(expo_by_site, 'Data/REI.rds')

# Write to csv

st_write(obj = expo_by_site,
         dsn = 'Data/REI.csv',
         layer_options = 'GEOMETRY=AS_XY')
