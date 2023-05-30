#######################################################################
########## Calculate effective fetch from raw fetch vectors ###########
#######################################################################

# Description: ----

# This script takes fetch distances in 32 directions around grid points,
# the output of Python or R fetch script, and calculates for each point:
#
# 1) Minimum fetch: the shortest fetch distance (proxy for distance to coast)
# 2) Sum fetch: the same of all 32 fetch distances (proxy for wave exposure)
# 3) Effective fetch: in 8 directions (N, NE, E, SE, S, SW, W, NW); 
#    needed to calculate Relative Exposure Index (REI)*
#
# *For description of effective fetch and REI, see:
#
# Fonseca MS, Bell SS (1998) Influence of physical setting on seagrass
#   landscapes near Beaufort, North Carolina, USA. Mar Ecol Prog Ser
#   171:109-121

# Requirements: ----

# csv file(s) containing outputs of Python or R fetch script
# i.e. fetch distance (metres) in 32 directions around grid points
# One row per point

# Load packages ----

library(dplyr)
library(purrr)
library(tidyr)
library(data.table)
library(sf)
library(vroom)

# Source functions ----

source('Code/roll_recycle_fun.R')

# Controls ----

# relative contribution of fetch vectors surrounding each heading to calculation of effective fetch

weights <- cos(c(45,33.75,22.5,11.25,0,11.25,22.5,33.75,45) *(pi/180))

# Path to project directory (string)

projDir <- "C:/REI"

# Coordinate reference system of fetch calculations (integer EPSG code)

crs_fetch <- 26920 # (UTM20)
# crs_fetch <- 26921 # (UTM21)
# crs_fetch <- 4326 # (WGS84)

# Script used to generate fetch calculates (Python or R)

fetch_script = "Python"
# fetch_script = "R"

# Combine fetch data from all points into one data table ----

# list of fetch file(s)

fetch_csv <- list.files(path = paste(projDir, "Data", sep = "/"),
                        pattern = '^fetch_.*csv$',
                        ignore.case = TRUE,
                        recursive = TRUE,
                        full.names = TRUE)

# If using Python script to calculate fetch

# Combine csv files from separate clusters of points 
# and reshape to long format (if using Python script)

if(fetch_script == "Python") {
  
  fetch_summary <- map(fetch_csv, fread) %>%
    rbindlist(idcol = 'cluster') %>%
    mutate(., site_names = seq_along(cluster)) %>%
    pivot_longer(data = .,
                 cols = starts_with("bearing"),
                 values_to = "fetch_length_m",
                 names_to = "direction",
                 names_prefix = "bearing",
                 names_transform = list(direction = as.numeric)) %>%
    setDT()
  
} else
  
  if(fetch_script == "R") {
    
    fetch_summary <- map(fetch_csv, fread) %>%
      rbindlist(idcol = 'cluster') %>% 
      setDT()
    
  }


# Calculate minimum fetch (proxy for distance to coast) and sum fetch (proxy for wave exposure)

fetch_proxies <- fetch_summary[, .(min_fetch = min(fetch_length_m),
                                 sum_fetch = sum(fetch_length_m)),
                             by = .(site_names,X,Y)]

vroom_write(fetch_proxies, "Data/fetch_proxies.csv", delim = ",")

# Calculate effective fetch ----

fetch_eff <- fetch_summary %>%
  dplyr::select(X, Y, direction, fetch_length_m) %>% 
  group_by(X, Y) %>% 
  nest() %>% 
  bind_cols(., site_names = as.character(seq_len(nrow(.)))) %>%   
  # Calculate effective fetch and place in tibble
  mutate(fetch_eff = map(data, ~roll.recycle(.$fetch_length_m, 9, 8, by = 4)),
         fetch_eff = map(fetch_eff, ~as.vector((weights %*% .)/sum(weights))),
         fetch_eff = map(fetch_eff, ~tibble(direction = c(360, seq(45, 315, 45)), fetch = .))) %>% 
  st_as_sf(coords = c("X","Y"), crs = crs_fetch)

saveRDS(fetch_eff, "Data/fetch_effective.rds")

# Move onto wind data interpolation: (4WindData.R)
