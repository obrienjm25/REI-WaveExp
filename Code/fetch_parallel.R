############################################################
####    Run fetch function with parallel processing     ####
############################################################

# load libraries

library(parallel)
library(multidplyr)

# source fetch function

source('fetch.R')

# Run fetch function in parallel using multidplyr

#' polygon_layer = sf object where the
#'                polygon geometries represent any obstructions to fetch
#'                calculations including the coastline, islands and/or 
#'                exposed reefs.
#' site_layer = sf object where the point 
#'                geometries represent the site locations.
#' max_dist = numeric. Maximum distance in kilometres (default 300). This 
#'                will need to be scaled manually if the units for the CRS are 
#'                not 'm'.
#' n_directions = numeric. The number of fetch vectors to calculate per 
#'                quadrant (default 8).
#' site_var (character) = column name in site_layer sf object holding variable
#'                        for site names

fetch_parallel = function(polygon_layer, site_layer, max_dist = 300, 
                          n_directions = 8, site_var){
  
  # Determine number of cores available minus 1
  cl <- detectCores() - 1
  
  # Set up new cluster for parallel processing with one fewer than number of
  # available cores
  # Load required libraries, objects, and functions to each core
  cluster <- new_cluster(n = cl) %>% 
    cluster_library(c('sf','raster','stringi','purrr','dplyr','tidyr','units')) %>% 
    cluster_assign(fetch = fetch,
                   site_layer = site_layer,
                   polygon_layer = polygon_layer,
                   max_dist = max_dist,
                   n_directions = n_directions,
                   site_var = site_var)
  
  # Calculate fetch for all survey sites in 11.25 degree increments
  my_fetch_proj <- site_layer %>%
    bind_cols(., cl_group = rep(1:cl, length.out = nrow(.))) %>% # grouping variable to split obs over cores
    group_by(cl_group) %>% # group by grouping variable
    partition(cluster = cluster) %>% # partition obs. to clusters (create party_df)
    do(fetch(polygon_layer = polygon_layer, # calculate fetch in parallel
             site_layer = .,
             max_dist = max_dist,
             n_directions = n_directions,
             site_names = pull(., var = site_var))) %>% 
    collect() %>% # combine observations from all clusters
    st_as_sf(sf_column_name = 'geometry') %>% 
    ungroup()
  
  return(my_fetch_proj)
  
}

