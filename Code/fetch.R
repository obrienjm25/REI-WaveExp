#### Function to calculate wind fetch ####
#' 
#' Code is adapted from https://github.com/cran/fetchR for use with sf spatial objects
#' and to improve execution speed
#' 
#' Fetch, the unobstructed distance over which wind-driven waves can build, is a 
#' popular proxy for wave exposure at a given location commonly used for site-specific 
#' evaluations. This fetch function calculates the fetch in a user-specified number of directions
#' around given locations within a waterbody out to a maximum distance set by the user.
#' 
#' The function requires an sf object (with polygon geometry type)
#' polygon_layer, that represents the coastline and an sf object (point geometry)
#' that represents the user-defined sites (site layer) for which fetch calculations 
#' are made.
#'
#' The number of directions for which the fetch calculations are made for each site is
#' determined by the number of directions per quadrant (n_directions). 
#' The default value (9) calculates 9 fetch vectors 90-degree quadrant, 
#' (i.e., one fetch vector every 10 degrees). The first fetch vector is 
#' always calculated for the northerly direction (0/360 degrees).
#' 
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
#' site_names = character vector of the site names. If missing, the site 
#'                names are taken from a column of the data associated with 
#'                site_layer matching the regular expression 
#'                {^[Nn]ames{0,1}}. If there is no such column, then 
#'                default names are created ('Site 1', 'Site 2', ...).
#' quiet = logical. Suppress diagnostic messages? (Default {FALSE}).
#' 
#' Returns an object of class sf
#' 
#' Note: At least one of the inputs to the polygon_layer or 
#'       site_layer arguments must be projected. If one of the inputs are 
#'       not projected, then it will be transformed to have the same projection 
#'       as the other. If both are projected, but do not have identical 
#'       coordinate reference systems (CRS) then site_layer will be 
#'       transformed to the same CRS as polygon_layer.

library(sf)
library(raster)
library(stringi)
library(purrr)
library(dplyr)
library(tidyr)
library(units)

fetch = function(polygon_layer, site_layer, max_dist = 300, n_directions = 8,
                 site_names, quiet = FALSE){
  
  if (!is(polygon_layer, "sf") && !is(st_geometry(polygon_layer), c('sfc_POLYGON', 'sfc_MULTIPOLYGON')))
    stop(paste("polygon_layer must be a sf object with a POLYGON or MULTIPOLYGON geometry type.\nSee",
               "'?sf' for details on how to create a",
               "sf object."), call. = FALSE)
  
  if (!is(site_layer, "sf") && !is(st_geometry(site_layer), c('sfc_POINT', 'sfc_MULTIPOINT')))
    stop(paste("polygon_layer must be a sf object with a POINT or MULTIPOINT geometry type.\nSee",
               "'?sf' for details on how to create a",
               "sf object."), call. = FALSE)
  
  if(is(st_geometry(polygon_layer), 'sfc_MULTIPOLYGON'))
     st_cast(polygon_layer, 'POLYGON')
  
  if (!is.numeric(max_dist) || length(max_dist) != 1)
    stop("max_dist must be a single number.", call. = FALSE)
  
  if (!is.numeric(n_directions) || length(n_directions) != 1)
    stop("n_directions must be a single integer.", call. = FALSE)
  n_directions = round(n_directions)
  
  if (n_directions < 1 || n_directions > 90)
    stop("n_directions must be between 1 and 90.", call. = FALSE)
  
  if (!missing(site_names)){
    site_names = as.character(site_names)
    
    if (length(site_names) != nrow(site_layer)){
      warning(paste("lengths differ for the number of sites and site names;", 
                    "using default names instead."), call. = FALSE)
      site_names = paste("Site", seq_along(st_geometry(site_layer)))
    }
  } else {
    if (any(grepl("^[Nn]ames{0,1}$", names(site_layer)))){
      name_col = grep("^[Nn]ames{0,1}$", names(site_layer))[[1]]
      site_names = as.character(pull(site_layer, name_col))
    } else {
      site_names = paste("Site", seq_along(st_geometry(site_layer)))
    }
  }
  
  quiet = as.logical(quiet[1])
  
  ## Check if the polygon and points layers are projected, and ensure they have 
  ## the same CRS.
  which_proj = list(polygon_layer, site_layer) %>% 
    map(., st_crs) %>% 
    map_lgl(., ~grepl('PROJCRS',.$wkt[1]))
  
  if (all(!which_proj))
    stop("polygon_layer and/or site_layer must be projected to calculate fetch", 
         call. = FALSE)
  
  if (all(which_proj) && !identical(st_crs(polygon_layer), st_crs(site_layer))){
    warning("the CRS for polygon_layer and site_layer differ; transforming
              site_layer CRS to match")
    site_layer = st_transform(site_layer, st_crs(polygon_layer))
  }
  
  if (!which_proj[1]){
    if (!quiet)
      message("projecting polygon_layer onto the site_layer CRS")
    polygon_layer = st_transform(polygon_layer, st_crs(site_layer))
  }
  
  if (!which_proj[2]){
    if (!quiet)
      message("projecting site_layer onto the polygon_layer CRS")
    site_layer = st_transform(site_layer, st_crs(polygon_layer))
  }
  
  if (!quiet)
    message("checking site locations are not on land")
  
  # Should remove these sites with warning instead of returning error
  if (any(lengths(st_intersects(site_layer, polygon_layer)) > 0))
    stop("at least one site location is on land")
  
  # Convert max_dist to appropriate units.
  # First of all convert max_dist to metres (default)
  max_dist = max_dist * 1000
  
  # Double check if metres are the correct units
  # If not, warn the user that the supplied max_dist should be scaled 
  # appropriately
  if (st_crs(polygon_layer)$units != "m")
    warning("the PROJ.4 unit is not metres; ensure max_dist has been scaled 
            appropriately")
  
  directions = head(seq(0, 360, by = 360 / (n_directions * 4)), -1)
  
  # Rearrange sequence order to start at 90 degrees to match up with the output
  # from st_buffer
  directions = unlist(split(directions, directions < 90), use.names = FALSE)
  
  # Create nested data that calculates and stores fetch vectors and lengths for
  # each site
  
  message("calculating fetch for ", nrow(site_layer), " sites")
  
  fetch_sf <- site_layer %>%
    bind_cols(., tibble(site_names)) %>%
           # Create polygon (approximating a circle) with a given radius. These 
           # vertices are used for creating the end points for the fetch vectors.
    mutate(d_bff = st_buffer(geometry, dist = max_dist, nQuadSegs = n_directions),
           # Calculate end points at the maximum distances.       
           fetch_ends = map(d_bff, ~head(st_coordinates(.)[,1:2], -1)),
           fetch_ends = map(fetch_ends, ~.[order(directions),]),
           # Create matrix repeating site coordinates (nrow = number of fetch directions)
           coords = map(geometry, st_coordinates),
           coords = map(coords, ~.[rep(1:nrow(.), times = n_directions * 4),]),
           # bind site coordinates with fetch ends
           all_mat = map2(coords, fetch_ends, rbind),
           # split matrix into list
           all_list = map(all_mat, ~split(., sort(directions))),
           # Create a list of 2x2 matrices indicating the start and end coordinates
           line_list = map(all_list, ~map(., matrix, ncol = 2)),
           # create sf line objects for all fetch directions
           # Lines radiate around site to max distance
           fetch_st_lines = map(line_list, ~st_sf(geom = st_sfc(st_multilinestring(.), 
                                                   crs = st_crs(polygon_layer)))),
           # determine indices of subset of polygons intersecting 
           # with full length fetch lines
           poly_sub = map(fetch_st_lines, ~st_intersects(., polygon_layer)),
           # cast from multiline to line (one row per direction)
           fetch_st_lines = map(fetch_st_lines, st_cast, 'LINESTRING'),
           # sort rows in sf line dataframes by fetch direction
           fetch_st_lines = map(fetch_st_lines, ~ mutate(., direction = sort(directions))),
           # Erase polygon layer from fetch lines
           fetch_st_lines = map2(fetch_st_lines, poly_sub,
                                ~st_difference(.x, st_union(st_combine(polygon_layer[unique(unlist(.y)),])))),
           # Extract first line segment from each fetch direction (this is fetch line shortened by first polygon intersection)
           fetch_st_lines = map(fetch_st_lines, ~st_cast(., 'LINESTRING')[as.character(seq(1:(n_directions * 4))),]),
           # Calculate fetch line lengths
           fetch_length = map(fetch_st_lines, st_length)) %>%
    dplyr::select(., site_names, fetch_st_lines, fetch_length) %>%
    unnest(., cols = c(fetch_length_m, fetch_st_lines)) %>% 
    dplyr::select(-geom)

}
