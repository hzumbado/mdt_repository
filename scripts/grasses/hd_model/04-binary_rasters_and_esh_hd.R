# Mojave desert tortoise distribution
# Script 04
# Binary predictions and estimation of ESH for grasses

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(terra)
library(tidyverse)

# folders -----------------------------------------------------------------

predictions <- 'output/models/grasses/predictions/'
models <- 'output/models/grasses/files/'

# Species data ------------------------------------------------------------

my_species <- 
  c(
    'Bromus_rubens', 
    'Schismus_arabicus',
    'Schismus_barbatus')

# shapefiles --------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(cv|rear).*\\.gpkg$',
  full.names = TRUE) %>% 
  map(~ .x %>% 
        read_sf() %>% 
        st_make_valid()) %>%
  set_names(
    'cv',
    'rear_edge') %>% 
  list2env(.GlobalEnv)

# rasters -----------------------------------------------------------------

range_maps <- 
  my_species %>% 
  map(
    ~ rast(
      paste0(
        predictions,
        .x,
        '_predictions_hd_model.tif')) %>% 
      crop(
        rear_edge,
        mask = TRUE)) %>% 
  set_names(my_species)

# check geometry ----------------------------------------------------------

compareGeom(
  range_maps[[1]],
  range_maps[[2]])

compareGeom(
  range_maps[[1]],
  range_maps[[3]])

# best model results ------------------------------------------------------

bm_results <-
  my_species %>% 
  map(
    ~ read_rds(
      paste0(
        models,
        .x,
        '_hd_model_results.rds')) %>% 
      pluck('best_model_results') %>%
      rename_all(.funs = tolower)) %>% 
  set_names(my_species)

# x10 ---------------------------------------------------------------------

x10 <-
  bm_results %>% 
  map(
    ~ .x %>% 
      filter(
        parameter == 
          'X10.percentile.training.presence.Cloglog.threshold') %>% 
      dplyr::select(value) %>% 
      pull()) %>% 
  as_vector() 

x10

# x10 threshold -----------------------------------------------------------

# br

x10_br <- 
  range_maps[[1]] >= x10[1]

# sa

x10_sa <- 
  range_maps[[2]] >= x10[2]

# sb

x10_sb <- 
  range_maps[[3]] >= x10[3]

# addition ----------------------------------------------------------------

x10_raster <- 
  sum(
    x10_br, 
    x10_sa, 
    x10_sb)

names(x10_raster) <- 'Species'

plot(x10_raster)

x10_raster %>%
  writeRaster(
    paste0(
      predictions, 
      'grasses_x10_hd.tif'), 
    overwrite = TRUE)

# mtss --------------------------------------------------------------------

mtss <- 
  bm_results %>% 
  map(
    ~ .x %>% 
      filter(
        parameter == 
          'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold') %>% 
      dplyr::select(value) %>% 
      pull()) %>% 
  as_vector() 

mtss

# mtss threshold ----------------------------------------------------------

# br

mtss_br <- 
  range_maps[[1]] >= mtss[1]

# sa

mtss_sa <- 
  range_maps[[2]] >= mtss[2]

# sb

mtss_sb <- 
  range_maps[[3]] >= mtss[3]

# addition ----------------------------------------------------------------

mtss_raster <- 
  sum(
    mtss_br, 
    mtss_sa, 
    mtss_sb)

names(mtss_raster) <- 'Species'

plot(mtss_raster)

mtss_raster %>%
  writeRaster(
    paste0(
      predictions, 
      'grasses_mtss_hd.tif'), 
    overwrite = TRUE)

# area function -----------------------------------------------------------

raster_area <-
  \(raster){
    
    cell_area <-
      cellSize(
        raster,
        unit = 'km')
    
    area <-
      zonal(
        cell_area,
        raster,
        fun = 'sum',
        na.rm = TRUE) %>% 
      as_tibble() %>% 
      rename(
        species = 1,
        area = 2) %>% 
      mutate(
        percentage = 
          area * 100 / sum(area))
    
    return(area)
  }

# rear-edge areas ---------------------------------------------------------

x10_areas <-
  raster_area(
    x10_raster)

mtss_areas <-
  raster_area(
    mtss_raster)

x10_areas

mtss_areas

# Coachella Valley --------------------------------------------------------

x10_cv <-
  x10_raster %>% 
  crop(
    cv,
    mask = TRUE)

mtss_cv <-
  mtss_raster %>% 
  crop(
    cv,
    mask = TRUE)

x10_areas_cv <-
  raster_area(
    x10_cv)

mtss_areas_cv <-
  raster_area(
    mtss_cv)

x10_areas_cv

mtss_areas_cv

# three-species co-occurrence ---------------------------------------------

x10_three <-
  x10_areas %>% 
  filter(species == 3)

mtss_three <-
  mtss_areas %>% 
  filter(species == 3)

x10_three_cv <-
  x10_areas_cv %>% 
  filter(species == 3)

mtss_three_cv <-
  mtss_areas_cv %>% 
  filter(species == 3)

x10_three

mtss_three

x10_three_cv

mtss_three_cv
