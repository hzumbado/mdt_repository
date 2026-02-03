# Mojave desert tortoise distribution
# Script 03
# Binary predictions and estimation of ESH

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(terra)
library(tidyverse)

# folders -----------------------------------------------------------------

predictions <- 'output/models/tortoise/predictions/'
models <- 'output/models/tortoise/files/'

# Species data ------------------------------------------------------------

my_species <- 'Gopherus agassizii'

# shapefiles --------------------------------------------------------------

rear_edge <- 
  read_sf('shapefiles/rear_edge.gpkg') %>%
  st_make_valid()

# raster ------------------------------------------------------------------

range_map <- 
  rast(
    paste0(
      predictions, 
      'mdt_predictions.tif')) 

# best model results ------------------------------------------------------

bm_results <-
  read_rds(
    paste0(
      models, 
      'tortoise_sdm_results.rds')) %>% 
  pluck('best_model_results') %>% 
  rename_all(., .funs = tolower)

# x10 ---------------------------------------------------------------------

# x10 <- 
#   filter(
#     bm_results, 
#     parameter == 'X10.percentile.training.presence.Cloglog.threshold') %>% 
#   select(value) %>% 
#   pull()

x10 <- 0.40 # to make map similar to sdm with original data

# mtss --------------------------------------------------------------------

# mtss <- 
#   filter(
#     bm_results, 
#     parameter == 'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold') %>% 
#   select(value) %>% 
#   pull()

mtss <- 0.45 # to make map similar to sdm with original data

# suitability maps --------------------------------------------------------

suitability_1 <-
  range_map %>% 
  clamp(
    lower =  x10,
    values = T) # values = F

names(suitability_1) = 'x10'

suitability_2 <-
  range_map %>% 
  clamp(
    lower =  mtss,
    values = T) # values = F 

names(suitability_2) = 'mtss'

# save suitability maps ---------------------------------------------------

plot(suitability_1)
plot(suitability_2)

c(suitability_1,suitability_2) %>% 
  writeRaster(
    paste0(
      predictions,
      'mdt_summarized_predicctions.tif'), 
    overwrite = TRUE)

# binary layer ------------------------------------------------------------

#x10

bin_lim <- 
  range_map >= x10

bin_x10 <-
  as.polygons(bin_lim) %>% 
  st_as_sf() %>%
  filter(Suitability == 1) 

plot(vect(bin_x10))

#mtss

bin_lim2 <- 
  range_map >= mtss

bin_mtss <-
  as.polygons(bin_lim2) %>% 
  st_as_sf() %>%
  filter(Suitability == 1) 

plot(vect(bin_mtss))

list(
  'x10_bin_mdt' = bin_x10,
  'mtss_bin_mdt' = bin_mtss) %>% 
  write_rds(
    paste0(
    predictions,
    'mdt_binary_predicctions.rds'))

# areas -------------------------------------------------------------------

a <-  st_area(bin_x10)
b <-  st_area(bin_mtss) 

units::set_units(c(a,b), 'km^2')

# reclassification matrix ------------------------------------------------

rclas1 <-
  matrix(
    c(
      -Inf, 0, NA, # Missing data
      0, x10, 1, # Unsuitable
      x10, 0.5, 2, # Low suitability
      0.5, 0.7, 3, # Moderate suitability
      0.7, 1, 4), # High suitability
    ncol = 3,
    byrow = T)

rclas2 <-
  matrix(
    c(
      -Inf, 0, NA, # Missing data
      0, mtss, 1, # Unsuitable
      mtss, 0.55, 2, # Low suitability
      0.55, 0.7, 3, # Moderate suitability
      0.7, 1, 4), # High suitability
    ncol = 3,
    byrow = T)

# function reclassify -----------------------------------------------------

rast_classify <-
  function(shape, raster, matrix){
    
    suitability_reclassified <- 
      classify(
        raster, 
        matrix)
    
    boundary_rast <-
      rasterize(
        vect(shape),
        suitability_reclassified)
    
    suitability_masked <-
      mask(
        suitability_reclassified,
        boundary_rast)
  }

# reclassified maps -------------------------------------------------------

suitability_map_1 <- 
  rast_classify(rear_edge, suitability_1, rclas1)

suitability_map_2 <- 
  rast_classify(rear_edge, suitability_2, rclas2)

plot(suitability_map_1)
plot(suitability_map_2)

# save stack --------------------------------------------------------------

c(suitability_map_1, suitability_map_2) %>% 
  writeRaster(
    paste0(
      predictions,
      'mdt_thresholds.tif'),
    overwrite = TRUE)

# area calculations -------------------------------------------------------

a <- 
  suitability_map_1 %>% 
  as.polygons() %>%
  st_as_sf() %>% 
  st_area() %>% 
  as_vector()

b <- 
  suitability_map_2 %>% 
  as.polygons() %>%
  st_as_sf() %>% 
  st_area() %>% 
  as_vector()

units::set_units(a, 'km^2')
units::set_units(b, 'km^2')

# as percentage

category <- 
  c('unsuitable', 'low', 'moderate', 'high')

areapol <-
  \(raster){
    
    p <-
      raster %>%
      as.polygons() %>%
      st_as_sf() %>% 
      st_area()
    
    p1 <- 
      units::set_units(p, 'km^2') %>% 
      as_vector()/sum(p/1000000)/10000
    
    p1 <- round(p1) %>% 
      set_names(category)
    
    return(p1)
  }

(x10_areas <- areapol(suitability_map_1))
(mtss_areas <- areapol(suitability_map_2))
