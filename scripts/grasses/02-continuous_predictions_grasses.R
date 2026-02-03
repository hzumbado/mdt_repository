# Mojave desert tortoise distribution
# Script 02
# SDM continuous predictions. Invasive grasses

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(sf)
library(tidyterra)
library(tidyverse)

# paths -------------------------------------------------------------------

models <- 'output/models/grasses/files/'
predictions <- 'output/models/grasses/predictions/'

# Species data ------------------------------------------------------------

my_species <- 
  c(
    'Bromus_rubens' , 
    'Schismus_arabicus',
    'Schismus_barbatus')

#  shapefiles -------------------------------------------------------------

rear_edge <- 
  read_sf('shapefiles/rear_edge.gpkg')

# raster ------------------------------------------------------------------

envs <-
  list.files(
    'rasters/present',
    pattern = '(Br|Sc)', 
    full.names = T) %>% 
  map(~ .x %>% 
        rast() %>% 
        crop(rear_edge, mask = T)) %>% 
  set_names(my_species) 

# selecting best model ----------------------------------------------------

#sp <- 'Bromus_rubens'
sp <- 'Schismus_arabicus'
#sp <- 'Schismus_barbatus'

best_model <- 
  read_rds(
    paste0(
      models,
      sp,
      '_model_results.rds')) %>% 
  pluck('best_model')

# raster predictions ------------------------------------------------------

range_map <- 
  terra::predict(
    best_model, 
    envs[[sp]], 
    args = c('outputformat=logistic')) 

names(range_map) <- 'Suitability'

plot(range_map)

# save raster -------------------------------------------------------------

range_map %>% 
  writeRaster(
    paste0(
      predictions,
      sp, 
      '_predictions.tif'), 
    overwrite = TRUE)