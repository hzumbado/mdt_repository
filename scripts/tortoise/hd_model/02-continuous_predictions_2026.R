# Mojave desert tortoise distribution
# Modeling
# Script 02
# SDM continuous predictions Mojave desert tortoise

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(sf)
library(tidyterra)
library(tidyverse)

# paths -------------------------------------------------------------------

models <- 'output/models/tortoise/files/'
preds <- 'output/models/tortoise/predictions/'

#  shapefiles -------------------------------------------------------------

rear_edge <- 
  read_sf('shapefiles/rear_edge.gpkg')

# raster ------------------------------------------------------------------

envs <- 
  rast('rasters/present/envs_predictions_hd_model.tif')

# model -------------------------------------------------------------------

best_model <- 
  read_rds(
    paste0(
      models, 
      'hd_model_results.rds')) %>% 
  pluck('best_model')

# continuous predictions --------------------------------------------------

range_map <-
  methods::selectMethod(
    'predict',
    signature = 'MaxEnt_model')(
      best_model,
      envs,
      args = 'outputformat=logistic')

names(range_map) <- 'Suitability'

range_map %>% 
  writeRaster(
    paste0(
      preds, 
      'mdt_predictions_hd_model.tif'), 
    overwrite = TRUE)

# map ---------------------------------------------------------------------

data <-
  read_rds('data/tortoise_model_data.rds')

occs <- 
  data %>% 
  filter(presence == 1) %>% 
  select(x, y) %>% 
  st_as_sf(
    coords = c('x', 'y'),
    crs = 4326)

plot(range_map)
points(occs, pch = 16, cex = 0.4, col = 'red')

terra::extract(range_map, occs) %>%
  summarise(mean = mean(Suitability),
            median = median(Suitability))


range_map %>% 
  clamp(lower = 0.36) %>% 
  plot()
