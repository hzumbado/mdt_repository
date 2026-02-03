# Mojave desert tortoise distribution
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
  rast('rasters/present/envs_predictions.tif')

# model -------------------------------------------------------------------

best_model <- 
  read_rds(
    paste0(
      models, 
      'tortoise_sdm_results.rds')) %>% 
  pluck('best_model')

# continuous predictions --------------------------------------------------

range_map <- 
  terra::predict(
    best_model, 
    envs, 
    args = c('outputformat=logistic')) 

names(range_map) <- 'Suitability'

range_map %>% 
  writeRaster(
    paste0(
      preds, 
      'mdt_predictions.tif'), 
    overwrite = TRUE)

# map ---------------------------------------------------------------------

plot(range_map)

range_map %>% 
  clamp(lower = 0.4) %>% 
  plot()
