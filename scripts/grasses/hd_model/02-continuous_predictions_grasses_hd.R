# Mojave desert tortoise distribution
# Script 02
# SDM continuous predictions. Invasive grasses

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(rJava)
library(predicts)
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
#sp <- 'Schismus_arabicus'
sp <- 'Schismus_barbatus'

best_model <- 
  read_rds(
    paste0(
      models,
      sp,
      '_hd_model_results.rds')) %>% 
  pluck('best_model')

# raster predictions ------------------------------------------------------

pred_fun <-
  methods::selectMethod(
    "predict",
    signature = "MaxEnt_model")

range_map <-
  pred_fun(
    best_model,
    envs[[sp]],
    args = "outputformat=cloglog")

names(range_map) <- 'Suitability'

plot(range_map)

global(
  range_map,
  c("min", "max"),
  na.rm = TRUE)

# save raster -------------------------------------------------------------

range_map %>% 
  writeRaster(
    paste0(
      predictions,
      sp, 
      '_predictions_hd_model.tif'), 
    overwrite = TRUE)
