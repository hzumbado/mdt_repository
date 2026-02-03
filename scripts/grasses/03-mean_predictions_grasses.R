# Mojave desert tortoise distribution
# Script 03
# SDM mean predictions. Invasive grasses

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(tidyterra)
library(sf)
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

# occurrences -------------------------------------------------------------

occs <-
  read_rds('data/grasses_model_data.rds') %>%
  filter(presence == 1) %>% 
  select(species, x, y) %>% 
  st_as_sf(
    coords = c(
      x = 'x',
      y = 'y'),
    crs = 4326) %>% 
  st_filter(rear_edge) %>% 
  vect()

# predictions -------------------------------------------------------------

grasses <- 
  list.files(
    predictions, 
    full.names = TRUE, 
    pattern = '^(Br|Sc)') %>% 
  map(
    ~ rast(.x)) %>% 
  set_names(my_species) %>% 
  rast()

range_map <- mean(grasses)

names(range_map) <- 'Mean Suitability'

# save raster -------------------------------------------------------------

range_map %>%
  writeRaster(
    paste0(
      predictions,
      'grasses_mean_predictions.tif'), 
    overwrite = TRUE)

# plot raster -------------------------------------------------------------

rm <- 
  range_map %>%
  as.polygons()

plot(range_map)

plot(rm, add = T)

points(
  occs, 
  pch = 19,
  cex = 0.75,
  border = 'black',
  col = c('red', 'green', 'blue'))
