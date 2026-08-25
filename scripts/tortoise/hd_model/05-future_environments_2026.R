# Mojave desert tortoise distribution
# Modeling
# Script 05
# SDM Future projections Mojave desert tortoise

# Setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(terra)
library(tmap)
library(tidyterra)
library(tidyverse)

# paths -------------------------------------------------------------------

# external data path -------------------------------------------------------

# Set this path to the local directory containing the WorldClim
# future climate rasters. This directory is outside the project
# and will differ among computers.

future <-
  'YOUR_LOCAL_PATH/worldclim/future_1km'

# future <-
#   'C:/Users/zumba/Documents/rasters/worldclim/future_1km'

# R project paths

hadgem <- 'rasters/future/'
models <- 'output/models/tortoise/files/'
predictions1 <- 'output/models/tortoise/predictions/'
predictions <- 'rasters/future_preds/'

# data --------------------------------------------------------------------

rear_edge <- 
  read_sf('shapefiles/rear_edge.gpkg') 

salton <- 
  read_sf('shapefiles/salton_sea.gpkg')

# climatic data -----------------------------------------------------------

range_map <- 
  rast(
    paste0(
      predictions1, 
      'mdt_predictions_hd_model.tif'))

bm_results <-
  read_rds(
    paste0(
      models, 
      'hd_model_results.rds')) %>% 
  pluck('best_model_results') %>% 
  rename_all(., .funs = tolower)


mtss <-
  read_rds(
    paste0(
      models, 
      'hd_model_results.rds')) %>% 
  pluck('best_model_results') %>% 
  rename_all(., .funs = tolower) %>% 
  filter(
    parameter == 'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold') %>%
  select(value) %>%
  pull() 

current_mtss <- 
  ifel(range_map >= mtss, 1, NA)

envs <-
  rast('rasters/present/envs_predictions_hd_model.tif')

names(envs)

envs_fixed <- 
  envs %>% 
  select(
    !c(bio_12))

bio <- 'bio_12'

layer_names <- 
  list.files(
    future,
    pattern = '.tif',
    full.names = FALSE) %>% 
  str_remove('.tif') %>% 
  str_replace_all('-', '_')

future_envs <-
  list.files(
    future,
    pattern = '.tif',
    full.names = TRUE) %>%
  map(~ .x %>% 
        rast()%>% 
        select(12) %>%  
        crop(rear_edge, mask = TRUE) %>% 
        c(envs_fixed)) %>% 
  set_names(layer_names) %>%  
  list2env(.GlobalEnv)

# hadgem ------------------------------------------------------------------

names(HadGEM3_ssp126_2041_2060)[1] <- bio 
names(HadGEM3_ssp126_2061_2080)[1] <- bio 
names(HadGEM3_ssp126_2081_2100)[1] <- bio 
names(HadGEM3_ssp245_2041_2060)[1] <- bio 
names(HadGEM3_ssp245_2061_2080)[1] <- bio 
names(HadGEM3_ssp245_2081_2100)[1] <- bio 
names(HadGEM3_ssp585_2041_2060)[1] <- bio 
names(HadGEM3_ssp585_2061_2080)[1] <- bio 
names(HadGEM3_ssp585_2081_2100)[1] <- bio 

hadgem_cropped <- 
  list(
    HadGEM3_ssp126_2041_2060, 
    HadGEM3_ssp126_2061_2080, 
    HadGEM3_ssp126_2081_2100, 
    HadGEM3_ssp245_2041_2060, 
    HadGEM3_ssp245_2061_2080, 
    HadGEM3_ssp245_2081_2100, 
    HadGEM3_ssp585_2041_2060, 
    HadGEM3_ssp585_2061_2080, 
    HadGEM3_ssp585_2081_2100) %>% 
  set_names(layer_names) 

# save cropped rasters ----------------------------------------------------

hadgem_cropped %>%
  names(.) %>%
  walk(~ writeRaster(
    hadgem_cropped[[.]], 
    paste0(hadgem, ., '.tif'),
    overwrite = TRUE))

tm_shape(rear_edge) + 
  tm_borders() +
  tm_shape(HadGEM3_ssp585_2081_2100$bio_12) +
  tm_raster(
    col.scale = tm_scale_continuous(values = terrain.colors(500))) +
  tm_shape(salton) +
  tm_polygons('cyan3')

# future predictions ------------------------------------------------------

best_model <-
  read_rds(
    paste0(
      models, 
      'hd_model_results.rds')) %>% 
  pluck('best_model') 

vars <- names(envs)

hadgem_future <- 
  list.files(
    'rasters/future',
    pattern = '.tif',
    full.names = TRUE) %>% 
  map(~ rast(.x) %>%
        tidyterra::select(all_of(vars)) %>%
        terra::predict(
          best_model, 
          args = c('outputformat=logistic'), 
          na.rm = T) %>% 
  terra::mask(
    current_mtss,
    maskvalues = 0)) %>% 
  set_names(layer_names)
  set_names(layer_names) 

# save rasters ------------------------------------------------------------

hadgem_future %>%
  names(.) %>%
  walk(~ writeRaster(
    hadgem_future[[.]], 
    paste0(
      predictions,
      ., 
      '.tif'), 
    overwrite = TRUE))
