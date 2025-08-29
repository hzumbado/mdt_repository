# Setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(terra)
library(tmap)
library(tidyterra)
library(tidyverse)

# paths -------------------------------------------------------------------

future <- 'C:/Users/zumba/Documents/rasters/worldclim/future_1km'
hadgem <- 'rasters/hadgem3/'
models <- 'output/models/tortoise/files/'
predictions1 <- 'output/models/tortoise/predictions/'
predictions2 <- 'output/models/tortoise/predictions/future/'

# data --------------------------------------------------------------------

rear_edge <- 
  read_sf('shapefiles/processed/range_area_2.gpkg') 

salton <- 
  read_sf('shapefiles/geography/salton_sea.gpkg')

# climatic data -----------------------------------------------------------

range_map <- 
  rast(
    paste0(
      predictions1, 
      'mdt_predictions.tif'))

mtss <- 
  read_rds(
    paste0(
      models, 
      'model_results.rds')) %>% 
  pluck('best_model_results') %>% 
  rename_all(., .funs = tolower) %>% 
  filter(
    parameter == "Maximum.training.sensitivity.plus.specificity.Cloglog.threshold") %>% 
  select(value) %>% 
  pull() 

range_map_pol <-
  (range_map >= mtss) |> 
  as.polygons() |>  
  st_as_sf() %>%
  filter(Suitability == 1) %>% 
  select(!Suitability)

envs <-
  rast('rasters/envs_predictions.tif')

names(envs)

envs_topography <- 
  envs %>% 
  select(!c(
    wc2.1_30s_bio_02, 
    wc2.1_30s_bio_15,
    wc2.1_30s_bio_18))

bio <- 
  c('wc2.1_30s_bio_02', 'wc2.1_30s_bio_15', 'wc2.1_30s_bio_18')

layer_names <- 
  list.files(
    future,
    pattern = '.tif',
    full.names = FALSE) %>% 
  str_remove(".tif") %>% 
  str_replace_all('-', '_')

future_envs <-
  list.files(
    future,
    pattern = '.tif',
    full.names = TRUE) %>%
  map(~ .x %>% 
        rast()%>% 
        select(2, 15, 18) %>%  
        crop(rear_edge, mask = TRUE) %>% 
        c(envs_topography)) %>% 
  set_names(layer_names) %>%  
  list2env(.GlobalEnv)

# hadgem ------------------------------------------------------------------

names(HadGEM3_ssp126_2041_2060)[1:3] <- bio 
names(HadGEM3_ssp126_2061_2080)[1:3] <- bio 
names(HadGEM3_ssp126_2081_2100)[1:3] <- bio 
names(HadGEM3_ssp245_2041_2060)[1:3] <- bio 
names(HadGEM3_ssp245_2061_2080)[1:3] <- bio 
names(HadGEM3_ssp245_2081_2100)[1:3] <- bio 
names(HadGEM3_ssp585_2041_2060)[1:3] <- bio 
names(HadGEM3_ssp585_2061_2080)[1:3] <- bio 
names(HadGEM3_ssp585_2081_2100)[1:3] <- bio 

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
    paste0(hadgem, ., ".tif"),
    overwrite = TRUE))

tm_shape(rear_edge) + 
  tm_borders() +
  tm_shape(HadGEM3_ssp585_2081_2100$wc2.1_30s_bio_15) +
  tm_raster(
    palette = terrain.colors(500),
    style = 'cont') 

# future predictions ------------------------------------------------------

best_model <-
  read_rds(
    paste0(
      models, 
      'model_results.rds')) %>% 
  pluck('best_model') 

vars <- names(envs)

hadgem_future <- 
  list.files(
    'rasters/hadgem3',
    pattern = '.tif',
    full.names = TRUE) %>% 
  map(~ rast(.x) %>%
        crop(range_map_pol, mask = TRUE) %>% 
        tidyterra::select(all_of(vars)) %>%
        terra::predict(
          best_model, 
          args = c("outputformat=logistic"), 
          na.rm = T)) %>% 
  set_names(layer_names) 

# save rasters ------------------------------------------------------------

hadgem_future %>%
  names(.) %>%
  walk(~ writeRaster(
    hadgem_future[[.]], 
    paste0(
      predictions,
      ., 
      ".tif"), 
    overwrite = TRUE))
