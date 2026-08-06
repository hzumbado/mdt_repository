# Mojave desert tortoise distribution
# Modeling
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
      'mdt_predictions_hd_model.tif')) 

# best model results ------------------------------------------------------

bm_results <-
  read_rds(
    paste0(
      models, 
      'tortoise_sdm_results.rds')) %>% 
  pluck('best_model_results') %>% 
  rename_all(., .funs = tolower)

# x10 ---------------------------------------------------------------------

x10 <-
  filter(
    bm_results,
    parameter == 'X10.percentile.training.presence.Cloglog.threshold') %>%
  select(value) %>%
  pull()

#x10 <- 0.40 # to make map similar to sdm with original data

# mtss --------------------------------------------------------------------

mtss <-
  filter(
    bm_results,
    parameter == 'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold') %>%
  select(value) %>%
  pull()

#mtss <- 0.45 # to make map similar to sdm with original data

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

c(suitability_1, suitability_2) %>% plot()

c(suitability_1,suitability_2) %>% 
  writeRaster(
    paste0(
      predictions,
      'mdt_summarized_predicctions_hd_model.tif'), 
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
    'mdt_binary_predicctions_hd_model.rds'))

# areas -------------------------------------------------------------------

a <-  st_area(bin_x10)
b <-  st_area(bin_mtss) 

units::set_units(c(a,b), 'km^2')

# reclassification matrix ------------------------------------------------

suitable_values <- 
  terra::values(range_map, na.rm = TRUE)

suitable_values <- 
  suitable_values[
  suitable_values >= x10]

p90 <- 
  unname(
    stats::quantile(
    suitable_values,
    probs = 0.90,
    na.rm = TRUE))

p90 # 0.643637


rclas <- 
  matrix(
    c(
    -Inf, x10, 1,   # Unsuitable
    x10, mtss, 2,   # Low suitability
    mtss, p90, 3,   # Moderate suitability
    p90, Inf, 4),     # High suitability
  ncol = 3,
  byrow = TRUE)

suitability_classes <- 
  terra::classify(
  range_map,
  rclas,
  right = FALSE)

plot(suitability_classes)

# rclas1 <-
#   matrix(
#     c(
#       -Inf, 0, NA, # Missing data
#       0, x10, 1, # Unsuitable
#       x10, 0.55, 2, # Low suitability
#       0.55, 0.62, 3, # Moderate suitability
#       0.62, 1, 4), # High suitability
#     ncol = 3,
#     byrow = T)
# 
# rclas2 <-
#   matrix(
#     c(
#       -Inf, 0, NA, # Missing data
#       0, mtss, 1, # Unsuitable
#       mtss, 0.58, 2, # Low suitability
#       0.58, 0.63, 3, # Moderate suitability
#       0.63, 1, 4), # High suitability
#     ncol = 3,
#     byrow = T)

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

suitability_map <- 
  rast_classify(rear_edge, range_map, rclas)

plot(suitability_map)

# save stack --------------------------------------------------------------

suitability_map %>% 
  writeRaster(
    paste0(
      predictions,
      'mdt_thresholds_hd_model.tif'),
    overwrite = TRUE)

# area calculations -------------------------------------------------------

# área de cada celda en km2

cell_area <- 
  terra::cellSize(
  suitability_map,
  unit = "km")

# área por clase

area_by_class <- 
  terra::zonal(
  cell_area,
  suitability_classes,
  fun = "sum",
  na.rm = TRUE)

area_by_class

area_table <- 
  area_by_class |>
  dplyr::mutate(
    class = dplyr::case_when(
      Suitability == 1 ~ "Unsuitable",
      Suitability == 2 ~ "Low",
      Suitability == 3 ~ "Moderate",
      Suitability == 4 ~ "High")) %>% 
  dplyr::select(
    class,
    area_km2 = area)

area_table


area_10ptp <- 
  area_table %>% 
  dplyr::filter(class %in% c("Low", "Moderate", "High")) |>
  dplyr::summarise(area_km2 = sum(area_km2)) |>
  dplyr::pull(area_km2)

area_mtss <- area_table |>
  dplyr::filter(class %in% c("Moderate", "High")) |>
  dplyr::summarise(area_km2 = sum(area_km2)) |>
  dplyr::pull(area_km2)

area_10ptp
area_mtss


total_area <- 
  sum(area_table$area_km2)

area_table <- 
  area_table |>
  dplyr::mutate(
    percent = 100 * area_km2 / total_area)

area_table
