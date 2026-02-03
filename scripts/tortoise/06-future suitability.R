# Mojave desert tortoise distribution
# Script 06
# SDM Future suitability Mojave desert tortoise

# Setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(units)
library(terra)
library(tmap)
library(gt)
library(tidyverse)

# paths -------------------------------------------------------------------

models <- 'output/models/tortoise/files/'
predictions <- 'rasters/future/'
predictions1 <- 'output/models/tortoise/predictions/'
future <- 'output/models/tortoise/predictions/future/'
my_species <- 'gopherus_agassizii'

# shapefiles --------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(cv|salton|edge)',
  full.names = TRUE) %>%
  map(~ (read_sf(.x)) %>% 
        st_make_valid() %>% 
        st_transform(crs = 4326)) %>% 
  set_names(
    'cv',
    'rear_edge',
    'salton') %>% 
  list2env(.GlobalEnv)

# mtss layer --------------------------------------------------------------

mtss <- 
  read_rds(
    paste0(
      models, 
      'tortoise_sdm_results.rds')) %>% 
  pluck('best_model_results') %>% 
  rename_all(., .funs = tolower) %>% 
  filter(
    parameter == 'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold') %>% 
  select(value) %>% 
  pull() 

mtss_bin <- 
  read_rds(
    paste0(
      predictions1,
      'mdt_binary_predicctions.rds')) %>% 
  pluck('mtss_bin_mdt')

# rasters -----------------------------------------------------------------

#present layer

range_map <- 
  rast(
    paste0(
      predictions1, 
      'mdt_predictions.tif'))

layer_names <- 
  list.files(
    future,
    pattern = '.tif',
    full.names = FALSE) %>%  
  str_remove('.tif') 

hadgem_future <- 
  list.files(
    predictions,
    pattern = '.tif',
    full.names = TRUE) %>% 
  map(~ rast(.x) %>% 
        clamp(lower = mtss)) %>% 
  set_names(layer_names)

# areas -------------------------------------------------------------------

hadgem_future <- 
  list.files(
    future,
    pattern = '.tif',
    full.names = TRUE) %>% 
  map(~ (rast(.x)) %>% 
        crop(mtss_bin, mask = TRUE)) %>% 
  set_names(layer_names) %>% 
  map(~(.x >= mtss) %>% 
        as.polygons() %>%  
        st_as_sf() %>%
        filter(lyr1 == 1))


st_area(mtss_bin) %>% 
  set_units('km2')
 
area <- 
  hadgem_future %>% 
  map(~.x %>%
        st_area()/1000000) 

period <-  
  rep(c(
    '2041-2060', 
    '2061-2080', 
    '2081-2100'),3)

ssp <-  
  rep(
    c('SSP126', 'SSP245', 'SSP585'), 
    c(3, 3, 3))

table_sdm <-
  tibble(period, ssp)

area_final <-
  area %>% 
  unlist() %>% 
  tibble() %>% 
  rename(area = '.') %>% 
  bind_cols(table_sdm, .) %>% 
  mutate(
    change = (area*100/(st_area(mtss_bin) %>% 
                          set_units('km2'))) %>% 
      as.numeric(),
    change = change - 100) %>%
  mutate(
    across(
      area:change, 
      ~.x %>% 
        round(0))) %>% 
  mutate(
    change = 
      paste0('(', change, ')')) %>% 
  unite('ESH', area:change, sep = ' ') %>% 
  pivot_wider(
    names_from = ssp, 
    values_from = ESH) %>% 
  rename(Period = period)

area_final %>% 
  write_csv('output/tables/area_final.csv')

area_final %>% 
  gt() %>% 
  gtsave(
    paste0(
      'output/tables/',
      my_species,
      '_area.docx'))
