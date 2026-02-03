# obscure coordinates

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(tidyverse)

my_species <- "Gopherus agassizii"

# shapefiles --------------------------------------------------------------

range <- 
  read_sf('shapefiles/rear_edge.gpkg') %>%
  st_make_valid() %>%
  janitor::clean_names()

list.files(
  'shapefiles', 
  pattern = '(salton|cal).*\\.gpkg$',
  full.names = TRUE) %>%
  map(~ .x %>% 
        read_sf() %>%
        st_transform(crs = st_crs(range)) %>% 
        st_make_valid() %>%
        janitor::clean_names()) %>%
  set_names('calibration_area', 'salton') %>% 
  list2env(.GlobalEnv)

set.seed(123)

dataset <-  
  read_rds("data/data_model.rds") %>% 
  filter(presence == 1) %>% 
  slice_sample(n = 200) %>% 
  bind_rows(
    read_rds("data/data_model.rds") %>% 
      filter(presence == 0))

dataset <-
  dataset %>%
  st_as_sf(
    coords = c('x', 'y'),
    crs = st_crs(salton),
    remove = FALSE) %>% 
  st_filter(calibration_area) %>% 
  st_filter(
    salton, 
    .predicate = st_disjoint) %>% 
  st_drop_geometry()

dataset %>% 
  summarize(
    n = n(),
    .by = presence)#451,bg 12791

dataset %>% 
  write_rds(
    'data/tortoise_model_data.rds')
