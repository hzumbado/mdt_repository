# Mojave desert tortoise distribution
# Script 04
# Binary predictions and estimation of ESH for grasses

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(terra)
library(tidyverse)

# folders -----------------------------------------------------------------

predictions <- 'output/models/grasses/predictions/'
models <- 'output/models/grasses/files/'

# Species data ------------------------------------------------------------

my_species <- 
  c(
    'Bromus_rubens' , 
    'Schismus_arabicus',
    'Schismus_barbatus')

# shapefiles --------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(cv|rear).*\\.gpkg$',
  full.names = TRUE) %>% 
  map(~ .x %>% 
        read_sf() %>% 
        st_make_valid()) %>%
  set_names(
    'cv',
    'rear_edge') %>% 
  list2env(.GlobalEnv)

# raster ------------------------------------------------------------------

range_map <- 
  rast(
    paste0(
      predictions,
      'grasses_mean_predictions.tif'))

# best model results ------------------------------------------------------

bm_results <-
    list.files(
      models,
      pattern = 'model_results.rds',
      full.names = T) %>% 
      map(~ .x %>% 
            read_rds() %>% 
            pluck('best_model_results') %>%
            rename_all(., .funs = tolower)) %>% 
      set_names(my_species)

# x10 ---------------------------------------------------------------------

x10 <-
  bm_results %>% 
  map(
    ~ .x %>% 
      filter(
        parameter == 
          'X10.percentile.training.presence.Cloglog.threshold') %>% 
      dplyr::select(value) %>% 
      pull()) %>% 
  as_vector() 

# Bromus rubesns = 0.29, 
# Schismus arabicus = 0.27 
# Schismus barbatus = 0.25

# x10 threshold -----------------------------------------------------------

#br

x10_br <- 
  range_map > x10[1]

x10_br <-
  x10_br %>% 
  terra::as.factor() 

#sa

x10_sa <- 
  range_map > x10[2]

x10_sa <-
  x10_sa %>% 
  terra::as.factor() 

#sb

x10_sb <- 
  range_map > x10[3]

x10_sb <-
  x10_sb %>% 
  terra::as.factor() 

#addtion

x10_raster <- 
  sum(
    x10_br, 
    x10_sa, 
    x10_sb)

plot(x10_raster)

x10_raster %>%
  writeRaster(
    paste0(
      predictions, 
      'grasses_x10.tif'), 
    overwrite = TRUE)

# mtss --------------------------------------------------------------------

mtss <- 
  bm_results %>% 
  map(
    ~ .x %>% 
      filter(
        parameter == 
          'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold') %>% 
      dplyr::select(value) %>% 
      pull()) %>% 
  as_vector() 

# Bromus rubesns = 0.35, 
# Schismus arabicus = 0.39 
# Schismus barbatus = 0.34

# mtss threshold ----------------------------------------------------------

mtss_br <- 
  range_map > mtss[1]

mtss_br <-
  mtss_br %>% 
  terra::as.factor() 

mtss_sa <- 
  range_map > mtss[2]

mtss_sa <-
  mtss_sa %>% 
  terra::as.factor() 

mtss_sb <- 
  range_map > mtss[3]

mtss_sb <-
  mtss_sb %>% 
  terra::as.factor() 

#addtion

mtss_raster <- 
  sum(
    mtss_br, 
    mtss_sa, 
    mtss_sb)

plot(mtss_raster)

mtss_raster %>%
  writeRaster(
    paste0(
      predictions, 
      'grasses_mtss.tif'), 
    overwrite = TRUE)

# area x10 ----------------------------------------------------------------

a <- 
  x10_raster %>% 
  as.polygons() %>%
  st_as_sf() %>% 
  st_area() %>% 
  units::set_units('km^2') %>% 
  as_vector()

a_cv <- 
  x10_raster %>% 
  as.polygons() %>%
  crop(vect(cv)) %>% 
  st_as_sf() %>% 
  st_area() %>% 
  units::set_units('km^2') %>% 
  as_vector()

b <- 
  mtss_raster %>% 
  as.polygons() %>%
  st_as_sf() %>% 
  st_area() %>% 
  units::set_units('km^2') %>% 
  as_vector()

b_cv <- 
  mtss_raster %>% 
  as.polygons() %>%
  crop(vect(cv)) %>% 
  st_as_sf() %>% 
  st_area() %>% 
  units::set_units('km^2') %>% 
  as_vector()

# areas -------------------------------------------------------------------

units::set_units(c(a,b), 'km^2')

# as percentage

areapol <-
  \(raster){
    
    p <-
      raster %>%
      as.polygons() %>%
      st_as_sf() %>% 
      st_area()
    
    p1 <- 
      units::set_units(p, 'km^2') %>% 
      as_vector()/sum(p/1000000)/10000
    
    p1 <- round(p1) 
    return(p1)
  }

(x10_areas <- areapol(x10_raster))
(mtss_areas <- areapol(mtss_raster))

(x10_areas <- areapol(x10_raster %>% crop(vect(cv))))
(mtss_areas <- areapol(mtss_raster %>% crop(vect(cv))))
