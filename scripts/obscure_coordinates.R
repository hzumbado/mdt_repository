# obscure coordinates

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(tmap)
library(usdm)
library(tidyterra)
library(tidyverse)
library(tidyverse)

my_species <- "Gopherus agassizii"

# shapefiles --------------------------------------------------------------

range <- 
  read_sf('shapefiles/rear_edge.gpkg') %>%
  st_make_valid() %>%
  janitor::clean_names()

list.files(
  'shapefiles', 
  pattern = '(jtnp|salton|cal).*\\.gpkg$',
  full.names = TRUE) %>%
  map(~ .x %>% 
        read_sf() %>%
        st_transform(crs = st_crs(range)) %>% 
        st_make_valid() %>%
        janitor::clean_names()) %>%
  set_names('calibration_area', 'jtnp', 'salton') %>% 
  list2env(.GlobalEnv)

dataset <-  
  read_rds("data/data_model.rds") %>% 
  filter(presence == 1) %>% 
  select(species, presence, x, y)

set.seed(123)
  
obscure_dataset <-
  dataset %>%
  mutate(
    x = x + runif(row_number(.), -0.1, 0.1),
    y = y + runif(row_number(.), -0.1, 0.1)) %>%
  st_as_sf(
    coords = c('x', 'y'),
    crs = st_crs(salton),
    remove = FALSE) %>% 
  st_filter(calibration_area) %>% 
  st_filter(
    salton, 
    .predicate = st_disjoint) 

# obscure_dataset %>% 
#   write_rds('data/occs_obscured.rds')

occs <- 
  obscure_dataset %>% 
  st_drop_geometry()

occs_sf <- 
  occs %>% 
  st_as_sf(
    coords = c(
      x = 'x', 
      y = 'y'), 
    crs = 4326)

rm(obscure_dataset)

# stack -------------------------------------------------------------------

wc <- 
  list.files(
    'C:/Users/zumba/Documents/rasters/worldclim/wc_1km',
    pattern = '\\.tif$',
    full.names = TRUE) %>% 
  rast() %>% 
  crop(
    calibration_area, 
    mask = TRUE)

# topography --------------------------------------------------------------

topography <- 
  list.files(
    'C:/Users/zumba/Documents/rasters/topography',
    pattern = '\\.tif$',
    full.names = TRUE) %>% 
  rast() %>% 
  crop(
    calibration_area, 
    mask = TRUE) %>% 
  project(wc)

# human footprint ---------------------------------------------------------

hfp <- 
  list.files(
    'rasters/hfp',
    pattern = '\\.tif$',
    full.names = TRUE) %>% 
  rast() %>% 
  crop(
    calibration_area, 
    mask = TRUE) %>% 
  project(wc)

envs <-
  c(
    wc, 
    topography,
    hfp) 

# function vif ------------------------------------------------------------

vif_sel <- 
  \(stack, th, method){
    
    vif <- 
      vifcor(stack, th, method = {{ method }}) 
    
    new_stack <- 
      stack %>% 
      select(
        dplyr::all_of(vif@results$Variables))
    return(new_stack)
  }

envs_stack <- vif_sel(envs, 0.6, 'pearson') 

names(envs_stack)

envs_reduced <- 
  envs_stack %>% 
  select(1, 3, 4, 8, 11, 12)

# saving stack ------------------------------------------------------------

envs <- file.path('rasters/envs_reduced.tif')

envs_reduced %>% 
  writeRaster(
    envs_reduced, 
    overwrite = TRUE)

# background --------------------------------------------------------------

set.seed(123)

p <- vect(occs_sf)

background <-
  predicts::backgroundSample(
    mask = envs_stack,
    n = 10000,
    p = p,
    excludep = TRUE) %>% 
  as_tibble() %>% 
  mutate(
    species = my_species, 
    presence = as.factor(0)) %>%  
  ecospat::ecospat.occ.desaggregation(
    min.dist = res(envs_reduced))

# environmental data ------------------------------------------------------

data <- 
  fuzzySim::gridRecords(
    rst = envs_stack, 
    pres.coords = occs %>% 
      select(x, y), 
    abs.coords = background %>% 
      select(x, y), 
    na.rm = T) %>% 
  as_tibble() %>% 
  mutate(species = my_species) %>%
  select(species, everything()) %>% 
  na.omit()

data %>% 
  summarize(
    n = n(),
    .by = presence)#451,bg 12791

set.seed(123)

data %>% 
  filter(
    presence == 1) %>% 
  slice_sample(n = 200) %>% 
  bind_rows(
    data %>% 
      filter(
        presence == 0)) %>% 
  write_rds(
    'data/data_model_obscured.rds')
