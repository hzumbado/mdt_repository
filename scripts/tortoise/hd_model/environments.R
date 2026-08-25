# setup -------------------------------------------------------------------

rm(list = ls())

library(usdm)
library(tidyterra)
library(sf)
library(terra)
library(humboldt)
library(tmap)
library(tidyverse)

# species -----------------------------------------------------------------

my_species <- "Gopherus agassizii"

# data --------------------------------------------------------------------

data <- 'data/tortoise/processed/'

occs <- 
  read_rds(
    paste0(data,
    'rear_edge_dataset_2026.rds'))

# occs <- 
#   read_rds(
#     paste0(
#       data,
#       'obscured_mdt_occs.rds')) %>% 
#   st_drop_geometry()

occs_sf <- 
  occs %>% 
  st_as_sf(
    coords = c(
      x = 'x', 
      y = 'y'), 
    crs = 4326)

# shapefiles --------------------------------------------------------------

world <- 
  read_sf('shapefiles/geography/world.gpkg') %>%
  st_make_valid() %>%
  janitor::clean_names()

list.files(
  'shapefiles/processed', 
  pattern = '(area_2|2026).*\\.gpkg$',
  full.names = TRUE) %>%
  map(~ .x %>% 
        sf::read_sf() %>% 
        sf::st_make_valid() %>%
        janitor::clean_names()) %>%
  set_names('calibration_area', 'rear_edge') %>% 
  list2env(.GlobalEnv)

study_area <-  
  sf::st_bbox(
    c(
      xmin = -124.5, 
      xmax = -109,
      ymin = 31, 
      ymax = 42.5),
    crs = sf::st_crs(world)) %>%  
  sf::st_as_sfc()

# worldclim ---------------------------------------------------------------



wc <- 
  list.files(
    # 'C:/YOUR_LOCAL_PATH/worldclim/wc_1km',
    'C:/Users/zumba/Documents/rasters/worldclim/wc_1km',
    pattern = '\\.tif$',
    full.names = TRUE) %>% 
  rast() %>% 
  tidyterra::select(5, 12) %>% 
  crop(
    calibration_area, 
    mask = TRUE)

names(wc) <- c('bio_05', 'bio_12')

# topography --------------------------------------------------------------

topography <- 
  list.files(
    'C:/YOUR_LOCAL_PATH/topography',
    'C:/Users/zumba/Documents/rasters/topography',
    pattern = '\\.tif$',
    full.names = TRUE) %>% 
  rast() %>% 
  select(12, 14) %>% 
  crop(
    calibration_area, 
    mask = TRUE) %>% 
  project(wc)

names(topography) <- c('slope', 'tpi')

compareGeom(topography, wc$bio_05)

# soils -------------------------------------------------------------------

soils <- 
  list.files(
    'C:/YOUR_LOCAL_PATH/soils',
    'C:/Users/zumba/Documents/rasters/soils',
    pattern = '.tif',
    full.names = TRUE) %>% 
  map(~.x %>% 
        rast() %>% 
        crop(
          calibration_area, 
          mask = TRUE) %>%
        resample(
          wc$bio_05, 
          method = "bilinear")) %>%
  rast() %>% 
  tidyterra::select(2, 3)

names(soils)

compareGeom(soils, wc$bio_05)

# stack -------------------------------------------------------------------

envs <-
  c(
    wc, 
    topography,
    soils)

vif <- 
  vifcor(envs, 0.8, method = 'pearson') 

global_cor_matrix <- 
  layerCor(envs, fun = "cor", use = "pairwise.complete.obs") 

# saving stack ------------------------------------------------------------

envs <- 
  envs %>% 
  select(
    dplyr::all_of(vif@results$Variables))

envs %>% 
  writeRaster(
    'rasters/model/envs_stack3.tif',
    overwrite = TRUE)

calibration_area %>% 
  tm_shape() +
   tm_borders(lwd = 3) +
  tm_shape(envs$slope) +
  tm_raster(
    col.scale = 
      tm_scale_continuous(values = 'scico.roma')) +
  tm_shape(occs_sf) +
  tm_dots(
    fill = 'region',
    fill.scale = 
      tm_scale(values = 'brewer.set2'),
    size = 0.2,
    shape = 21) +
  tm_layout(legend.outside = TRUE)

# background --------------------------------------------------------------

set.seed(123)

p <- vect(occs_sf)

background <-
  predicts::backgroundSample(
    mask = envs,
    n = 20000,
    p = p,
    excludep = TRUE) %>%
  as_tibble() %>%
  humboldt.occ.rarefy(colxy = c(1, 2), 1, 'km') %>% 
  as_tibble() %>% 
  mutate(
    species = my_species, 
    presence = as.factor(0)) %>% 
  ecospat::ecospat.occ.desaggregation(
    min.dist = res(envs))

# environmental data ------------------------------------------------------

data <- 
  fuzzySim::gridRecords(
    rst = envs, 
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
    .by = presence)#842,bg 10562

data %>%
  write_rds(
    'data/tortoise/processed/data_model3.rds')

# data %>% 
#   write_rds(
#     'data/tortoise/processed/data_model_obscured.rds')

# prevalence --------------------------------------------------------------

fuzzySim::prevalence(data$presence) #0.074

# map ---------------------------------------------------------------------

world %>%
  tm_shape(bb = study_area) +
  tm_grid(lines = F, labels.size = 0.8) +
  tm_polygons('#C8B097') +
  tm_shape(wc$bio_12) +
  tm_raster(
    col.scale = 
      tm_scale_continuous(values = 'viridis')) +
  tm_shape(rear_edge, is.main = T) +
  tm_polygons(fill = 'yellow') +
  tm_shape(
    background %>% 
      st_as_sf(
        coords = c(
          x = 'x', 
          y = 'y'), 
        crs = 4326)) +
  tm_dots(
    size = 0.1,
    col = 'black') +
  tm_shape(occs_sf) +
  tm_dots(
    size = 0.2,
    shape = 21) +
  tm_xlab(
    'Longitude', 
    size = 1.2) +
  tm_ylab(
    'Latitude', 
    size = 1.2, 
    rotation = 90) +
  tm_layout(
    title.size = 2,
    legend.outside = T,
    legend.title.size = 1.5,
    legend.title.fontface = 'bold',
    legend.text.size = 1,
    bg.color = 'lightblue')
