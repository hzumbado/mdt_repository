# Mojave desert tortoise distribution
# Modeling
# Script 06
# distance raster

# setup -------------------------------------------------------------------

rm(list = ls())

library(sf)
library(terra)
library(predicts)
library(spatstat)
library(tmap)
library(tidyverse)

# data --------------------------------------------------------------------

list.files(
  'shapefiles',
  pattern = '(cv|salton|cal_road|jtpb).*\\.gpkg$',
  full.names = TRUE) %>% 
  map(~ .x %>% 
        read_sf() %>% 
        st_make_valid() %>%
        janitor::clean_names()) %>%
  set_names(
    'road',
    'cv',
    'jtpb',
    'salton') %>% 
  list2env(.GlobalEnv)

# shapefiles processed ----------------------------------------------------

study_area <-
  st_bbox(
    c(
      xmin = -116.9,
      xmax = -115.5,
      ymin = 33.4,
      ymax = 34.05),
    crs = st_crs(cv)) %>%
  st_as_sfc()

road_cv <- 
  road %>%
  st_filter(study_area)

road_cv_all <-
  st_crop(
    jtpb, 
    st_bbox(study_area)) %>% 
  st_union(road_cv)

# occurrences -------------------------------------------------------------

occs_cv <- 
  read_rds('data/tortoise_model_data.rds') %>% 
  filter(presence == 1) %>% 
  select(x, y) %>% 
  st_as_sf(
    coords = c(
      x = 'x', 
      y = 'y'), 
    crs = 4326,
    remove = FALSE) %>% 
  st_filter(study_area) #237 occs

# rasters -----------------------------------------------------------------

elevation <-
  rast('rasters/dem/dem_gopherus.tif') %>%
  crop(study_area) %>% 
  clamp(lower = 0)

# distance raster ---------------------------------------------------------

st_dist <- 
  terra::distance(
    rasterize(
      vect(road_cv_all),
      elevation))  

names(st_dist) <- 'distance'

st_dist_ext <-
  terra::extract(
    st_dist, 
    occs_cv, 
    ID = FALSE) %>% 
  as_tibble() %>% 
  bind_cols(occs_cv) %>% 
  filter(!is.na(distance))

# save files --------------------------------------------------------------

st_dist %>% 
  writeRaster(
    paste0(
      'rasters/distance/distance.tif'),
    overwrite = TRUE)

st_dist_ext %>% 
  write_rds('data/distance_data.rds')

# analyses ----------------------------------------------------------------

mean(st_dist_ext$distance, na.rm = T) #3776.852
which(st_dist_ext$distance <= 3000) %>% 
  length/nrow(st_dist_ext)*100 # 41/69 ~ 56.1% 
which(st_dist_ext$distance <= 5000) %>% 
  length/nrow(st_dist_ext)*100# 51/69 ~ 72.1%

# kernel density ----------------------------------------------------------

# Original analysis extent

study_area_utm <-
  st_transform(
    study_area,
    32611)

# Add buffer around KDE window to avoid edge truncation

kde_window <-
  st_buffer(
    study_area_utm,
    dist = 15000)

# Occurrences

occs_utm <-
  st_transform(
    occs_cv,
    32611)

xy <-
  st_coordinates(occs_utm)

# Point pattern using buffered window

mdt_ppp <-
  spatstat.geom::ppp(
    x = xy[, 1],
    y = xy[, 2],
    window = spatstat.geom::as.owin(kde_window))

# Scott bandwidth

sigma <-
  spatstat.explore::bw.scott(mdt_ppp)

sigma

# KDE

mdt_image <-
  spatstat.explore::density.ppp(
    mdt_ppp,
    sigma = sigma,
    kernel = "gaussian",
    eps = 1000)

# Convert to raster

mdt_grid <-
  terra::rast(mdt_image)

terra::crs(mdt_grid) <- "EPSG:32611"

names(mdt_grid) <- "Density"

# Crop back to original study area

mdt_grid <-
  terra::crop(
    mdt_grid,
    terra::vect(study_area_utm))

cv_utm <-
  st_transform(
    cv,
    32611)

plot(mdt_grid)

plot(
  st_geometry(cv_utm),
  add = TRUE,
  border = "red",
  lwd = 2)

plot(
  st_geometry(occs_utm),
  add = TRUE,
  pch = 16,
  cex = 0.5)

# Save

terra::writeRaster(
  mdt_grid,
  "rasters/distance/mdt_density.tif",
  overwrite = TRUE)

