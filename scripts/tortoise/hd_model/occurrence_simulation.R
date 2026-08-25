# simulated occurrences
# Generate simulated Gopherus agassizii occurrences for the public
# repository. Occurrences are sampled within the calibration area using
# habitat suitability as sampling weights and jittered within raster cells.
# These data are intended only to reproduce the analytical workflow and
# do not represent actual Mojave Desert tortoise locations.


# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(sf)
library(tidyverse)

# suitability -------------------------------------------------------------

suitability <-
  rast('output/models/tortoise/predictions/mdt_predictions_hd_model.tif')

calibration <-
  vect('shapefiles/calibration_area.gpkg')

suitability <-
  crop(
    suitability,
    calibration) %>%
  mask(calibration)

# raster cells ------------------------------------------------------------

cells <-
  as.data.frame(
    suitability,
    xy = TRUE,
    na.rm = TRUE)

names(cells)[3] <-
  'suitability'

# simulated occurrences ---------------------------------------------------

set.seed(123)

occ_sim <-
  cells %>%
  slice_sample(
    n = 1000,
    weight_by = suitability,
    replace = FALSE)

# jitter ------------------------------------------------------------------

cell_size_x <-
  res(suitability)[1]

cell_size_y <-
  res(suitability)[2]

occ_sim <-
  occ_sim %>%
  mutate(
    x = x + runif(
      n(),
      -cell_size_x / 2,
      cell_size_x / 2),
    y = y + runif(
      n(),
      -cell_size_y / 2,
      cell_size_y / 2))

# sf ----------------------------------------------------------------------

occ_sim <-
  st_as_sf(
    occ_sim,
    coords = c('x', 'y'),
    crs = crs(suitability))

# calibration area --------------------------------------------------------

calibration_sf <-
  st_as_sf(calibration)

occ_sim <-
  occ_sim %>%
  st_filter(
    calibration_sf,
    .predicate = st_within)

# final sample ------------------------------------------------------------

occ_sim <-
  occ_sim %>%
  slice_sample(
    n = 842)

# attributes --------------------------------------------------------------

occ_sim$species <-
  'Gopherus agassizii'

occ_sim$simulated <-
  TRUE

# check -------------------------------------------------------------------

nrow(occ_sim)

all(
  st_within(
    occ_sim,
    calibration_sf,
    sparse = FALSE))

# save --------------------------------------------------------------------

st_write(
  occ_sim,
  'data/mdt_occ_simulated.gpkg',
  delete_dsn = TRUE,
  quiet = TRUE)

occ_sim %>% 
  st_drop_geometry() %>%
  as_tibble() %>% 
  select(species, simulated, everything())
