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
tables <- 'output/tables/'

# Species data ------------------------------------------------------------

my_species <- 'Gopherus agassizii'

# study area ---------------------------------------------------------------

rear_edge <-
  read_sf('shapefiles/rear_edge.gpkg') %>%
  st_make_valid() %>%
  st_transform(4326)

# continuous prediction ---------------------------------------------------

range_map <-
  rast(
    paste0(
      predictions,
      'mdt_predictions_hd_model.tif'))

names(range_map) <- 'Suitability'

# restrict prediction to rear-edge study area -----------------------------

range_map_rear <-
  mask(
    range_map,
    vect(rear_edge))

# best model results ------------------------------------------------------

bm_results <-
  read_rds(
    paste0(
      models,
      'hd_model_results.rds')) %>%
  pluck('best_model_results') %>%
  rename_with(tolower)

# model-derived thresholds ------------------------------------------------

x10 <-
  bm_results %>%
  filter(
    parameter ==
      'X10.percentile.training.presence.Cloglog.threshold') %>%
  pull(value)

mtss <-
  bm_results %>%
  filter(
    parameter ==
      'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold') %>%
  pull(value)

x10
mtss

# continuous suitability above each threshold -----------------------------

suitability_x10 <-
  ifel(
    range_map_rear >= x10,
    range_map_rear,
    NA)

suitability_mtss <-
  ifel(
    range_map_rear >= mtss,
    range_map_rear,
    NA)

names(suitability_x10) <- 'x10'
names(suitability_mtss) <- 'mtss'

plot(suitability_x10)
plot(suitability_mtss)

# save continuous thresholded predictions --------------------------------

c(suitability_x10, suitability_mtss) %>%
  writeRaster(
    paste0(
      predictions,
      'mdt_summarized_predictions_hd_model.tif'),
    overwrite = TRUE)

# binary predictions ------------------------------------------------------

bin_x10 <-
  ifel(
    range_map_rear >= x10,
    1,
    NA)

bin_mtss <-
  ifel(
    range_map_rear >= mtss,
    1,
    NA)

names(bin_x10) <- 'x10'
names(bin_mtss) <- 'mtss'

plot(bin_x10)
plot(bin_mtss)

# save binary predictions -------------------------------------------------

binary_predictions <-
  c(bin_x10, bin_mtss)

writeRaster(
  binary_predictions,
  paste0(
    predictions,
    'mdt_binary_predictions_hd_model.tif'),
  overwrite = TRUE)

# current ESH area by threshold -------------------------------------------

area_x10 <-
  cellSize(
    bin_x10,
    unit = 'km',
    mask = TRUE) %>%
  global(
    fun = 'sum',
    na.rm = TRUE) %>%
  pull(1)

area_mtss <-
  cellSize(
    bin_mtss,
    unit = 'km',
    mask = TRUE) %>%
  global(
    fun = 'sum',
    na.rm = TRUE) %>%
  pull(1)

area_x10
area_mtss

# suitability classification ----------------------------------------------
#
# 1 = Unsuitable: suitability < 10PTP
# 2 = Low:        10PTP <= suitability < MTSS
# 3 = Moderate:   MTSS <= suitability < 0.63
# 4 = High:       suitability >= 0.63

suitable_values <- 
  terra::values(
  range_map_rear,
  na.rm = TRUE)

high_threshold <- 
  unname(
  stats::quantile(
    suitable_values[suitable_values >= x10],
    probs = 0.90,
    na.rm = TRUE))

rclas <-
  matrix(
    c(
      -Inf, x10,            1,
      x10,  mtss,           2,
      mtss, high_threshold, 3,
      high_threshold, Inf,  4),
    ncol = 3,
    byrow = TRUE)

suitability_classes <-
  classify(
    range_map_rear,
    rclas,
    right = FALSE)

names(suitability_classes) <- 'class_id'

plot(suitability_classes)

# save categorical map ----------------------------------------------------

writeRaster(
  suitability_classes,
  paste0(
    predictions,
    'mdt_suitability_classes_hd_model.tif'),
  overwrite = TRUE)

# area by suitability class -----------------------------------------------

cell_area <-
  cellSize(
    suitability_classes,
    unit = 'km')

area_by_class <-
  zonal(
    cell_area,
    suitability_classes,
    fun = 'sum',
    na.rm = TRUE) %>%
  as_tibble()

names(area_by_class) <- c('class_id', 'area_km2')

area_table <-
  area_by_class %>%
  mutate(
    class = case_when(
      class_id == 1 ~ 'Unsuitable',
      class_id == 2 ~ 'Low',
      class_id == 3 ~ 'Moderate',
      class_id == 4 ~ 'High',
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    percent = 100 * area_km2 / sum(area_km2)
  ) %>%
  select(
    class_id,
    class,
    area_km2,
    percent
  ) %>%
  arrange(class_id)

area_table

# threshold areas calculated from the same categorical raster ------------

area_10ptp_classes <-
  area_table %>%
  filter(class_id %in% c(2, 3, 4)) %>%
  summarise(area_km2 = sum(area_km2)) %>%
  pull(area_km2)

area_mtss_classes <-
  area_table %>%
  filter(class_id %in% c(3, 4)) %>%
  summarise(area_km2 = sum(area_km2)) %>%
  pull(area_km2)

area_10ptp_classes
area_mtss_classes

# consistency checks ------------------------------------------------------

stopifnot(
  isTRUE(
    all.equal(
      area_x10,
      area_10ptp_classes,
      tolerance = 0.01)))

stopifnot(
  isTRUE(
    all.equal(
      area_mtss,
      area_mtss_classes,
      tolerance = 0.01)))

# final tables ------------------------------------------------------------

threshold_table <-
  tibble(
    threshold = c('10PTP', 'MTSS'),
    value = c(x10, mtss),
    area_km2 = c(
      area_10ptp_classes,
      area_mtss_classes))

threshold_table

area_table %>%
  write_csv(
    paste0(
      tables,
      'mdt_suitability_area_by_class.csv'))

threshold_table %>%
  write_csv(
    paste0(
      tables,
      'mdt_esh_area_by_threshold.csv'))

# save key objects --------------------------------------------------------

write_rds(
  list(
    x10 = x10,
    mtss = mtss,
    high_threshold = high_threshold,
    area_by_class = area_table,
    area_by_threshold = threshold_table),
  paste0(
    predictions,
    'mdt_esh_results_hd_model.rds'))
