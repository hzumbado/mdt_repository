# Mojave desert tortoise distribution
# Modeling
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
predictions <- 'output/models/tortoise/predictions/'
future_preds <- 'rasters/future_preds/'
layers <- 'rasters/future/'

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

range_map <- 
  rast(
    paste0(
      predictions, 
      'mdt_predictions_hd_model.tif'))

mtss <-
  read_rds(
    paste0(
      models, 
      'hd_model_results.rds')) %>% 
  pluck('best_model_results') %>% 
  rename_all(., .funs = tolower) %>% 
  filter(
    parameter == 'Maximum.training.sensitivity.plus.specificity.Cloglog.threshold') %>%
  select(value) %>%
  pull() 

rear_edge_mask <- 
  terra::rasterize(
  terra::vect(rear_edge),
  range_map,
  field = 1)

current_mtss <- 
  terra::ifel(
    range_map >= mtss,
    1,
    NA) |>
  terra::mask(rear_edge_mask)

future_files <- 
  list.files(
  future_preds,
  pattern = "\\.tif$",
  full.names = TRUE)

layer_names <- 
  basename(future_files) |>
  stringr::str_remove("\\.tif$")

hadgem_future <- 
  future_files |>
  purrr::map(\(f) {
    
    future_raster <- terra::rast(f)
    
    # Restrict to currently suitable habitat
    future_raster <- terra::mask(
      future_raster,
      current_mtss)
    
    # Binary future suitability using the current-model MTSS
    terra::ifel(
      future_raster >= mtss,
      1,
      NA
    )
  }) |>
  rlang::set_names(layer_names)

# areas -------------------------------------------------------------------

current_binary <- ifel(
  range_map >= mtss,
  1,
  NA)

current_area <- current_binary %>%
  cellSize(unit = "km", mask = TRUE) %>%
  global("sum", na.rm = TRUE) %>%
  pull()

current_area

current_area <- 
  terra::cellSize(
  current_mtss,
  unit = "km",
  mask = TRUE) |>
  terra::global(
    fun = "sum",
    na.rm = TRUE) |>
  as.numeric()

future_area <- hadgem_future |>
  purrr::map_dbl(\(r) {
    
    terra::cellSize(
      r,
      unit = "km",
      mask = TRUE
    ) |>
      terra::global(
        fun = "sum",
        na.rm = TRUE
      ) |>
      as.numeric()
  })


period <- rep(
  c("2041-2060", "2061-2080", "2081-2100"),
  times = 3
)

ssp <- rep(
  c("SSP126", "SSP245", "SSP585"),
  each = 3
)

area_final <- tibble::tibble(
  period = period,
  ssp = ssp,
  area = future_area
) |>
  dplyr::mutate(
    change = 100 * (area - current_area) / current_area,
    area = round(area, 0),
    change = round(change, 1),
    ESH = paste0(
      area,
      " (",
      dplyr::if_else(change > 0, paste0("+", change), as.character(change)),
      "%)"
    )
  ) |>
  dplyr::select(period, ssp, ESH) |>
  tidyr::pivot_wider(
    names_from = ssp,
    values_from = ESH
  ) |>
  dplyr::rename(Period = period)

area_final


future_continuous <- future_files |>
  purrr::map(terra::rast)

purrr::map_dfr(
  future_continuous,
  \(r) {
    tibble::tibble(
      min = terra::global(r, "min", na.rm = TRUE)[1, 1],
      mean = terra::global(r, "mean", na.rm = TRUE)[1, 1],
      max = terra::global(r, "max", na.rm = TRUE)[1, 1],
      cells_above_mtss = terra::global(
        r >= mtss,
        "sum",
        na.rm = TRUE
      )[1, 1]
    )
  },
  .id = "scenario"
)


current_area
mtss
global(current_mtss, "sum", na.rm = TRUE)



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
