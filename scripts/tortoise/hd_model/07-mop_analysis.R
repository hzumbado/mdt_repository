

# setup -------------------------------------------------------------------

rm(list = ls())

library(terra)
library(sf)
library(smop)
library(tidyverse)

future_path <- "rasters/future/"
present_path <- "rasters/present/"
output_path <- "output/mop/"

dir.create(
  output_path, 
  recursive = TRUE, 
  showWarnings = FALSE)

m <-
  read_sf("shapefiles/calibration_area.gpkg") %>%
  st_make_valid()


# list.files(
#   'shapefiles',
#   pattern = '(calib|salt|rear)',
#   full.names = TRUE) %>%
#   map(~ (read_sf(.x)) %>% 
#         st_make_valid() %>% 
#         st_transform(crs = 4326)) %>% 
#   set_names(
#     'M',
#     'rear_edge',
#     'salton') %>% 
#   list2env(.GlobalEnv)
  
# rasters -----------------------------------------------------------------


# Environmental predictors (present)

present <- 
  rast('rasters/present/envs_stack.tif') %>% 
  crop(
    vect(m),
    mask = TRUE)


vars <-
  c(
    "bio_12",
    "slope",
    "tpi",
    "sand",
    "cvfo")

present <-
  present[[vars]]

names(present)


future_files <-
  list.files(
    'rasters/future',
    pattern = '\\.tif$',
    full.names = TRUE)

future_envs <-
  future_files %>%
  map(
    ~ .x %>%
      rast()) %>%
  set_names(
    basename(future_files) %>%
      str_remove('\\.tif$'))

plot(future_envs)


# check predictor names/order ---------------------------------------------

walk(
  future_envs,
  \(r) {
    stopifnot(
      identical(
        names(present),
        names(r)))})

# MOP analyses ------------------------------------------------------------



mop_ssps <-
  future_files %>%
  map(
    ~ mop(
      M_calibra = present,
      G_transfer = .x,
      percent = 10,
      normalized = TRUE,
      standardize_vars = TRUE))

# inspect -----------------------------------------------------------------

mop_ssps[[1]]

plot(
  mop_ssps[[1]],
  main = names(mop_ssps)[1])

# strict extrapolation summary --------------------------------------------

extrap_summary <-
  mop_ssps |>
  imap_dfr(
    \(mop_raster, scenario) {
      
      vals <-
        values(
          mop_raster,
          mat = FALSE)
      
      total_pixels <-
        sum(
          !is.na(vals))
      
      strict_pixels <-
        sum(
          vals == 0,
          na.rm = TRUE)
      
      tibble(
        scenario = scenario,
        total_pixels = total_pixels,
        strict_extrap_pixels = strict_pixels,
        percent_strict_extrap =
          100 * strict_pixels / total_pixels)})

extrap_summary

# strict extrapolation rasters --------------------------------------------

strict_maps <-
  mop_ssps %>%
  map(
    ~ ifel(
      is.na(.x),
      NA,
      ifel(
        .x == 0,
        1,
        0)))

strict_stack <-
  strict_maps %>%
  rast()

strict_stack_cat <-
  strict_stack %>%
  as.factor()

for(i in 1:nlyr(strict_stack_cat)){
  
  levels(strict_stack_cat[[i]]) <-
    data.frame(
      ID = c(0, 1),
      class = c(
        "No strict extrapolation",
        "Strict extrapolation"))}

names(strict_stack_cat) <-
  names(strict_stack)

plot(
  strict_stack_cat,
  col = c("purple4", "yellow"))

# save MOP rasters --------------------------------------------------------

iwalk(
  mop_ssps,
  \(r, nm) {
    writeRaster(
      r,
      paste0(
        output_path,
        nm,
        "_mop.tif"),
      overwrite = TRUE)})

# save strict extrapolation rasters ---------------------------------------

iwalk(
  strict_maps,
  \(r, nm) {
    
    writeRaster(
      r,
      paste0(
        output_path,
        nm,
        "_strict_extrapolation.tif"),
      overwrite = TRUE)})

# save summary ----------------------------------------------------------

write_csv(
  extrap_summary,
  paste0(
    output_path,
    "mop_strict_extrapolation_summary.csv"))


bio12_min <-
  global(
    present$bio_12,
    "min",
    na.rm = TRUE
  )[1, 1]

bio12_max <-
  global(
    present$bio_12,
    "max",
    na.rm = TRUE
  )[1, 1]

bio12_novelty <-
  future_envs |>
  imap_dfr(
    \(r, nm) {
      
      vals <-
        values(
          r$bio_12,
          mat = FALSE)
      
      total <-
        sum(!is.na(vals))
      
      outside <-
        sum(
          vals < bio12_min |
            vals > bio12_max,
          na.rm = TRUE)
      
      tibble(
        scenario = nm,
        percent_bio12_outside =
          100 * outside / total)})

bio12_novelty
