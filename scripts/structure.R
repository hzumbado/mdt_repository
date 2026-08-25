# project structure

folders <-
  c('data',
    'shapefiles',
    'rasters',
    'rasters/dem',
    'rasters/distance',
    'rasters/future',
    'rasters/future_preds',
    'rasters/hfp',
    'scripts/grasses',
    'scripts/tortoise',
    'outputs/tables',
    'output/mop',
    'output/models',
    'output/models/grasses/files',
    'output/models/grasses/predictions',
    'output/models/tortoise/files',
    'output/models/tortoise/predictions')

sapply(
  folders,
  FUN = dir.create,
  recursive = TRUE)
