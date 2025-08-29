# Mojave desert tortoise distribution
# SDM

# setup -------------------------------------------------------------------

rm(list = ls())

library(rJava)
library(ENMeval)
library(terra)
library(tidyterra)
library(tidyverse)

# folders -----------------------------------------------------------------

models <- 'output/models/tortoise/files/'

# data --------------------------------------------------------------------

data <-
  read_rds('data/data_model_obscured.rds')

# raster ------------------------------------------------------------------

envs <- 
  rast(
    paste0(
      'rasters/present/envs_reduced.tif'))

# model data --------------------------------------------------------------

occs <- 
  data %>% 
  filter(presence == 1) %>% 
  select(x, y) 

bg <- 
  data %>% 
  filter(presence == 0) %>% 
  select(x, y)

# model -------------------------------------------------------------------

sdm <- 
  ENMevaluate(
    occs = occs, 
    envs = envs, 
    bg = bg, 
    tune.args = 
      list(fc = 
             c('L', 'Q', 'H', 'LQ', 'LH', 'QH', 'LQH'), 
           rm = 1:4), 
    partitions = "block",
    algorithm = "maxent.jar", 
    doClamp = TRUE, 
    overlap = FALSE,
    taxon.name = "Gopherus agassizii",
    parallel = TRUE)

model_results <-
  sdm@results %>% 
  as_tibble() 

# model selection ---------------------------------------------------------

opt.seq <- 
  sdm@results %>% 
  filter(auc.val.avg == max(auc.val.avg)) %>%
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  select(
    tune.args,
    auc = 'auc.train',
    AUC = 'auc.val.avg') #data best model # 

# best model --------------------------------------------------------------

bm <-
  sdm@models %>% 
  pluck(opt.seq$tune.args[1])

bm_results <-
  bm@results %>% 
  as.data.frame() %>%
  rownames_to_column(
    var = 'Parameter') %>% 
  as_tibble() %>%
  rename(Value =  V1)

# var contribution --------------------------------------------------------

var_contrib <-
  sdm@variable.importance[[opt.seq$tune.args[1]]] %>% 
  as_tibble() %>%
  mutate(
    variable = variable %>% 
      fct_recode(
        'Precipitation of Warmest Quarter' = 'wc2.1_30s_bio_18',
        'Northness' = 'northness_1KM',
        'Human footprint' = 'hfp_2020',
        'Rugedness' = 'vrm_1KM',
        'Precipitation Seasonality' = 'wc2.1_30s_bio_15',
        'Mean Diurnal Range' = 'wc2.1_30s_bio_02')) %>% 
  arrange(desc(percent.contribution))

# response curves --------------------------------------------------------

predicts::partialResponse(
  sdm@models[[opt.seq$tune.args]])

# predictions ------------------------------------------------------------

predictions <- 
  data %>%
  mutate(
    prediction =
      as.vector(
        terra::predict(
          bm, 
          data, 
          type = "cloglog"))) %>%
  select(
    species:y, 
    prediction, 
    everything()) 

# best model settings -----------------------------------------------------

sdm@results %>%
  as_tibble() %>%
  mutate(rm = as_factor(rm)) %>% 
  ggplot(aes(
    x = fc, 
    y = auc.val.avg, 
    group = rm, 
    col = rm)) +
  geom_point() +
  scale_color_manual(values = c(
    '#999999', 
    '#E69F00', 
    '#56B4E9', 
    'salmon')) +
  geom_line(aes(col = rm)) +
  theme_classic()

# save model --------------------------------------------------------------

sdm %>% 
  write_rds(
    paste0(
      models, 
      "model_reduced_vars.rds"))

# save results -----------------------------------------------------------

list(
  model_results = model_results,  
  opt.seq = opt.seq,
  best_model = bm,
  best_model_results = bm_results,  
  predictions = predictions,
  var_contribution = var_contrib) %>% 
  write_rds(
    paste0(
      models,
      'model_results.rds'))
