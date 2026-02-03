# Mojave desert tortoise distribution
# Script 01
# SDM. Invasive grasses

# setup -------------------------------------------------------------------

rm(list = ls())

library(rJava)
library(ENMeval)
library(terra)
library(tidyterra)
library(tidyverse)

# folders -----------------------------------------------------------------

models <- 'output/models/grasses/files/'

# data --------------------------------------------------------------------

data <-
  read_rds('data/grasses_model_data.rds')

my_species <- 
  model_species <- 
   'Bromus_rubens' 
  #'Schismus_arabicus'
  #'Schismus_barbatus'

# raster ------------------------------------------------------------------

envs <- 
  rast(
    paste0(
      'rasters/present/',
      my_species,
      '_envs_reduced.tif'))

# model data --------------------------------------------------------------

occs <- 
  data %>% 
  filter(
    species == my_species,
    presence == 1) %>% 
  select(x, y) 

bg <- 
  data %>% 
  filter(
    species == my_species,
    presence == 0) %>% 
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
    partitions = 'block',
    algorithm = 'maxent.jar', 
    doClamp = TRUE, 
    overlap = FALSE,
    taxon.name = my_species,
    parallel = TRUE)

sdm %>%
  write_rds(
    paste0(
      models,
      my_species,
      '_model_reduced_vars.rds'))

# sdm <-
#   read_rds(
#     paste0(
#       models,
#       my_species,
#       '_model_reduced_vars.rds'))

# model results -----------------------------------------------------------

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
  arrange(desc(percent.contribution))
      
# response curves --------------------------------------------------------

predicts::partialResponse(
  sdm@models[[opt.seq$tune.args]])

# predictions ------------------------------------------------------------

data <- 
  data %>%
  filter(species == my_species)

predictions <- 
  data %>%
  mutate(
    prediction =
      as.vector(
        terra::predict(
          bm,
          data, 
          type = 'cloglog'))) %>%
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

ggsave(
  paste0(
  'output/figures/',
  my_species, 
  '_best_model_settings_plot.jpg'),
  dpi = 300)

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
      my_species,
      '_model_results.rds'))
