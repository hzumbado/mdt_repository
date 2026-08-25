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
   #'Bromus_rubens' 
  'Schismus_arabicus'
  #'Schismus_barbatus'

my_species

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

# source ------------------------------------------------------------------

source('scripts/tortoise/hd_model/tss_metrics.R')

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
    user.eval = tss_metrics,
    doClamp = TRUE, 
    overlap = FALSE,
    taxon.name = my_species,
    parallel = TRUE)

sdm %>%
  write_rds(
    paste0(
      models,
      my_species,
      '_model_reduced_vars_hd.rds'))

# sdm <-
#   read_rds(
#     paste0(
#       models,
#       my_species,
#       '_model_reduced_vars_hd.rds'))

# model results -----------------------------------------------------------

model_results <-
  sdm@results %>% 
  as_tibble() 

# model selection ---------------------------------------------------------

ranked_models <- 
  sdm@results %>%
  filter(!is.na(or.10p.avg), !is.na(cbi.val.avg)) %>%
  filter(or.10p.avg <= 0.20) %>%
  arrange(
    desc(cbi.val.avg),
    desc(auc.val.avg),
    desc(tss_10ppt_val.avg),
    or.10p.avg,
    ncoef) %>% 
  select(
    tune.args,
    or.10p = 'or.10p.avg', 
    bi = 'cbi.val.avg', 
    tss = 'tss_max_val.avg',
    auc = 'auc.val.avg', 
    ncoef)

opt.seq <-
  ranked_models %>%
  slice(1)

opt.seq

# best model --------------------------------------------------------------

bm <-
  sdm@models %>% 
  pluck(opt.seq$tune.args[1])

bm_results <-
  bm@results %>% 
  as.data.frame() %>%
  rownames_to_column(
    var = "Parameter") %>% 
  as_tibble() %>%
  rename(Value =  V1)

bm_results %>% 
  print(n = 56)

# var contribution --------------------------------------------------------

var_contrib <-
  sdm@variable.importance[[opt.seq$tune.args[1]]] %>% 
  as_tibble() %>%
  arrange(desc(percent.contribution))

var_contrib
      
# response curves --------------------------------------------------------

predicts::partialResponse(
  sdm@models[[opt.seq$tune.args]])

# predictions ------------------------------------------------------------

data <- 
  data %>%
  filter(species == my_species)

vars <- colnames(bm@presence)

data %>%
  select(all_of(vars)) %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))))

# repair missing predictor values ----------------------------------------

vars %>%
  walk(
    ~ {
      
      if(all(is.na(data[[.x]]))){
        
        data[[.x]] <<-
          values(
            envs[[.x]]
          )[data$cells]
      }
    })


# predictions -------------------------------------------------------------

predictions <- 
  data %>%
  mutate(
    prediction =
      as.vector(
        predicts::predict(
          bm,
          data, 
          args = 'outputformat=cloglog'))) %>%
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
      '_hd_model_results.rds'))
