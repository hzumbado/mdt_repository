# Mojave desert tortoise distribution
# Modeling
# Script 01
# SDM

# setup -------------------------------------------------------------------

rm(list = ls())

library(rJava)
library(ENMeval)
library(terra)
library(tidyterra)
library(tidyverse)

# paths -------------------------------------------------------------------

models <- 'output/models/tortoise/files/'

# source ------------------------------------------------------------------

source('scripts/tortoise/hd_model/tss_metrics.R')

# environmental data ------------------------------------------------------

envs <- 
  rast('rasters/present/envs_stack.tif')

# model data ---------------------------------------------------------

data <-
  read_rds('data/tortoise_model_data.rds')

occs <- 
  data %>% 
  filter(presence == 1) %>% 
  select(x, y) 

bg <- 
  data %>% 
  filter(presence == 0) %>% 
  select(x, y)

# model -------------------------------------------------------------------

mx <- 
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
    user.eval = tss_metrics, 
    doClamp = TRUE, 
    overlap = FALSE,
    taxon.name = "Gopherus agassizii",
    parallel = TRUE)

# model results -----------------------------------------------------------

model_results <-
  mx@results %>% 
  as_tibble() 

# save maxent model -------------------------------------------------------

mx %>% 
  write_rds(
    paste0(
      models, 
      "hd_model.rds"))

# model selection ---------------------------------------------------------

ranked_models <- 
  mx@results %>%
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
    auc = 'auc.val.avg', 
    tss = 'tss_max_val.avg', 
    tss_10  = 'tss_10ppt_val.avg', 
    ncoef)

opt.seq <-
  ranked_models %>%
  slice(1)

# best model --------------------------------------------------------------

bm <-
  mx@models %>% 
  pluck(opt.seq$tune.args[1])

bm_results <-
  bm@results %>% 
  as.data.frame() %>%
  rownames_to_column(
    var = "Parameter") %>% 
  as_tibble() %>%
  rename(Value =  V1)

view(bm_results)

# var contribution --------------------------------------------------------

var_contrib <-
  mx@variable.importance[[opt.seq$tune.args[1]]] %>% 
  as_tibble() %>%
  mutate(
    variable = variable %>% 
      fct_recode(
        'Annual Precipitation' = 'bio_12',
        'Coarse Fragment Volume' = 'cvfo',
        'Sand Content' = 'sand',
        'Slope' = 'slope',
        'Topographic Position Index' = 'tpi')) %>% 
  arrange(desc(percent.contribution))

# response curves --------------------------------------------------------

predicts::partialResponse(
  mx@models[[opt.seq$tune.args]])

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

mx@results %>%
  as_tibble() %>%
  mutate(rm = as_factor(rm)) %>% 
  ggplot(aes(x = fc, y = auc.val.avg, group = rm, col = rm)) +
  geom_point() +
  scale_color_manual(values = c(
    "#999999", 
    "#E69F00", 
    "#56B4E9", 
    'salmon')) +
  geom_line(aes(col = rm)) +
  theme_classic()

ggsave(
  'output/figures/best_model_settings_plot.jpg',
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
      'hd_model_results.rds'))
