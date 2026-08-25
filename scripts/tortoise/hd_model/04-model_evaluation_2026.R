# Mojave desert tortoise distribution
# Modeling
# Script 04
# SDM evaluation Mojave desert tortoise

# setup -------------------------------------------------------------------

rm(list = ls())

library(modEvA)
library(ecospat)
library(tidyverse)

# paths -------------------------------------------------------------------

models <- 'output/models/tortoise/files/'

# data' -------------------------------------------------------------------

model_data <-
  read_rds(
    paste0(
      models, 
      'hd_model_results.rds')) %>% 
  pluck('predictions')

# THRESHOLD-DEPENDENT MEASURES (classification) ####

s <-
  optiThresh(
    obs = model_data$presence, 
    pred = model_data$prediction, 
    pch = 20, 
    cex = 0.1, 
    measures = c(
      'CCR', 
      'Sensitivity', 
      'Specificity', 
      'Precision', 
      'kappa', 
      'TSS'))
s

# threshMeasures ----------------------------------------------------------

par(mar = c(6, 3, 2, 1))

measures <-
  threshMeasures(
    obs = model_data$presence, 
    pred = model_data$prediction, 
  thresh = 'maxSSS', 
  main = 'MXT', 
  measures = c(
    'CCR', 
    'Sensitivity', 
    'Specificity', 
    'Precision',
    'kappa', 
    'TSS'))

prev <- measures$Prevalence
mtss  <- measures$Threshold

eval <- 
  measures$ThreshMeasures %>% 
  as.data.frame() %>%
  rownames_to_column(var = 'Parameter') %>% 
  as_tibble() %>% 
  pivot_wider(
    names_from = Parameter, 
    values_from = Value)

# Boyce index -------------------------------------------------------------

# training BI

boyce_index <- 
  ecospat.boyce(
  fit = model_data$prediction, 
  obs = model_data$prediction[model_data$presence == 1])

boyce_index_table <- 
  tibble(
    boyce_index$F.ratio, 
    boyce_index$HS) %>% 
  rename(
    Fratio = 'boyce_index$F.ratio', HS = 'boyce_index$HS')

bi <- 
  boyce_index_table %>% 
  ggplot(aes(HS, Fratio)) +
  geom_line(col = 'darkblue') +
  geom_smooth(col = 'orange') +
  theme_classic()

# evaluation table --------------------------------------------------------

tibble(
  prev, 
  mtss, 
  eval,
  boyce = boyce_index$cor) %>% 
  rename_all(tolower) %>%
  mutate(
    across(prev:boyce,
           ~.x %>% round(2)))
