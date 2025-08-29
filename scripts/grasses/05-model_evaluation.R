# Mojave desert tortoise distribution
# SDM evaluation. Grasses

# setup -------------------------------------------------------------------

rm(list = ls())

library(modEvA)
library(ecospat)
library(tidyverse)

# folders -----------------------------------------------------------------

models <- 'output/models/grasses/files/'
tables <- 'output/tables/'

# species data ------------------------------------------------------------

my_species <- 
  c(
    "Bromus_rubens" , 
    "Schismus_arabicus",
    "Schismus_barbatus")

# data --------------------------------------------------------------------

model_data <-
  list.files(
    models,
    pattern = 'model_results.rds',
    full.names = T) %>% 
  map(~ .x %>% 
  read_rds() %>% 
    pluck('predictions')) %>% 
    set_names(my_species)
    
# THRESHOLD-DEPENDENT MEASURES (classification) ####

sp <- "Schismus_arabicus"
#sp <- "Schismus_barbatus"
#sp <- "Bromus_rubens"

s <-
  optiThresh(
  obs = model_data[[sp]]$presence, 
  pred = model_data[[sp]]$prediction, 
  pch = 20, 
  cex = 0.1, 
  measures = c(
    "CCR", 
    "Sensitivity", 
    "Specificity", 
    "Precision", 
    "kappa", 
    "TSS"))

# threshMeasures ----------------------------------------------------------

par(mar = c(6, 3, 2, 1))

measures <-
  threshMeasures(
  obs = model_data[[sp]]$presence, 
  pred = model_data[[sp]]$prediction,   
  thresh = 'maxSSS', 
  main = "MXT", 
  measures = c(
    "CCR", 
    "Sensitivity", 
    "Specificity", 
    "Precision","kappa", 
    "TSS"))

prev <- measures$Prevalence
mtss  <- measures$Threshold

eval <- 
  measures$ThreshMeasures %>% 
  as.data.frame() %>%
  rownames_to_column(var = "Parameter") %>% 
  as_tibble() %>% 
  pivot_wider(names_from = Parameter, values_from = Value)

# Boyce index -------------------------------------------------------------

data <- 
  model_data[[sp]] %>% 
  filter((!is.na(prediction)))

boyce_index <- 
  ecospat.boyce(
  fit = data$prediction, 
  obs = data$prediction[data$presence == 1])

boyce_index_table <- 
  tibble(
    boyce_index$F.ratio, 
    boyce_index$HS) %>% 
  rename(
    Fratio = 'boyce_index$F.ratio', HS = 'boyce_index$HS')

boyce_index_table %>% 
  ggplot(aes(HS, Fratio)) +
  geom_point(col = 'darkblue') +
  geom_smooth(col = 'orange') +
  theme_classic()

# eval table --------------------------------------------------------------

tibble(
  species = sp,
  prev, 
  mtss, 
  eval,
  boyce = boyce_index$cor) %>% 
  rename_all(., .funs = tolower) %>% 
  write_csv(
    paste0(
      tables, 
      sp, 
      '_eval_table.csv'))
