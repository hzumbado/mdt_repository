# Setup -------------------------------------------------------------------

rm(list = ls())

library(CoordinateCleaner)
library(humboldt)
library(tidyverse)

# species -----------------------------------------------------------------

my_species <- 
  c(
    "Bromus_rubens" , 
    "Schismus_arabicus",
    "Schismus_barbatus")

# full dataset ------------------------------------------------------------

clean_files <- 
  list.files(
    'data/raw/', 
    pattern = 'grasses_clean.csv', 
    full.names = T) |> 
  map_dfr(~ .x |>   
             read_csv())

# rebuild dataset ---------------------------------------------------------

occs_rarefaction <- 
  function(data, rarefy.dist, rarefy.units) {
    data_rar <- 
      data %>%
      humboldt.occ.rarefy(
        colxy = c(
          "x", 
          "y"), 
        rarefy.dist = rarefy.dist, 
        rarefy.units = rarefy.units) %>% 
      as_tibble()
    return(data_rar)
  }

clean_files_full <-
  clean_files |>  
  group_split(species) |> 
  set_names(my_species) |>  
  map(
    ~ .x |> 
  occs_rarefaction(3, 'km')) 

clean_files_full |> 
  bind_rows() |>  
  write_csv(
    'data/processed/grasses_dataset.csv')
