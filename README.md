# Assessing habitat suitability and risk for the Mojave Desert tortoise in the rear edge of its distribution

## Repository

This is a repository of the materials used for the assessment of the Mojave Desert tortoise _Gopherus agassizii_ in the rear edge of its distribution. Here we have included the code needed to run the species distribution models and assessment analyses. Follow the instructions on the main manuscript and Appendix S! and S2 to download the raw/unprocessed data.

### Repository data

Project structure: The project was built in R V.4.5.0 through RStudio. To facilitate reproducibility, the project uses the R package renv V 1.1.4. Once the project has been forked, use the option renv::restore() to restore the R packages. If errors occur due to computer incompatibility, the lockfile can be rebuild using renv::init(). For more instructions refer to the [renv website](https://rstudio.github.io/renv/articles/renv.html)

### Ocurrence data files: 

1) tortoise_model_data: We have provided a simulation pseudo-ocurrence dataset (n = 200) within the study area to ensure code reproducibility. Tortoise occurrence data are not currently available or have limited availability owing to restrictions. Please contact jeffrey_lovich@usgs.gov for more information.

2) grasses_model_data: Clean occurrence and environmental data from three invasive grasses (_Bromus rubens, Schismus arabicus, Schismus barbatus_) derived from [Calflora](https://www.calflora.org), [the Global Biodiversity Information Facility (GBIF)](https://www.gbif.org), and [iNaturalist](https://www.inaturalist.org).
   
### raster files

We have included processed raster files containing the environmental information after removing highly correlated predictors (threshold  = 0.70) and predictors with low contribution to a fisrt model run. This process is needed due to the large size of raster files. 

For the Mojave desert tortoise and each grass species we provide a raster file named 'species_envs_reduced.tif' within the 'rasters' folder




