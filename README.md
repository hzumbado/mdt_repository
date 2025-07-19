# Assessing habitat suitability and risk for the Mojave Desert tortoise in the rear edge of its distribution

## Repository

This is a repository of the materials used for the assessment of the Mojave Desert tortoise *Gopherus agassizii* in the rear edge of its distribution. Here, we have included the code needed to run the species distribution models and assessment analyses. Follow the instructions on the main manuscript and Appendix S! and S2 to download the raw/unprocessed data.

### Repository data

Project structure: The project was built in R V.4.5.0 through RStudio. To facilitate reproducibility, the project uses the R package renv V 1.1.4. Once the project has been forked, use the option renv::restore() to restore the R packages. If errors occur due to computer incompatibility, the lockfile can be rebuilt using renv::init(). For more instructions refer to the [renv website](https://rstudio.github.io/renv/articles/renv.html)

### Occurrence data files:

1)  tortoise_model_data: We have provided a simulation pseudo-ocurrence dataset (n = 200) within the study area to ensure code reproducibility. Tortoise occurrence data are not currently available or have limited availability owing to restrictions. Please contact [jeffrey_lovich\@usgs.gov](mailto:jeffrey_lovich@usgs.gov){.email} for more information.

2)  grasses_model_data: Clean occurrence and environmental data from three invasive grasses (*Bromus rubens, Schismus arabicus, Schismus barbatus*) derived from [Calflora](https://www.calflora.org), [the Global Biodiversity Information Facility (GBIF)](https://www.gbif.org), and [iNaturalist](https://www.inaturalist.org).

### Raster files

Processed raster files containing the environmental information after removing highly correlated predictors (threshold = 0.70) and predictors with low contribution to a first model run can be downloaded from the [spatial data repository](10.5281/zenodo.16110023). This second repository for spatial data is needed due to the large size of the raster files. 

For the Mojave Desert tortoise and each grass species, we have provided raster files named 'species_envs_reduced.tif' that must be placed within the 'rasters/present' folder. To model future suitability for the Mojave Desert tortoise please place all raster files starting with 'hadgem' within the 'rasters/future' folder.
