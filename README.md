# Assessing habitat suitability and risk for the Mojave Desert tortoise in the rear edge of its distribution

Authors and Affiliations

Hector Zumbado-Ulate¹
Luis E. Barrios²
Danelle A. Baronia²
G. Darrel Jenerette²
Lynn C. Sweet²

¹ Escuela de Ciencias Biológicas, Universidad Nacional, Heredia, Costa Rica
² Center for Conservation Biology, University of California, Riverside, CA 92521, USA

## Repository

Overview

This repository contains the scripts, data and supporting materials used for the assessment of the Mojave Desert tortoise *Gopherus agassizii* in the rear edge of its distribution. The archive is intended to ensure long-term preservation, transparency, and reproducibility of all analyses. All analyses were conducted in R and include species distribution modeling, spatial analyses, and projections of future range shifts for the Mojave Desert tortoise in the rear edge of its distribution in southern California. Follow the instructions on the main manuscript and Appendix S1 and S2 to download the raw/unprocessed data and other supporting materials from a Zenodo complementary repository (10.5281/zenodo.16110023).

### Contents of This Archive

The GitHub repository includes:

An R Project file (.Rproj)

A renv.lock file specifying exact package versions

Structured scripts following the analytical workflow

Shapefiles

### Project structure

The project was built in R V.4.5.0 through RStudio. The code is organized as a self-contained R Project and the computational environment is administered using the renv package V 1.1.5. This ensures that package dependencies and versions are recorded and can be restored consistently across systems.
 Once the project has been cloned, use the option renv::restore() to restore the R packages. If errors occur due to computer incompatibility, the lockfile can be rebuilt using renv::init(). For more instructions refer to the [renv website](https://rstudio.github.io/renv/articles/renv.html)

### Key R Packages

Core packages used in the analyses include:

`ENMeval` — evaluation and tuning of species distribution models

`ecospat` — niche dynamics and spatial ecology analyses

`tidyverse` — data manipulation and visualization

`terra` — spatial raster processing

`sf` — vector-based spatial analysis

`ggplot2` — figure generation

Exact package versions are recorded in the renv.lock file and can be restored automatically.

### Occurrence data files:

1)  tortoise_model_data: To ensure code reproducibility, we have provided an ocurrence dataset (n = 451) with obscured coordinates. Precise tortoise occurrences are not currently available or have limited availability owing to restrictions. Please contact [jeffrey_lovich\@usgs.gov](mailto:jeffrey_lovich@usgs.gov){.email} for more information.

2)  grasses_model_data: Clean occurrence and environmental data from three invasive grasses (*Bromus rubens, Schismus arabicus, Schismus barbatus*) derived from [Calflora](https://www.calflora.org), [the Global Biodiversity Information Facility (GBIF)](https://www.gbif.org), and [iNaturalist](https://www.inaturalist.org).

### Raster files

Processed raster files containing the environmental information after removing highly correlated predictors (threshold = 0.70) and predictors with low contribution to a first model run can be downloaded from the [spatial data repository](10.5281/zenodo.16110023). This second repository for spatial data is needed due to the large size of the raster files. 

For the Mojave Desert tortoise and each grass species, we have provided raster files named 'species_envs_reduced.tif' that must be placed within the 'rasters/present' folder. To model future suitability for the Mojave Desert tortoise please place all raster files starting with 'hadgem' within the 'rasters/future' folder.

### Present and Future Climate Scenarios

All present and future climate datasets (19 bioclimatic variables) were obtained from WorldClim version 2.1, at a spatial resolution of 30 arc-seconds (~1 km at the equator):

present: https://www.worldclim.org/data/worldclim21.html
future: https://www.worldclim.org/data/cmip6/cmip6_clim30s.html

Future climate projections were derived from CMIP6 downscaled climate datasets and were used to model potential future shifts in the Mojave Desert tortoise distribution.

Specifically:

Shared Socio-economic Pathways (SSPs):

SSP1–2.6 (SSP126)

SSP2–4.5 (SSP245)

SSP5–8.5 (SSP585)

Time period: 2041–2060, 2061–2080, 2081–2100

Global Circulation Models (GCMs):

HadGEM3-GC31-LL

Details on variable selection, processing, and aggregation are provided within the metadata files and associated R scripts.

### Reproducibility

To reproduce the analyses:

1) Download and extract the Zenodo archive.

2) Clone or download the GitHub repository linked above.

3) Open the R Project file (.Rproj) in R (version 4.5.0).

4) Restore the package environment using `renv::restore()`

5) Update file paths in the scripts if necessary.

6) Run scripts sequentially following their numeric prefixes.

### Data Usage Notes

Data are provided for research and academic use.

Users should consult metadata files for data sources, preprocessing steps, and known limitations.

Derived products should be cited appropriately.

### License

Data: Creative Commons Attribution 4.0 International

Code:  GNU General Public License 3.0

License details are provided within the Zenodo record and the associated GitHub repository.

### Citation

If you use these data or materials, please cite both the Zenodo archive and the associated manuscript:

Zumbado-Ulate, H., Barrios, L. E., Baronia, D. A., Jenerette, G. D., & Sweet, L. C.
Biogeographic patterns of angiosperm richness and projected range shifts in desert ecosystems of southern California.
American Journal of Botany.
Zenodo DOI: 10.5281/zenodo.18167096

### Contact

Corresponding author: Hector Zumbado-Ulate
Affiliation: Escuela de Ciencias Biológicas, Universidad Nacional, Costa Rica
Email: zumbadohector@gmail.com

