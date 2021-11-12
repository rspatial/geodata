
# geodata

<!-- badges: start -->
<!-- badges: end -->

**geodata** is an R package for downloading geographic data.
This package facilitates access to climate, elevation, soil, species occurrence, and administrative boundary data, and is a successor of the `getData()` function from the **raster** package.

## Installation

You can install the released version of **geodata** from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("geodata")
```

You can install the development version of **geodata** from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rspatial/geodata")
```

## Data sources

|Functions                                                       |Description                                                                                                     |
|:---------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------|
|`cmip6_world()`                                                 |Downscaled and calibrated CMIP6 climate data for projected future climates                            |
|`country_codes()`                                               |Get country codes for all countries in the world                                                                |
|`crop_calendar_sacks()`                                         |Sachs crop calendar data                                                                                        |
|`crop_monfreda()`                                               |Monfreda crop data (area, yield)                                                                                             |
|`crop_spam()`                                                   |SPAM crop data (area, yield, value)                                                                                                   |
|`elevation_3s()`, `elevation_30s()`, `elevation_global()`       |Get elevation data for any country in the world                                                                 |
|`gadm()`, `world()`                                             |Get administrative boundaries for any country in the world                                                      |
|`cropland_africa()`                                             |Download cropland extent data for Africa                                                                        |
|`population()`                                                  |Download population density data                                                                                |
|`soil_af_elements()`                                            |Connect to or download chemical soil element concentration (for the 0-30 cm topsoil) data for Africa            |
|`soil_af_isda()`                                                |Soil data for Africa derived from the iDSA data set                                                    |
|`soil_af()`                                                     |Chemical soil properties data for Africa for different soil depths                                     |
|`soil_world_vsi()`                                              |Virtually connect to the global soilgrids data                                                                  |
|`soil_world()`                                                  |Global soils data                                                                                      |
|`sp_occurrence()`                                               |Species occurrence records from the Global Biodiversity Information Facility |
|`worldclim_global()`, `worldclim_country()`, `worldclim_tile()` |WorldClim glocal climate data                                                                                         |
