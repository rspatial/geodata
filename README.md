
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
|`cmip6_world()`                                                 |Downscaled and calibrated CMIP6 projected future climate data                            |
|`country_codes()`                                               |Country codes                                                                |
|`crop_calendar_sacks()`                  |Crop calendar data by [Sacks et al](https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/)     |
|`crop_monfreda()`                        |Crop area and yield data for 175 crops by [Monfreda et al.](http:://www.earthstat.org/harvested-area-yield-175-crops/)    |
|`crop_spam()`                                                   |[MapSPAM](https://www.mapspam.info/data/) crop data (area, yield, value)     |
|`elevation_3s()`, `elevation_30s()`, `elevation_global()`       |Elevation data                                                                  |
|`gadm()`, `world()`                                             |Administrative boundaries for any country, or the entire world from [GADM](https://gadm.org)      |
|`osm()`                                                         |OpenStreetMap data by country (places and roads) |
|`cropland_africa()`                                             |Cropland extent data for Africa by [QED](https://about.maps.qed.ai/)                      |
|`population()`                   |Population density [Gridded Population of the World](http://sedac.ciesin.columbia.edu/data/collection/gpw-v4/documentation)    |
|`soil_af_elements()`                                            |Connect to or download chemical soil element concentration (for the 0-30 cm topsoil) data for Africa  |
|`soil_af_isda()`                |Soil data for Africa derived from the [iDSA data set](https://envirometrix.nl/isdasoil-open-soil-data-for-africa/)   |
|`soil_af()`                                                     |Chemical soil properties data for Africa for different soil depths                                     |
|`soil_world_vsi()`                                              |Virtually connect to the global [SoilGrids](https://www.isric.org/explore/soilgrids) data        |
|`soil_world()`                                                  |Global soils data from [SoilGrids](https://www.isric.org/explore/soilgrids)          |
|`sp_occurrence()`                                               |Species occurrence records from the [Global Biodiversity Information Facility](https::/www.gbif.org) |
|`travel_time()`                   |Travel time to and from cities and ports by [Nelson et al.](https://www.nature.com/articles/s41597-019-0265-5)   |
|`worldclim_global()`, `worldclim_country()`, `worldclim_tile()` |[WorldClim](https://worldclim.org) glocal climate data     |

