# CMIP6 climate model data

Download downscaled and calibrated CMIP6 climate data for projected
future climates. Either for the entire world or for a 30 degrees tile.
For more information see <https://www.worldclim.org/>

## Usage

``` r
cmip6_world(model, ssp, time, var, res, path, ...)

cmip6_tile(lon, lat, model, ssp, time, var, path, ...)
```

## Arguments

- model:

  character. Climate model abbreviation. One of "ACCESS-CM2",
  "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5",
  "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR",
  "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0",
  "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL",
  "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6",
  "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL"

- ssp:

  character. A valid Shared Socio-economic Pathway code: "126", "245",
  "370" or "585"

- time:

  character. A valid time period. One of "2021-2040", "2041-2060", or
  "2061-2080"

- var:

  character. Variables name. One of "tmin", "tmax", "prec" and "bioc"

- res:

  numeric. Spatial resolution. One of 10, 5, 2.5 and 0.5 (minutes of a
  degree). Silently ignored if provided to cmip6_tile(), available only
  at res=0.5

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

- lon:

  numeric. Longitude

- lat:

  numeric. Latitude

## Value

SpatRaster

## See also

[`vrt`](https://rspatial.github.io/terra/reference/vrt.html) to combine
tiles

## Examples

``` r
# \donttest{
# download of large files takes a while
tmin10 <- cmip6_world("CNRM-CM6-1", "585", "2061-2080", 
          var="tmin", res=10, path=tempdir())
# }
```
