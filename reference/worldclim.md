# WorldClim climate data

Download climate data from WorldClim version 2.1. See Details for
variables and units.

## Usage

``` r
worldclim_global(var, res, path, version="2.1", ...)
worldclim_country(country, var, path, version="2.1", ...)
worldclim_tile(var, lon, lat, path, version="2.1", ...)
```

## Arguments

- var:

  character. Valid variables names are "tmin", "tmax", "tavg", "prec",
  "wind", "vapr", and "bio"

- res:

  numeric. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a
  degree)

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- country:

  character. Country name or code

- lon:

  numeric. Longitude

- lat:

  numeric. Latitude

- version:

  character or numeric. WorldClim version number. Only "2.1" supported
  at the moment

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## Details

These are the WorldClim monthly average climate data.

|     |              |                                   |              |
|-----|--------------|-----------------------------------|--------------|
|     | **Variable** | **Description**                   | **Unit**     |
|     | `tmin`       | minimum temperature               | °C           |
|     | `tmax`       | maximum temperature               | °C           |
|     | `tavg`       | average temperature               | °C           |
|     | `prec`       | total precipitation               | mm           |
|     | `srad`       | incident solar radiation          | kJ m⁻² day⁻¹ |
|     | `wind`       | wind speed (2 m above the ground) | m s⁻¹        |
|     | `vapr`       | vapor pressure                    | kPa          |

## See also

<https://www.worldclim.org/>

## Examples

``` r
lux <- worldclim_country("Luxembourg", var="tmin", path=tempdir())
#> The geodata server is temporary out of service for maintenance. It should be back on 20 December. 
```
