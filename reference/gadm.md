# Administrative boundaries

Get administrative boundaries for any country in the world. Data are
read from files that are downloaded if necessary.

## Usage

``` r
gadm(country, level=1, path, version="latest", resolution=1, ...)
```

## Arguments

- country:

  character. Three-letter ISO code or full country name. If you provide
  multiple names they are all downloaded and `rbind`-ed together

- level:

  numeric. The level of administrative subdivision requested. (starting
  with 0 for country, then 1 for the first level of subdivision)

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- version:

  character. Either "latest" or GADM version number (can be "3.6", "4.0"
  or "4.1")

- resolution:

  integer indicating the level of detail. Only for version 4.1. It
  should be either 1 (high) or 2 (low)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## See also

[`world`](https://rspatial.github.io/geodata/reference/world.md)

## Value

SpatVector

## Details

The data are from <https://gadm.org>

## Examples

``` r
bel <- gadm(country="BEL", level=1, path=tempdir())
#> The geodata server is temporary out of service for maintenance. It should be back on 20 December. 
```
