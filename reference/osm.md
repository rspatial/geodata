# OpenStreetMap data

Get OpenStreetMap (OSM) data

## Usage

``` r
osm(country, var, path, proxy=FALSE, ...)
```

## Arguments

- country:

  character. Three-letter ISO code or full country name

- var:

  character. Currently it can be one of "places", "highways", or
  "railway"

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- proxy:

  logical. Return a SpatVectorProxy?

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatVector

## Details

License: Open Data Commons Open Database License (ODbL).

See <https://www.openstreetmap.org/copyright>

## Examples

``` r
aruba <- osm(country="Aruba", "places", path=tempdir())
#> The geodata server is temporary out of service for maintenance. It should be back on 20 December. 
```
