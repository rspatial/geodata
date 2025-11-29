# Administrative boundaries

Get the borders for all the countries in the world. Data are read from
files that are downloaded if necessary.

## Usage

``` r
world(resolution=5, level=0, path, version="latest", ...)
```

## Arguments

- resolution:

  integer between 1 and 5 indicating the level of detail. 1 is high 5 is
  low

- level:

  numeric. The level of administrative subdivision requested. (starting
  with 0 for country, then 1 for the first level of subdivision). Only
  level 0 is currently available

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- version:

  character. Only "3.6" is currently supported

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatVector

## See also

[`gadm`](https://rspatial.github.io/geodata/reference/gadm.md)

## Details

The data are from <https://gadm.org>

## Examples

``` r
w <- world(path=tempdir())
```
