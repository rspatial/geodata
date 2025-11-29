# Elevation

Elevation data for any country. The main data source is Shuttle Radar
Topography Mission (SRTM) , specifically the hole-filled CGIAR-SRTM (90
m resolution) from https://srtm.csi.cgiar.org/. These data are only
available for latitudes between -60 and 60.

The 1 km (30 arc seconds) data were aggregated from SRTM 90 m resolution
data and supplemented with the GTOP30 data for high latitudes (\>60
degrees).

## Usage

``` r
elevation_3s(lon, lat, path, ...)
elevation_30s(country, path, mask=TRUE, subs="", ...)
elevation_global(res, path, ...)
```

## Arguments

- lon:

  numeric. Longitude

- lat:

  numeric. Latitude

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- country:

  character. Country name or code

- mask:

  logical. set grid cells outside of the country boundaries to NA

- subs:

  character

- res:

  numeric. Valid resolutions are 10, 5, 2.5, and 0.5 (minutes of a
  degree)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## Examples

``` r
be <- elevation_30s(country="BEL", path=tempdir() )
```
