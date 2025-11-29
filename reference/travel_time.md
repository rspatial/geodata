# Travel time to a city or port

Download global travel time to a city or port data on rasters at a 30
arc-seconds (about 1 km2) resolution.

## Usage

``` r
travel_time(to="city", size=1, up=FALSE, path, ...)
```

## Arguments

- to:

  character. "city" or "port"

- size:

  positive integer indicating the size of the city or port. Can be
  between 1 and 9 if `to="city"` or between 1 and 5 if `to="port"`. See
  Details

- up:

  logical. If `TRUE` the travel time to a city of the size chosen **or
  larger** is returned

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## Details

Description of the the `size` argument.

**`to="city"`**

|     |          |                         |
|-----|----------|-------------------------|
|     | **size** | **Inhabitants**         |
|     | 1        | 5,000,000 to 50,000,000 |
|     | 2        | 1,000,000 to 5,000,000  |
|     | 3        | 500,000 to 1,000,000    |
|     | 4        | 200,000 to 500,000      |
|     | 5        | 100,000 to 200,000      |
|     | 6        | 50,000 to 100,000       |
|     | 7        | 20,000 to 50,000        |
|     | 8        | 10,000 to 20,000        |
|     | 9        | 5,000 to 10,000         |

**`to="port"`**

|     |          |                 |                     |
|-----|----------|-----------------|---------------------|
|     | **size** | **Description** | **Number of ports** |
|     | 1        | Large           | 160                 |
|     | 2        | Medium          | 361                 |
|     | 3        | Small           | 990                 |
|     | 4        | Very small      | 2,153               |
|     | 5        | Any             | 3,778               |

## References

Nelson, A., D.J. Weiss, J. van Etten, A. Cattaneo, T.S. McMenomy & J.
Koo, 2019. A suite of global accessibility indicators. Scientific Data
6: 266. doi:10.1038/s41597-019-0265-5

Version 3 (2019-05-15) from
<https://figshare.com/articles/dataset/Travel_time_to_cities_and_ports_in_the_year_2015/7638134/3>
