# Sacks crop calendar data

Download Sacks crop calendar data. The crops available are returned by
`sacksCrops`

## Usage

``` r
crop_calendar_sacks(crop="", path, ...)

sacksCrops()
```

## Arguments

- crop:

  character. Crop name. See `sacksCrops` for valid names

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## References

Sacks, W.J., D. Deryng, J.A. Foley, and N. Ramankutty, 2010. Crop
planting dates: an analysis of global patterns. Global Ecology and
Biogeography 19: 607-620. doi:10.1111/j.1466-8238.2010.00551.x.

## See also

<https://sage.nelson.wisc.edu/data-and-models/datasets/crop-calendar-dataset/>

## Examples

``` r
# \donttest{
# download may take > 5s
cas <- crop_calendar_sacks("cassava", path=tempdir())
#> The geodata server is temporary out of service for maintenance. It should be back on 20 December. 
# }
```
