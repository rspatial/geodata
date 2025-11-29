# Landcover data

Landcover data at 30-seconds spatial resolution for (most of) the world.
Values are the fraction of a landcover class in each cell. The values
are derived from the ESA WorldCover data set at 0.3-seconds resolution.
(License CC BY 4.0). See <https://esa-worldcover.org/en> for more
information.

## Usage

``` r
landcover(var, path, ...)
```

## Arguments

- var:

  character. One of "trees", "grassland", "shrubs", "cropland", "built",
  "bare", "snow", "water", "wetland", "mangroves", "moss"

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## See also

`landcover`

## References

Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., Souverijns, N.,
Brockmann, C., Quast, R., Wevers, J., Grosu, A., Paccini, A., Vergnaud,
S., Cartus, O., Santoro, M., Fritz, S., Georgieva, I., Lesiv, M.,
Carter, S., Herold, M., Li, Linlin, Tsendbazar, N.E., Ramoino, F.,
Arino, O., 2021. ESA WorldCover 10 m 2020 v100.
doi:10.5281/zenodo.5571936.
