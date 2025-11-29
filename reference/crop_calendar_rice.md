# crop calendar for rice

Get crop calendar and production data for rice

## Usage

``` r
rice_calendar(path, ...)
```

## Arguments

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatVectorCollection

## References

Laborte, A.G.; Gutierrez, M.A.; Balanza, J.G.; Saito, K.; Zwart, S.J.;
Boschetti, M.; Murty, MVR; Villano, L.; Aunario, J.K.; Reinke, R.; Koo,
J.; Hijmans, R.J.; Nelson, A., 2017. RiceAtlas, a spatial database of
global rice calendars and production. Scientific Data 4: 170074
[doi:10.1038/sdata.2017.74](https://doi.org/10.1038/sdata.2017.74)

## Examples

``` r
# \donttest{
# first time api call takes a while
rice <- rice_calendar(path=tempdir())
#> Dataset or resource not reachable.
#> Status code:  202
#> something went wrong
cal <- rice[1]
# }
```
