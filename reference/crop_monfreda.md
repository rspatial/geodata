# Monfreda crop data

Monfreda global crop data (area, yield) for 175 crops.

Data may be freely used for research, study, or teaching, but must be
cited appropriately (see below). Re-release of the data, or
incorporation of the data into a commercial product, is allowed only
with explicit permission.

## Usage

``` r
monfredaCrops()
crop_monfreda(crop="", var="area_ha", path, ...)
```

## Arguments

- crop:

  character. Crop name(s). See `monfredaCrops` for valid names

- var:

  character. The variable(s) of interest. Choose from "area_ha" (crop
  area in ha per cell), "area_f" (crop area as a fraction of each cell),
  "area_q" (quality of the crop area data), "yield" (crop yield in
  Mg/ha), "yield_q" (quality of the yield data), "prod" (production per
  grid cell in Mg), or "all"

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## References

Monfreda, C., N. Ramankutty, and J. A. Foley (2008), Farming the planet:
2. Geographic distribution of crop areas, yields, physiological types,
and net primary production in the year 2000, Global Biogeochem. Cycles,
22, GB1022, doi:10.1029/2007GB002947.

## See also

<http://www.earthstat.org/harvested-area-yield-175-crops/>

## Examples

``` r
# \donttest{
# download may take > 5s
mcas <- crop_monfreda("cassava", path=tempdir())
#> download failed
mcas
#> NULL
names(mcas)
#> NULL
# }
```
