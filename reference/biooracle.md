# Marine data

Marine data from Bio-Oracle

## Usage

``` r
bio_oracle(path, var, stat, benthic=FALSE, 
    depth="Mean", time="Present", rcp, ...)
```

## Arguments

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- var:

  character. Variable of interest. One of 'Calcite', 'Chlorophyll',
  'Cloud.cover', 'Current.Velocity', 'Diffuse.attenuation',
  'Dissolved.oxygen', 'Ice.cover', 'Ice.thickness', 'Iron',
  'Light.bottom', 'Nitrate', 'Par', 'pH', 'Phosphate', 'Phytoplankton',
  'Primary.productivity', 'Salinity', 'Silicate', 'Temperature'

- stat:

  character. Statistic of interest. One of 'Lt.max', 'Lt.min', 'Max',
  'Mean', 'Min', 'Range'. It should be "" if `var` is "pH"

- benthic:

  logical. If `FALSE` surface data are returned

- depth:

  character. Either "Min", "Mean", or "Max". Only relevant if `benthic`
  is `TRUE`

- time:

  character. Either "Present", "2050" or "2100"

- rcp:

  character. Either "26", "45", "60", or "85"

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## References

Assis, J., Tyberghein, L., Bosh, S., Verbruggen, H., Serrão, E.A., & De
Clerck, O. (2017). Bio-ORACLE v2.0: Extending marine data layers for
bioclimatic modelling. Global Ecology and Biogeography 27: 277-284.

## See also

<https://bio-oracle.org/>

## Examples

``` r
# \donttest{
# this is a large download
x <- bio_oracle(path=tempdir(), "Salinity", "Max", 
    benthic=TRUE, depth="Mean", time="Present")
#> Cached as: /tmp/Rtmp0MapG8/bio-oracle/Present.Benthic.Mean.Depth.Salinity.Max.tif.zip
# }
```
