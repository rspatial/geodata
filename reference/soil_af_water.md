# Soil data for water balance computation (Africa only)

Download physical soil properties data for Africa that can be used in
water balance computation. The values are for a soil depth of 0 to 30
cm. The spatial resolution is 30 arc-seconds (about 1 km2), aggregated
from the original 250m resolution.

For other properties see
[`soil_af`](https://rspatial.github.io/geodata/reference/soil_af.md),
[`soil_af_elements`](https://rspatial.github.io/geodata/reference/soil_af_elements.md),
[`soil_af_isda`](https://rspatial.github.io/geodata/reference/soil_af_isda.md).

For more info, see
https://www.isric.org/projects/soil-property-maps-africa-250-m-resolution

The data have a CC-BY 4.0 NC license

## Usage

``` r
soil_af_water(var, depth = "30cm", path, ...)
```

## Arguments

- var:

  character. Variables name such as "awcpf23" or "pwp". See Details

- depth:

  character. Either "30cm" or "erzd" (the effective rooting zone depth
  of maize)

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## Details

|     |            |                                                                        |              |
|-----|------------|------------------------------------------------------------------------|--------------|
|     | **var**    | **description**                                                        | **unit**     |
|     | awcpf23    | Available water capacity of the fine earth at field capacity (pF 2.3)  | volumetric % |
|     | pwp        | Moisture content of the fine earth at permanent wilting point (pF 4.2) | volumetric % |
|     | tetas      | Moisture content of the fine earth at saturation                       | volumetric % |
|     | tawcpf23   | Absolute total available water capacity                                | cm?          |
|     | tawcpf23mm | Absolute total available water capacity in mm                          | mm           |
|     | erzd       | Effective root zone depth (for maize)                                  | cm           |

## See also

[`soil_af_elements`](https://rspatial.github.io/geodata/reference/soil_af_elements.md),
[`soil_af_isda`](https://rspatial.github.io/geodata/reference/soil_af_isda.md),
[`soil_world`](https://rspatial.github.io/geodata/reference/soil_grids.md)

## Examples

``` r
# \donttest{
# this downloads a large file
tetaS <- soil_af_water(var="tetas", depth="erzd", path=tempdir())
#> Cached as: /tmp/Rtmp0MapG8/soil_af/af_erzd_tetas.tif
# }
```
