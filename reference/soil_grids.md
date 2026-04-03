# Global soil data

Download global soils data. The data are derived from the SoilGRIDS
database. The data were aggregated and transformed to a
longitude/latitude coordinate reference system with 30-second spatial
resolution.

See https://www.isric.org/explore/soilgrids for more info.

data license: CC-BY 4.0

Note that currently only the "mean" values for up to 60cm depth are
available.

## Usage

``` r
soil_world(var, depth, stat="mean", name="", path, vsi=FALSE, ...)
```

## Arguments

- var:

  character. Variables name. One of: "bdod", "cfvo", "clay", "nitrogen",
  "ocd", "ocs", "phh2o", "sand", "silt", "soc", "wrb". See Details

- depth:

  numeric. One of `5, 15, 30, 60, 100, 200`. This is shorthand for the
  following depth ranges: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm.
  Ignored if `var="wrb"`

- stat:

  character. One of "mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95".
  Ignored if `var="wrb"`

- name:

  character. One of "Acrisols", "Albeluvisols", "Alisols", "Andosols",
  "Arenosols", "Calcisols", "Cambisols", "Chernozems", "Cryosols",
  "Durisols", "Ferralsols", "Fluvisols", "Gleysols", "Gypsisols",
  "Histosols", "Kastanozems", "Leptosols", "Lixisols", "Luvisols",
  "Nitisols", "Phaeozems", "Planosols", "Plinthosols", "Podzols",
  "Regosols", "Solonchaks", "Solonetz", "Stagnosols", "Umbrisols",
  "Vertisols". Only used when `var="wrb"`

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- vsi:

  logical. If `TRUE`, a virtual connection to the file is used in stead
  of a download. This allows downloading a small area only by
  subsequently using
  [`crop`](https://rspatial.github.io/terra/reference/crop.html) or
  [`window`](https://rspatial.github.io/terra/reference/window.html)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## Details

|     |            |                                             |              |
|-----|------------|---------------------------------------------|--------------|
|     | **var**    | **description**                             | **unit**     |
|     | `bdod`     | Bulk density of the fine earth fraction     | kg dm⁻³      |
|     | `cec`      | Cation Exchange Capacity of the soil        | cmol(+) kg⁻¹ |
|     | `cfvo`     | Vol. fraction of coarse fragments (\> 2 mm) | %            |
|     | `nitrogen` | Total nitrogen (N)                          | g kg⁻¹       |
|     | `phh2o`    | pH (H₂O)                                    | \-           |
|     | `sand`     | Sand (\> 0.05 mm) in fine earth             | %            |
|     | `silt`     | Silt (0.002-0.05 mm) in fine earth          | %            |
|     | `clay`     | Clay (\< 0.002 mm) in fine earth            | %            |
|     | `soc`      | Soil organic carbon in fine earth           | g kg⁻¹       |
|     | `ocd`      | Organic carbon density                      | kg m⁻³       |
|     | `ocs`      | Organic carbon stocks                       | kg m⁻²       |

## References

Poggio L., de Sousa L.M., Batjes N.H., Heuvelink G.B.M., Kempen B.,
Ribeiro E., Rossiter D., 2021. SoilGrids 2.0: producing soil information
for the globe with quantified spatial uncertainty. Soil 7:217-240, 2021.
doi:10.5194/soil-7-217-2021

## See also

For virtual access to the original data:
[`soil_world_vsi`](https://rspatial.github.io/geodata/reference/soil_grids_vsi.md)
For Africa:
[`soil_af_isda`](https://rspatial.github.io/geodata/reference/soil_af_isda.md),
[`soil_af`](https://rspatial.github.io/geodata/reference/soil_af.md),
[`soil_af_elements`](https://rspatial.github.io/geodata/reference/soil_af_elements.md)

## Examples

``` r
# \donttest{
# this downloads a large file 
gph <- soil_world(var="phh2o", depth=5, path=tempdir())
#> Cached as: /tmp/Rtmp0MapG8/soil_world/phh2o_0-5cm_mean_30s.tif
# }
```
