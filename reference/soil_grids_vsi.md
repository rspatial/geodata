# Global soil data, virtual connection

Virtually connect to the global soilgrids data. See
https://www.isric.org/explore/soilgrids for more info.

data license: CC-BY 4.0

## Usage

``` r
soil_world_vsi(var, depth, stat="mean", name="")
```

## Arguments

- var:

  character. Variables name. One of: "bdod", "cfvo", "clay", "nitrogen",
  "ocd", "ocs", "phh2o", "sand", "silt", "soc", "wrb". See Details

- depth:

  numeric. One of 5, 15, 30, 60, 100, 200. This is shorthand for the
  following depth ranges: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm.
  Ignored if `var="wrb"`

- stat:

  character. One of "mean", "uncertainty", "Q0.05", "Q0.5", "Q0.95".
  Ignored if `var="wrb"`

- name:

  character. One of 'Acrisols', 'Albeluvisols', 'Alisols', 'Andosols',
  'Arenosols', 'Calcisols', 'Cambisols', 'Chernozems', 'Cryosols',
  'Durisols', 'Ferralsols', 'Fluvisols', 'Gleysols', 'Gypsisols',
  'Histosols', 'Kastanozems', 'Leptosols', 'Lixisols', 'Luvisols',
  'Nitisols', 'Phaeozems', 'Planosols', 'Plinthosols', 'Podzols',
  'Regosols', 'Solonchaks', 'Solonetz', 'Stagnosols', 'Umbrisols',
  'Vertisols'. Only used when `var="wrb"`

## Value

SpatRaster

## Details

The below table lists the variable names, a description, and the units
of the variables. Note that these units are not standard units, and are
different from the data for other soil data available through this
package.

|     |            |                                             |              |
|-----|------------|---------------------------------------------|--------------|
|     | **var**    | **description**                             | **unit**     |
|     | `bdod`     | Bulk density of the fine earth fraction     | cg cm⁻³      |
|     | `cec`      | Cation Exchange Capacity of the soil        | mmol(+) kg⁻¹ |
|     | `cfvo`     | Vol. fraction of coarse fragments (\> 2 mm) | ‰            |
|     | `nitrogen` | Total nitrogen (N)                          | cg kg⁻¹      |
|     | `phh2o`    | pH (H₂O)                                    | \-           |
|     | `sand`     | Sand (\> 0.05 mm) in fine earth             | ‰            |
|     | `silt`     | Silt (0.002-0.05 mm) in fine earth          | ‰            |
|     | `clay`     | Clay (\< 0.002 mm) in fine earth            | ‰            |
|     | `soc`      | Soil organic carbon in fine earth           | dg kg⁻¹      |
|     | `ocd`      | Organic carbon density                      | hg m⁻³       |
|     | `ocs`      | Organic carbon stocks                       | hg m⁻²       |

## References

Poggio, L., de Sousa, L.M., Batjes, N.H., Heuvelink, G.B.M., Kempen, B.,
Ribeiro, E., and Rossiter, D., 2021. SoilGrids 2.0: producing soil
information for the globe with quantified spatial uncertainty. Soil
7:217-240, 2021. doi:10.5194/soil-7-217-2021

## See also

[`soil_world`](https://rspatial.github.io/geodata/reference/soil_grids.md)
to download these data at 30-seconds spatial resolution.

For Africa:
[`soil_af_isda`](https://rspatial.github.io/geodata/reference/soil_af_isda.md),
[`soil_af`](https://rspatial.github.io/geodata/reference/soil_af.md),
[`soil_af_elements`](https://rspatial.github.io/geodata/reference/soil_af_elements.md)

## Examples

``` r
# \donttest{
ph <- soil_world_vsi(var="phh2o", depth=5)
ph
#> class       : SpatRaster 
#> size        : 58034, 159246, 1  (nrow, ncol, nlyr)
#> resolution  : 250, 250  (x, y)
#> extent      : -19949750, 19861750, -6147500, 8361000  (xmin, xmax, ymin, ymax)
#> coord. ref. : Interrupted_Goode_Homolosine 
#> source      : phh2o_0-5cm_mean.vrt 
#> name        : phh2o_0-5cm_mean 
# }
```
