# Soil data for Africa

Download chemical soil properties data for Africa for different soil
depths. The spatial resolution is 30 arc-seconds (about 1 km2),
aggregated from the original 250m resolution.

There are more recent estimations for some of the properties available
in other data sets. See
[`soil_af_isda`](https://rspatial.github.io/geodata/reference/soil_af_isda.md)
and
[`soil_world`](https://rspatial.github.io/geodata/reference/soil_grids.md).

For more info, see
https://web.archive.org/web/20250421195225/https://www.isric.org/projects/soil-property-maps-africa-250-m-resolution

The data have a CC-BY 4.0 NC license

## Usage

``` r
soil_af(var, depth, path, ...)
```

## Arguments

- var:

  character. Variables name (see Details below). One of: "acid-exch",
  "Al-extr", "Al-exch", "bases-exch", "BDR", "BLKD", "Ca-exch", "CEC",
  "clay", "coarse", "drain", "ECN", "K-exch", "Mg-exch", "Na-exch",
  "Ntot", "pH", "sand", "silt", "SOC"

- depth:

  numeric. One of `5, 15, 30, 60, 100, 200`. This is shorthand for the
  following depth ranges: 0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm.
  Or one of `20, 50` for 0-20 or 20-50 cm

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## Details

|     |            |                                  |               |
|-----|------------|----------------------------------|---------------|
|     | **var**    | **description**                  | **unit**      |
|     | clay       | Soil texture fraction clay       | %             |
|     | sand       | Soil texture fraction sand       | %             |
|     | silt       | Soil texture fraction silt       | %             |
|     | coarse     | Coarse fragments volumetric      | %             |
|     | SOC        | Organic carbon                   | g kg⁻¹ (‰)    |
|     | BLKD       | Bulk density (fine earth)        | kg m⁻³        |
|     | BDR        | Depth to bedrock                 | cm            |
|     | .          | .                                | .             |
|     | pH         | pH (H₂O)                         | \-            |
|     | ECN        | Electrical conductivity          | mS/m (?)      |
|     | acid-exch  | Exchangeable acidity             | cmol(+) kg⁻¹  |
|     | bases-exch | Sum of exchangeable bases        | cmol(+) kg⁻¹  |
|     | CEC        | Cation Exchange Capacity         | cmol(+) kg⁻¹  |
|     | Al-extr    | Extractable Aluminum (Mehlich 3) | mg kg⁻¹ (ppm) |
|     | Al-exch    | Exchangeable Aluminum            | cmol(+) kg⁻¹  |
|     | Ca-exch    | Exchangeable Calcium             | cmol(+) kg⁻¹  |
|     | K-exch     | Exchangeable Potassium           | cmol(+) kg⁻¹  |
|     | Mg-exch    | Exchangeable Magnesium           | cmol(+) kg⁻¹  |
|     | Na-exch    | Exchangeable Sodium              | cmol(+) kg⁻¹  |
|     | Ntot       | Total nitrogen                   | g kg⁻¹        |

## References

Hengl T, Heuvelink GBM, Kempen B, Leenaars JGB, Walsh MG, Shepherd KD,
et al. (2015) Mapping Soil Properties of Africa at 250 m Resolution:
Random Forests Significantly Improve Current Predictions. PLoS ONE
10(6): e0125814. doi:10.1371/journal.pone.0125814

## See also

[`soil_af_elements`](https://rspatial.github.io/geodata/reference/soil_af_elements.md),
[`soil_af_isda`](https://rspatial.github.io/geodata/reference/soil_af_isda.md),
[`soil_world_vsi`](https://rspatial.github.io/geodata/reference/soil_grids_vsi.md)

## Examples

``` r
# \donttest{
# downloads a large file
aph <- soil_af(var="ph", depth=5, path=tempdir())
# }
```
