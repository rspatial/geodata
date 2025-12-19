# Soil elements data for Africa

Connect to or download chemical soil element concentration (for the 0-30
cm topsoil) data for Africa. The spatial resolution is 30 arc-seconds
(about 1 km2), aggregated from the original 250 m spatial resolution.

The data have an Open Data Commons Open Database License (ODbL)

For more information, see
https://web.archive.org/web/20250421195225/https://www.isric.org/projects/soil-property-maps-africa-250-m-resolution

## Usage

``` r
soil_af_elements(var, path, ...)
```

## Arguments

- var:

  character. Variables name. One of: "Al", "B", "Ca", "Cu", "Fe", "K",
  "Mg", "Mn", "N", "Na", "P", "Ptot", "Zn". See Details

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## Details

|     |         |                        |               |
|-----|---------|------------------------|---------------|
|     | **var** | **description**        | **unit**      |
|     | Al      | Extractable aluminum   | mg kg⁻¹ (ppm) |
|     | B       | Extractable boron      | mg kg⁻¹ (ppm) |
|     | Ca      | Extractable calcium    | mg kg⁻¹ (ppm) |
|     | Cu      | Extractable copper     | mg kg⁻¹ (ppm) |
|     | Fe      | Extractable iron       | mg kg⁻¹ (ppm) |
|     | K       | Extractable potassium  | mg kg⁻¹ (ppm) |
|     | Mg      | Extractable magnesium  | mg kg⁻¹ (ppm) |
|     | Mn      | Extractable manganese  | mg kg⁻¹ (ppm) |
|     | N       | Organic nitrogen       | mg kg⁻¹ (ppm) |
|     | Na      | Extractable sodium     | mg kg⁻¹ (ppm) |
|     | P       | Extractable phosphorus | mg (100 kg⁻¹) |
|     | Ptot    | Total phosphorus       | mg (100 kg⁻¹) |
|     | Zn      | Extractable zinc       | mg kg⁻¹ (ppm) |

## References

Hengl T, Heuvelink GBM, Kempen B, Leenaars JGB, Walsh MG, Shepherd KD,
et al. (2015) Mapping Soil Properties of Africa at 250 m Resolution:
Random Forests Significantly Improve Current Predictions. PLoS ONE
10(6): e0125814. doi:10.1371/journal.pone.0125814

## See also

[`soil_af`](https://rspatial.github.io/geodata/reference/soil_af.md),
[`soil_af_isda`](https://rspatial.github.io/geodata/reference/soil_af_isda.md),
[`soil_world`](https://rspatial.github.io/geodata/reference/soil_grids.md)

## Examples

``` r
# \donttest{
# downloads a large file
fe <- soil_af_elements("Fe", path=tempdir(), quiet=TRUE)
#> The geodata server is temporary out of service for maintenance. It should be back on 20 December. 
# }
```
