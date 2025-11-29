# iSDA soil data for Africa

Download soil data for Africa derived from the iDSA data set. The
original data were aligned and aggregated to 30 arc-seconds (about 1
km2). The original spatial resolution was 30m.

For more info see:

<https://envirometrix.nl/isdasoil-open-soil-data-for-africa/>

<https://zenodo.org/search?page=1&size=20&q=iSDAsoil>

## Usage

``` r
soil_af_isda(var, depth=20, error=FALSE, path, virtual=FALSE, ...)
```

## Arguments

- var:

  character. Name of the soil variable. Should be one of: "Al", "bdr",
  "clay", "C.tot", "CEC", "Ca", "db.od", "eCEC.f", "Fe", "K", "Mg",
  "N.tot", "oc", "P", "pH.H2O", "sand", "silt", "S", "texture", "wpg2",
  "Zn". See the Details section below

- depth:

  numeric. One of 20 (for 0-20 cm) and 50 (for 20-50 cm). Ignored if
  `var="bdr"` for which the depth is always 0-200 cm

- error:

  logical. If `TRUE` the error estimates are returned

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- virtual:

  logical. If `TRUE` a virtual connection to the file is returned. This
  is useful if you want to extract a small area without downloading the
  entire raster

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## Details

|     |         |                                    |              |
|-----|---------|------------------------------------|--------------|
|     | **var** | **description**                    | **unit**     |
|     | Al      | extractable aluminum               | mg kg⁻¹      |
|     | bdr     | bed rock depth                     | cm           |
|     | clay    | clay content                       | %            |
|     | C.tot   | total carbon                       | kg⁻¹         |
|     | Ca      | extractable calcium                | mg kg⁻¹      |
|     | db.od   | bulk density                       | kg m⁻³       |
|     | eCEC.f  | effective cation exchange capacity | cmol(+) kg⁻¹ |
|     | Fe      | extractable iron                   | mg kg⁻¹      |
|     | K       | extractable potassium              | mg kg⁻¹      |
|     | Mg      | extractable magnesium              | mg kg⁻¹      |
|     | N.tot   | total organic nitrogen             | g kg⁻¹       |
|     | OC      | Organic Carbon                     | g kg⁻¹       |
|     | P       | extractable phosphorus             | mg kg⁻¹      |
|     | pH.H2O  | pH (H₂O)                           | \-           |
|     | sand    | Sand content                       | %            |
|     | silt    | Silt content                       | %            |
|     | S       | Extractable sulfer                 | mg kg⁻¹      |
|     | texture | texture class                      | \-           |
|     | wpg2    | stone content                      | %            |
|     | Zn      | Extractable zinc                   | mg kg⁻¹      |

## References

Tomislav Hengl, Matthew A. E. Miller, Josip Križan, Keith D. Shepherd,
Andrew Sila, Milan Kilibarda, Ognjen Antonijevic, Luka Glušica, Achim
Dobermann, Stephan M. Haefele, Steve P. McGrath, Gifty E. Acquah, Jamie
Collinson, Leandro Parente, Mohammadreza Sheykhmousa, Kazuki Saito,
Jean-Martial Johnson, Jordan Chamberlin, Francis B.T. Silatsa, Martin
Yemefack, John Wendt, Robert A. MacMillan, Ichsani Wheeler & Jonathan
Crouch, 2021. African soil properties and nutrients mapped at 30 m
spatial resolution using two-scale ensemble machine learning. Scientific
Reports 11: 6130.

## See also

[`soil_af_elements`](https://rspatial.github.io/geodata/reference/soil_af_elements.md),
[`soil_af`](https://rspatial.github.io/geodata/reference/soil_af.md),
[`soil_world`](https://rspatial.github.io/geodata/reference/soil_grids.md)

## Examples

``` r
# \donttest{
# downloads a large file
afph <- soil_af_isda("ph.h2o", path=tempdir(), quiet=TRUE)
# }
```
