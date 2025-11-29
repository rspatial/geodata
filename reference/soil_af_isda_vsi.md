# iSDA soil data for Africa, virtual connection

Virtually connect to the iSDA soil data for Africa. The spatial of these
data is 30m.

For more info see:

<https://envirometrix.nl/isdasoil-open-soil-data-for-africa/>

<https://zenodo.org/search?page=1&size=20&q=iSDAsoil>

## Usage

``` r
soil_af_isda_vsi(var)
```

## Arguments

- var:

  character. The variables name, one of: "Al", "bdr", "clay", "C.tot",
  "CEC", "Ca", "db.od", "eCEC.f", "Fe", "K", "Mg", "N.tot", "oc", "P",
  "pH.H2O", "sand", "silt", "S", "texture", "wpg2", "Zn".see Details

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
|     | P       | Phosphorus content                 | mg kg⁻¹      |
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
[`soil_af_isda`](https://rspatial.github.io/geodata/reference/soil_af_isda.md),
[`soil_world`](https://rspatial.github.io/geodata/reference/soil_grids.md)
