# Cropland distribution data

Cropland distribution data at a 30-seconds spatial resolution from three
sources:

`worldcover` is derived from the ESA WorldCover data set at 0.3-seconds
resolution. (License CC BY 4.0), see <https://esa-worldcover.org/en>.
Values were aggregated and represent the fraction cropland in each cell.

`glad` is derived from the "Global cropland expansion in the 21st
century" (Potatov et al) data available
[here](https://glad.umd.edu/dataset/croplands). Values were aggregated
and resampled. They represent the fraction cropland in each cell. There
are five layers representing the following years: 2003, 2007, 2011,
2015, and 2019.

`QED` has cropland distribution data for Africa. The values are
probabilities of cropland presence estimated with a neural network that
was trained on an initial 1-million point
[Geosurvey](https://geosurvey.qed.ai) conducted in 2015. License:
CC-BY-SA 4.0; <https://about.maps.qed.ai/>

## Usage

``` r
cropland(source, path, year, ...)
```

## Arguments

- source:

  character. One of "WorldCover", "GLAD", or "QED"

- path:

  character. Path for storing the downloaded data. See
  [`geodata_path`](https://rspatial.github.io/geodata/reference/geodata_path.md)

- year:

  numeric. Optional for the GLAD dataset to get data for a single year.
  One of 2003, 2007, 2011, 2015, and 2019

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

SpatRaster

## See also

[`landcover`](https://rspatial.github.io/geodata/reference/landcover.md)

## References

WorldCover: Zanaga, D., Van De Kerchove, R., De Keersmaecker, W.,
Souverijns, N., Brockmann, C., Quast, R., Wevers, J., Grosu, A.,
Paccini, A., Vergnaud, S., Cartus, O., Santoro, M., Fritz, S.,
Georgieva, I., Lesiv, M., Carter, S., Herold, M., Li, Linlin,
Tsendbazar, N.E., Ramoino, F., Arino, O., 2021. ESA WorldCover 10 m 2020
v100. doi:10.5281/zenodo.5571936.

GLAD: Potapov, P., S. Turubanova, M.C. Hansen, A. Tyukavina, V. Zalles,
A. Khan, X.-P. Song, A. Pickens, Q. Shen, J. Cortez, 2021. Global maps
of cropland extent and change show accelerated cropland expansion in the
twenty-first century. Nature Food. doi:10.1038/s43016-021-00429-z
