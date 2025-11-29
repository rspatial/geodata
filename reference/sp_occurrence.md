# Download species occurrence data from GBIF

Download data from the Global Biodiversity Information Facility
([GBIF](https://www.gbif.org)) data portal.

`sp_genus` returns a data.frame with all the species names associated
with a genus.

`sp_occurrence` downloads occurrence records for a single species.

You can check the number of records returned by using the option
`download=FALSE`.

Note that the you can only download up to record number 100,000. To
avoid getting more than 100,000 records, you can do separate queries for
different geographic areas. This has been automated in
`sp_occurrence_split`. This function recursively splits the area of
interest into smaller areas until the number of records in an area is
less than 50,000. It then downloads these records and saves them in a
folder called "gbif". After all areas have been evaluated, the data are
combined into a single file and returned as a data.frame or SpatVector).
If the function is interrupted, it can be run again, and it will resume
where it left off.

If you want to download data for an entire genus, first run `sp_genus`
and then download data for the returned species names one by one.

Before using this function, please first check the GBIF [data use
agreement](https://www.gbif.org/terms/licences) and see the note below
about how to cite these data.

## Usage

``` r
sp_genus(genus, simple=TRUE, ...)

sp_occurrence(genus, species="", ext=NULL, args=NULL, geo=TRUE, 
  removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, 
  start=1, end=Inf, fixnames=TRUE, sv=FALSE, ...)

sp_occurrence_split(genus, species="", path=".", ext=c(-180,180,-90,90),
  args=NULL, geo=TRUE, removeZeros=FALSE, ntries=5, nrecs=300,
  fixnames=TRUE, prefix=NULL, sv=FALSE, ...)
```

## Arguments

- genus:

  character. genus name

- species:

  character. species name

- ext:

  SpatExtent object to limit the geographic extent of the records. A
  SpatExtent can be created using functions like
  [`ext`](https://rspatial.github.io/terra/reference/ext.html) and
  [`draw`](https://rspatial.github.io/terra/reference/draw.html)

- args:

  character. Additional arguments to refine the query. See query
  parameters in http://www.gbif.org/developer/occurrence for more
  details

- geo:

  logical. If `TRUE`, only records that have a georeference (longitude
  and latitude values) will be downloaded

- removeZeros:

  logical. If `TRUE`, all records that have a latitude OR longitude of
  zero will be removed if `geo==TRUE`, or set to `NA` if `geo==FALSE`.
  If `FALSE`, only records that have a latitude AND longitude that are
  zero will be removed or set to `NA`

- download:

  logical. If `TRUE`, records will be downloaded, else only the number
  of records will be shown

- ntries:

  integer. How many times should the function attempt to download the
  data, if an invalid response is returned (perhaps because the GBIF
  server is very busy)

- nrecs:

  integer. How many records to download in a single request (max is
  300)?

- start:

  integer. Record number from which to start requesting data

- end:

  integer. Last record to request

- fixnames:

  If `TRUE` a few unwieldy and poorly chosen variable names are changed
  as follows. "decimalLatitude" to "lat", "decimalLongitude" to "lon",
  "stateProvince" to "adm1", "county" to "adm2", "countryCode" to
  "ISO2". The names in "country" are replaced with the common (short
  form) country name, the original values are stored as "fullCountry"

- path:

  character. Where should the data be downloaded to (they will be put in
  a subdirectory "gbif")?

- prefix:

  character. prefix of the downloaded filenames (best left NULL, the
  function will then use "genus_species"

- simple:

  logical. If `TRUE`, a vector the accepted species names are returned.
  Otherwise a data.frame with much more information is returned

- sv:

  logical. If `TRUE`, a SpatVector rather than a data frame is returned.

- ...:

  additional arguments passed to
  [`download.file`](https://rdrr.io/r/utils/download.file.html)

## Value

data.frame, or SpatVector if `sv=TRUE`

## Note

Under the terms of the GBIF data user agreement, users who download data
agree to cite a DOI. Citation rewards data-publishing institutions and
individuals and provides support for sharing open data
\[[1](https://docs.ropensci.org/rgbif/articles/gbif_citations.html)\]\[[2](https://www.gbif.org/citation-guidelines)\].
You can get a DOI for the data you downloaded by creating a ["derived"
dataset](https://www.gbif.org/derived-dataset/about). For this to work,
you need to keep the "datasetKey" variable in your dataset.

## References

<https://www.gbif.org/occurrence>
<https://www.gbif.org/derived-dataset/about>

## Examples

``` r
sp_occurrence("solanum", "acaule", download=FALSE)
#> [1] 5058

sp_occurrence("Batrachoseps", "luciae", down=FALSE)
#> [1] 2376
g <- sp_occurrence("Batrachoseps", "luciae", geo=TRUE, end=5)
#> 5 records found
#> 1-
#> 5
#> 5 records downloaded
#plot(g[, c("lon", "lat")])


## args
#a1 <- sp_occurrence("Elgaria", "multicarinata", 
#      args="recordNumber=Robert J. Hijmans RH-2")
#a2 <- sp_occurrence("Batrachoseps", "luciae",
#      args=c("year=2023", "identifiedBy=Anthony Ye"))

## year supports "range queries"
#a3 <- sp_occurrence("Batrachoseps", "luciae", 
#      args=c("year=2020,2023", "identifiedBy=Kuoni W"))
#table(a3[,c("year")])
```
