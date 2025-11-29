# Get country codes

Get country codes for all countries in the world.

## Usage

``` r
country_codes(query=NULL)
```

## Arguments

- query:

  character. A single word that can be used to subset the returned
  data.frame

## Value

data.frame

## Examples

``` r
cc <- country_codes()
head(cc)
#>                    NAME ISO3 ISO2       NAME_ISO       NAME_FAO
#> 1           Afghanistan  AFG   AF    AFGHANISTAN    Afghanistan
#> 2 Akrotiri and Dhekelia  XAD <NA>           <NA>           <NA>
#> 3                 Åland  ALA   AX  ÅLAND ISLANDS           <NA>
#> 4               Albania  ALB   AL        ALBANIA        Albania
#> 5               Algeria  DZA   DZ        ALGERIA        Algeria
#> 6        American Samoa  ASM   AS AMERICAN SAMOA American Samoa
#>              NAME_LOCAL      SOVEREIGN       UNREGION1 UNREGION2 continent
#> 1           Afghanestan    Afghanistan   Southern Asia      Asia      Asia
#> 2 Akrotiri and Dhekelia United Kingdom    Western Asia      Asia      Asia
#> 3                 Åland        Finland Northern Europe    Europe    Europe
#> 4             Shqiperia        Albania Southern Europe    Europe    Europe
#> 5            Al Jaza'ir        Algeria Northern Africa    Africa    Africa
#> 6        American Samoa  United States       Polynesia   Oceania   Oceania

p <- country_codes(query="Per")
p
#>                  NAME ISO3 ISO2 NAME_ISO NAME_FAO        NAME_LOCAL SOVEREIGN
#> 4             Albania  ALB   AL  ALBANIA  Albania         Shqiperia   Albania
#> 51  Clipperton Island  XCL <NA>     <NA>     <NA> Clipperton Island    France
#> 178              Peru  PER   PE     PERU     Peru              Perú      Perú
#>           UNREGION1 UNREGION2     continent
#> 4   Southern Europe    Europe        Europe
#> 51  Central America  Americas North America
#> 178   South America  Americas South America
```
