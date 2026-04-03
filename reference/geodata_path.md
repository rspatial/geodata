# Set the data path

This function allows you set or get the default download cache path for
the geodata package. The default path is ignored if you set the `path`
argument in a geodata function to another value.

A special value is "user_data_dir". This sets it to the location
returned by `file.path(rappdirs::user_data_dir(), "geodata")`

Alternatively, you can also set a system variable "GEODATA_PATH" to the
desired path.

## Usage

``` r
geodata_path(path, persistent=TRUE)
```

## Arguments

- path:

  character. Path name where geodata should be download and cache data.
  If missing, the current default path is returned. Use `NA` to restore
  the original value

- persistent:

  logical. Should the path be persistent (automatically used in future R
  sessions)?

## Value

character

## Examples

``` r
geodata_path()
#> [1] ""
```
