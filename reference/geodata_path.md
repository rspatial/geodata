# Set the data path

This function allows you set or get the default download cache path for
the geodata package. The default path is ignored if you set the `path`
argument in a geodata function to another value.

The "factory-fresh" default location for downloads is returned by
`file.path(rappdirs::user_data_dir(), ".geodata")`

You can set it to another value with this function. To save your own
default path across sessions, you can add a line like this:

`options( geodata_default_path = "c:/your/geodata/path")` to the file
returned by `file.path( R.home(), "etc/Rprofile.site")`

Alternatively, you can also set a system variable "GEODATA_PATH" to the
desired path.

## Usage

``` r
geodata_path(path)
```

## Arguments

- path:

  character. Path name where geodata should be download and cache data.
  If missing, the current default path is returned. Use `NA` to restore
  the original value

## Value

character

## Examples

``` r
geodata_path()
#> [1] "~/.local/share/geodata"
```
