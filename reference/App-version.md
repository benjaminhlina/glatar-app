# App version functions

These function build and display the app version and build time.

## Usage

``` r
app_version_head()

app_version_label(app_version, build_time = NULL)
```

## Arguments

- app_version:

  `character`for app version.

- build_time:

  a `POSIXct` object. If `NULL` it uses `Sys.time(). `
