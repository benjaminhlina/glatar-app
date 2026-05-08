# Source Tab

Provides the source tab.

## Usage

``` r
view_source_ui(id)

view_source_server(id, con, main_input, source_sidebar_vals)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"view_source"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- main_input:

  the shiny input from the main server.

- source_sidebar_vals:

  a `reactiveVal()` object produced by
  [`source_sidebar_server()`](https://benjaminhlina.github.io/glatar-app/reference/source_sidebar_module.md).
  The inputs from this dictate what is displayed in the main panel.

## Details

`view_source_ui()` provides the source user interface.

`view_source_server()` provides the source server.
