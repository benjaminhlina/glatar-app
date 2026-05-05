# Summary Info Tab

Provides the summary info tab.

## Usage

``` r
view_summary_info_ui(id)

summary_info_server(id, con, main_input, summary_sidebar_vals)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"summary_info"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- main_input:

  the shiny input from the main server.

- summary_sidebar_vals:

  a `reactiveVal()` object produced by
  [`summary_sidebar_server()`](https://benjaminhlina.github.io/glatar-app/reference/summary_sidebar_module.md).
  The inputs from this dictate what is displayed in the main panel.

## Details

`view_summary_info_ui()` provides the summary info user interface.

`summary_info_server()` provides the summary info server.
