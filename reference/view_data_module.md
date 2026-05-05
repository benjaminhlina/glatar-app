# View Raw Data Tab

Provides the raw data tab.

## Usage

``` r
view_data_ui(id)

view_data_server(id, con, main_input, raw_sidebar_vals, auth_state)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"view_data"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- main_input:

  the shiny input from the main server.

- raw_sidebar_vals:

  a `reactiveVal()` object produced by
  [`raw_data_sidebar_server()`](https://benjaminhlina.github.io/glatar-app/reference/raw_data_module.md).
  The inputs from this dictate what is displayed in the main panel.

- auth_state:

  the authorization state to login.

## Details

`view_data_ui()` provides the view data user interface.

`view_data_server()` provides the raw data tab server.
