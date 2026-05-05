# Source Tab

Provides the source tab.

## Usage

``` r
view_source_ui(id)

view_source_server(id, con, main_input)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"view_source"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- main_input:

  the shiny input from the main server.

## Details

`view_source_ui()` provides the source user interface.

`view_source_server()` provides the source server.
