# View Map Tab

Provides the mapping tab.

## Usage

``` r
view_map_ui(id)

view_map_server(id, con)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"view_map"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

## Details

`view_map_ui()` provides the mappping user interface.

`view_map_server()` provides the mapping server.
