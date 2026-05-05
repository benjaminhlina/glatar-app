# Taxa Search Tab

Provides the taxa search tab.

## Usage

``` r
taxa_search_ui(id)

taxa_search_server(id, con)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"taxa_search"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

## Details

`taxa_search_ui()` provides the taxa search user interface.

`taxa_search_server()` provides the taxa search server.
