# Summary Table Sidebar

Provides the sidebar for the summary table pane.

## Usage

``` r
summary_sidebar_ui(id)

summary_sidebar_server(id, con, main_input)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"summary_sidebar"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- main_input:

  the shiny input from the main server

## Details

`summary_sidebar_ui()` provides the summary table sidebar user
interface.

`summary_sidebar_server()` provides the summary table sidebar server.
