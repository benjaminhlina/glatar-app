# Raw Data Sidebar

Provides the sidebar for the raw data pane.

## Usage

``` r
raw_data_sidebar_ui(id)

raw_data_sidebar_server(id, con, main_input, auth_state)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"raw_sidebar"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- main_input:

  the shiny input from the main server.

- auth_state:

  the authorization state to login.

## Details

`raw_data_sidebar_ui()` that provides user interface for raw data
sidebar

`raw_data_sidebar_server()` provides the raw data sidebar server.
