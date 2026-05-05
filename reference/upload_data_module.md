# Upload Data Tab

Provides the upload data tab.

## Usage

``` r
upload_data_ui(id)

upload_data_server(id, con, auth_state)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"insert_data"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- auth_state:

  the authorization state to login.

## Details

`upload_data_ui()` provides the upload data user interface.

`upload_data_server()` provides the upload data server.
