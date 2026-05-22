# Start Database Connection

This function uses environment supplied variables, `{DBI}`, and
`{RPostgres}` to connect to a PostgreSQL database.

## Usage

``` r
start_db_con(username = NULL, password = NULL)
```

## Arguments

- username:

  The username for user specific connections. Default is `NULL` and will
  use default user.

- password:

  The passward for user specific connections. Default is `NULL` and will
  use default user.

## Value

Returns a connection object.
