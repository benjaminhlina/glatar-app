# Start Database Connection

This function uses environment supplied variables, `{DBI}`, and
`{RPostgres}` to connect to a PostgreSQL database.

## Usage

``` r
start_db_con(user = NULL)
```

## Arguments

- username:

  The username for user specific connections. Default is `NULL` and will
  use default user.

## Value

returns a connection object.
