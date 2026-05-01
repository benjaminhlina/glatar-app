# Upload New Data to Database

This function takes the split tables and uploads them to the database.

## Usage

``` r
upload_to_db(con, tables_to_submit)
```

## Arguments

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- tables_to_submit:

  a `list` containing the the tables to be submitted.

## Value

a `list` that has results information and the submitted tables.
