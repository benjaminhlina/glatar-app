# Read functions

Read functions allow for `.xlsx` files to be read into the the app to be
able to then manipulate and eventually validate and import into the
database.

## Usage

``` r
read_col_types(file_path, tbl_name, skip, n_max)

read_xl(
  file_path,
  tbl_name,
  con = NULL,
  col_types = NULL,
  skip,
  rename = TRUE,
  rename_twice = NULL
)
```

## Arguments

- file_path:

  the path to the file provided by UI upload observer.

- tbl_name:

  the table name to be imported

- skip:

  the number of rows to skip given the header is not the first row.

- n_max:

  n_max is number of the maximum number of data rows to read.

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- col_types:

  a vector contianing the column types supplied by `read_col_types()`.

- rename:

  a logical that strips excesive information and renames column names to
  snake case and follows the database shcema. Defaults to `TRUE`.

- rename_twice:

  a logical that strips excesive information and renames column names to
  snake case and follows the database shcema. Defaults to \`NULL“.

## Value

`read_col_types()` returns a vector that has each column type for a
given excel file.

`read_xl()`returns a `data.frame` with the a given database table.

## Details

\`read_col_types()“ reads in a file and determiens the number of columns
and the column types

\`read_xl()“ reads in a file and cleans up the columns names before any
other processes occure such as validation and spliting to the database.
