# Clean functions

These functions clean up `data.frames` and `vectors` that are coming
into the the databae prior to submission, validation reports, or are in
the database and need need to be cleaned prior to being displayed (e.g.,
schema constraints)

## Usage

``` r
clean_all_validations(...)

clean_data_tables(df, flag_cols, type, group_cols, filter_coords = TRUE)

clean_db_constraints(clause)

match_to_db_col(col_name, db_cols)

rename_to_db_col(df, con, table_name)

clean_row_index(row_index_str, min_run = 5)
```

## Arguments

- ...:

  objects to be cleaned. Usually validation reports

- df:

  a `data.frame` of imported new data

- flag_cols:

  collumns to flag and coalesce

- type:

  is the data table type that is being supplied e.g., tbl_isotope

- group_cols:

  columns that will be grouping our summariziing e.g., lat and lon, pi
  name, waterbody ect.

- filter_coords:

  Logical value that controls whether `NA` coords are removed. Default
  value is `TRUE`.

- clause:

  constraint clause to be cleaned

- col_name:

  column name to be cleaned to match database column names

- db_cols:

  column names in the database to be used

- con:

  PostgreSQL connection object using `{DBI}`

- table_name:

  the table name in the database that each column name is assinged to.

- row_index_str:

  a column containing the row index from submitted data during
  validation

- min_run:

  minimum number of consecutive numbers before it condenses the row
  numbers. Default is `5`.
