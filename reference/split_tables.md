# Split Tables

Split tables takes validated and cleaned data submissions and splits
them by columns into the table name from the data dictionary.

## Usage

``` r
split_tables(df, tables_to_split)
```

## Arguments

- df:

  the `data.frame` containing the submitted data.

- tables_to_split:

  the information from data dictionary determing which columns belong to
  which tables.

## Value

returns a `list`containing the tables named apporipately and split into
their proper tables for database submission.
