# Add functions

These functions add new columns to the ingested data prior to being
validated or imported into the database

## Usage

``` r
add_new_id(df, id_name, max_ids)

add_source_id(tbl_samples, tbl_sources)

add_sub_sor_tbl(split_tables, sub_tbl, sor_tbl)

add_table_ids(tables_split, tables_ids, max_ids)

add_taxonomic_groups(df, species_list)

add_valid_cols(df, valid_values)

add_valid_taxonomy(df, species_list)
```

## Arguments

- df:

  a `data.frame`

- id_name:

  the id column name to add new id to in the database

- max_ids:

  the max id value

- tbl_samples:

  `data.frame` that contains sample data

- tbl_sources:

  `data.frame` that contains source data

- split_tables:

  a `list` cotaining `tbl_samples` split into database tables prior to
  submission.

- sub_tbl:

  the `data.frame` that will be submitted to `tbl_submission` in the
  database.

- sor_tbl:

  the `data.frame` that will be submitted to `tbl_sources` in the
  database.

- tables_split:

  a `list` cotaining `tbl_samples` split into database tables prior to
  submission.

- tables_ids:

  column name that ids are assigned

- species_list:

  a `tbl_lazy` object of the `tbl_taxonomy` from the database

- valid_values:

  a `data.frame` containing valid values from database schema derived
  from
  [`get_valid_values()`](https://benjaminhlina.github.io/glatar-app/reference/get_functions.md).

## Details

`add_valid_taxonomy()` adds columns whether or not `valid_taxonomy()`
returns species that are present in `tbl_taxononomy`'s `common_name` and
`scientific_name`

## See also

[`get_valid_values()`](https://benjaminhlina.github.io/glatar-app/reference/get_functions.md)
