# Convert functions

These functions convert columns whether that is column names or the
values in the columns themeselves.

## Usage

``` r
convert_nice_name(cols, lookup = nice_name_lookup)
```

## Arguments

- cols:

  column to be converted

- lookup:

  a `vector` that has the name of the column and the nice looking name
  of the column e.g., `sample_id` becomes `Sample ID`.
