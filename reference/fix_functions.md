# Fix Functions Email Body

These functions fix or adjust an object to better fit within the
required user interface and database. For example case types

## Usage

``` r
fix_case_types(df)

fix_table_order(split_tables)

fix_title_label(x, max = NULL)

fix_var_generic(df, var_raw)
```

## Arguments

- df:

  a `data.frame`

- split_tables:

  a `list` containing inported data as `data.frames` ready to be
  submited. \`

- x:

  a vector of selected variables.

- max:

  the maximum nmber of objects to permit before the function starts
  condense the names. Defaults to `4`.

- var_raw:

  a reactive value that is the raw variable name

## Details

`fix_case_types` fix case types once validation has occured for incoming
data into the database.

`fix_table_order` fixes the order of the tables within the `list` so we
can iteratively load data into the the database. The proper order is
`tbl_submission`, `tbl_sources`, and \`tbl_samples“ the an other
supporting tables.

`fix_title_label` adjusts the plot title to be dynamic and change based
on the selected species and waterbodies.

`fix_var_generic` returns a `list` containing three objects one being
`df` with the selected variable, one being the `variable` name and the
last being `variable label`. This function is used in summary tables,
histograms and scatter plots.
