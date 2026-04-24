# Get functions

These functions are very important as they get infomration from a
PostgreSQL database, whther that is schemas, raw values or other
information about the database. Each function creates a SQL string that
is excuted on a given table in the database.

## Usage

``` r
get_column_map(con)

get_data(con, debug_sql = FALSE)

get_data_tables(con, df, data_tables, flag_cols, var)

get_dropdown_choices(df, type)

get_good_groups(df)

get_id_col(con)

get_id_max(table_name, id_col)

get_join_table(df, table, con)

get_numeric_vars(con)

get_ranges(df, x_var, y_var)

get_raw_data(
  con,
  selected_vars = NULL,
  grouping_vars = NULL,
  debug_sql = FALSE
)

get_selected_tab(input)

get_summary_data(
  con,
  selected_vars = NULL,
  grouping_vars = NULL,
  debug_sql = FALSE
)

get_submission_id(con)

get_tables_needed(con, var)

get_taxa_col(con)

get_theme_choices(
  con,
  theme,
  numeric_choices,
  numeric_names,
  length_vars,
  energy_vars
)

get_valid_values(con)

get_var_types(df, var)
```

## Arguments

- con:

  a valid `DBI` connection to a PostgreSQL database.

- debug_sql:

  a logical value that will provide SQL for debugging. Defualt is
  `FALSE`.

- df:

  a `tbl_lazy`.

- data_tables:

  a vector produced by
  [`data_tables()`](https://benjaminhlina.github.io/glatar-app/reference/vector_functions.md).

- flag_cols:

  a vector that has replaced `tbl_` to `has_` if table is present in
  [`data_tables()`](https://benjaminhlina.github.io/glatar-app/reference/vector_functions.md).

- var:

  a variable of interst to get unique values.

- type:

  variable of interest for a given dropdown.

- table_name:

  the name of the database table to get the maxiumum id column value.

- id_col:

  name of the corresponding id column in a given `table_name` that we
  want the maximum id value.

- table:

  table name to get from the database.

- x_var:

  the `x` variable of interest.

- y_var:

  the `y` variable of interest.

- selected_vars:

  a `reactive` object that is the column name of interest usually
  generated from sidebar.

- grouping_vars:

  a `reactive` object that is the column name of grouping variables
  usually generated from sidebar.

- input:

  input from UI

- theme:

  a `vector` containing different data themes

- numeric_choices:

  a `reactive` value that has numerical choices.

- numeric_names:

  a `reactive` value that has selected numerical column names.

- length_vars:

  a `reactive` value that has length variables.

- energy_vars:

  a `reactive` value that has energy variables.

## Details

`get_column_map()` gets the data dictonary from the database.

`get_data()` retrives base template for all sample data in the the
database. It gets `tbl_samples`, `tbl_loc`, and `tbl_length` and joins
them to make a base for all other opperations.

`get_data_tables()` this retrieves and identifes which data belongs to
each sample id. For example, id `1` has calorimetry and proximent
composition data. This will start the base dataframe to gather this
information.

`get_id_col()`use a SQL query where we select all of the ID columns
throughout the entire database and then select only the columns that we
want to add new ID values to. This information is then fed to
`get_id_max()`.

`get_raw_data()` retrives base template for all sample data in the the
database and joins variables and grouping variables of interest.

`get_selected_tabs()` is an exception within `get_*` functions as it
doesn't interact with the database and instead interacts with the
`shiny` UI.

`get_summary_data()` retrives base template for all sample data in the
the database and joins variables and grouping variables of interest for
summarizing.

`get_tables_needed()` this gets the tables of interest

`get_valid_values()` gets all the schemas in the database and grabs all
the constraints and cleans them up so we can use the constraints to
validate data.

## See also

`get_id_max()`

`get_id_col()`

`get_column_map()`
