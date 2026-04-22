# Create functions

These functions are all use other functions to create
[`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
objects usually used in modules. Any function that uses a `reactive()`
call is considered to be creating an object that is then displayed in
the UI.

## Usage

``` r
create_filtered_data(data, input_source, pane)

create_mean_data(data, input_source)

create_raw_data(
  activated = NULL,
  con,
  input_source,
  main_input,
  tab = NULL,
  var_field
)

create_searching_data(
  collect = TRUE,
  con = NULL,
  input,
  source_data = NULL,
  tbl_name = NULL
)

create_sidebar_df(con)

create_source_data(activated = NULL, con, main_input, tab = NULL)

create_summary_data(
  activated = NULL,
  con,
  input_source,
  main_input,
  tab = NULL,
  var_field
)

create_zoom_slider(data, input_source)
```

## Arguments

- data:

  a `reactive()` data frame like object usually created from another
  `create_function`. Considering raw data is gathered through a
  PostgresSQL connection these reactive objects tend to be `tbl_lazy`.

- input_source:

  usually an object created by a sidebar function. These objects tend to
  be `reactive()` outcomes from `observe()` or `observeEvent()` calls
  within a module.

- pane:

  the sidepane that is bein selected e.g., `"scatter_plot". `

- activated:

  is a `shiny` reactive value which gets triggered when the tab is
  activated. This makes it so that the tab stays the same when clicking
  away but also that it doesn't run the tab upon start up nor when
  clicking away.

- con:

  a `DBI` conection to, in this case PostgreSQL database

- main_input:

  the `main_input` from the shiny server.

- tab:

  the selected tab that is being displayed e.g., `"summary_info". `

- var_field:

  a `character` string that identifies the object in the `list` that is
  supplied to `input_source`. This object name is the variable field
  that is of interest for the particular use for a given function.

- collect:

  a `logical` that determines whether to `collect()` the data from the
  PostgreSQL database and turn into a `tibble` object in a given
  session. Default is `TRUE`.\`

- input:

  a given shiny module and server input.

- source_data:

  a reactive object that is `tbl_source` a `tbl_lazy` object from a
  PostgreSQL database.

- tbl_name:

  a `character` string of the database name to create searching data
  from e.g., `tbl_taxonomy`.
