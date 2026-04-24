# Display functions

These functions are all display vital information within GLATAR such as
plots, histograms, and tables. They function by wrapping the given
display object (e.g., table) with a `render*()` function from `shiny`.

## Usage

``` r
display_add_taxa(data, output, output_id, session)

display_col_width(...)

display_hist(data, input_source, output, output_id = "summary_histogram")

display_scatter_plot(data, input_source, output, output_id = "scatter_plot")

display_submission_map(ns, output, output_id = "map", split_tables)

display_sub_map_msg(
  ns,
  output,
  output_id = "location_map",
  split_tables,
  validated_submission,
  validated_sources,
  validated_samples
)

display_table(data, output, output_id = "summary_table_output", search = TRUE)

display_upload_status(
  ns,
  output,
  output_id = "upload_status",
  upload_succeeded = NULL,
  submission_results = NULL,
  email_succeeded = NULL,
  user_email = NULL
)

display_validation_status(
  ns,
  output,
  output_id = "upload_status",
  split_tables = NULL,
  validated = TRUE
)
```

## Arguments

- data:

  a reactive data object for a given variable. `observe()` or
  `observeEvent()` calls within a module.

- output:

  an output within a shiny module.

- output_id:

  the id name of the output, e.g., `"summary_histogram"`.

- session:

  module session.

- ...:

  additional arguments

- input_source:

  usually an object created by a sidebar function. These objects tend to
  be `reactive()` outcomes from `observe()` or `observeEvent()` calls
  within a module.

- ns:

  a namespace object created from `NS()` from `shiny`

- split_tables:

  a `list` that contains the tables that will be submitted split into
  their given PostgreSQL table names.

- validated_submission:

  a reactive value this contains the validated `tbl_submission`.

- validated_sources:

  a reactive value this contains the validated `tbl_sources`.

- validated_samples:

  a reactive value this contains the validated `tbl_samples`.

- search:

  a logitcal value that determines whether `datatable()` from `DT` has a
  search bar or not. Default value is `TRUE`.

- upload_succeeded:

  a [`tryCatch()`](https://rdrr.io/r/base/conditions.html) that tries to
  submit the split tables to the database. The returning object is a
  logical value, `TRUE` or `FALSE` which will change the given status of
  ghe upload that is then displayed to the user. This object is returned
  by `upload_to_db()`.

- submission_results:

  an object returned by `upload_to_db` that has the name of the table,
  the number of rows that were submitted, and the submission id so this
  can be dynamically displayed to the user.

- email_succeeded:

  a [`tryCatch()`](https://rdrr.io/r/base/conditions.html) that tries to
  send an meail. The returning object is a logical value, `TRUE` or
  `FALSE` which will change the given email status displayed to the
  user.

- user_email:

  a valid email address of the user, e.g., `user.name@example.com`.

- validated:

  a logical value to determine if a validation succeeded. Default is
  `TRUE`.
