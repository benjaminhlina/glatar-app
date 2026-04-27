# Message functions

These functions use `{cli}` to display messages in other functions when
different opperations occur. They are helpfui in seeing how the suer
interacts with the app as well as diagnosing isssues that may arrise.

## Usage

``` r
msg_dropdowns(
  waterbody_choices,
  common_name_choices,
  grouping_choices,
  numeric_choices
)

msg_hist_ui(df, var, type_val, col)

msg_hist_vars(df, var, ba)

msg_input_source(input_source_name, envir = parent.frame())

msg_mean_data(df, summary_grouping_vars, y_vals)

msg_summary_data(df, name = deparse(substitute(df)))

msg_selected_vars(selected_vars)

msg_tab_name(tab)
```

## Arguments

- waterbody_choices:

  a vector of water body choices

- common_name_choices:

  a vector of common name choices

- grouping_choices:

  a vector of grouping variable choices

- numeric_choices:

  a vector of numerical choices

- df:

  the `data.frame` that has summary information.

- var:

  the `x` variable that is being supplied to the histogram

- type_val:

  the type of value supplied

- col:

  the column name of the column in dataframe that is of interest

- ba:

  vector that is either `"before"` or `"after"`.

- input_source_name:

  the name of the input

- envir:

  the R environment

- summary_grouping_vars:

  the summary grouping variables

- y_vals:

  the `y` value of interst

- selected_vars:

  an object that has selected variables from dropdowns.

- tab:

  the name of the tab.

- nane:

  the object name that has been triggered. It is usually a `reactive`
  object.

## Details

`msg_dropdowns()` returns info on the drop downs selected.

`msg_hist_ui()` provides info on the histogogram

`msg_hist_vars()` provides info on the histogogram variables

`msg_input_source()` returns info on whether info from valid sources is
being used.

`msg_mean_data()` returns info on the mean data object

`msg_summary_data()`returns info about summary data

`msg_selected_vars()`returns info about the selected varialbes

`msg_tab_name()`tab name.
