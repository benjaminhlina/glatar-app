# Exporf functions

These functions allow the exportation of different objects usually into
excel files.

## Usage

``` r
export_excel(
  input_source,
  file_name,
  output,
  output_id,
  tabs,
  id,
  input,
  session,
  df_name
)
```

## Arguments

- input_source:

  is the shiny input source

- file_name:

  file name to be used

- output:

  a shiny output

- output_id:

  a shiny output id e.g., `download_summary`

- tabs:

  tab name to be used e.g., `summary_info`

- id:

  shiny id object

- input:

  a shiny input object

- session:

  a shiny session object

- df_name:

  the name of the df to be exported
