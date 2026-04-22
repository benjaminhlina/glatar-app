# Check functions

These functions check different conditions

## Usage

``` r
check_empty_character(x)

check_sheets(file_path, output)

check_tab_credentials(user, pass, credentials)

check_taxonomy_match(input_values, db_values)
```

## Arguments

- x:

  a \`vector

- file_path:

  the file path supplied by reactive handler

- output:

  shiny object to output too

- user:

  a `vector` containing the username

- pass:

  a `vector` containing the password

- credentials:

  a `dataframe` containing username and password

- input_values:

  a `vector` of common and species names in incoming excel sheet.

- db_values:

  a `vector` of common and species naems that are in `tbl_taxonomy`
