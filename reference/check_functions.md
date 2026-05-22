# Check functions

These functions check different conditions

## Usage

``` r
check_empty_character(x)

check_sheets(file_path, output)

check_email(x, arg_name = NULL)

check_tab_credentials(user, valid_users)

check_taxonomy_match(input_values, db_values)
```

## Arguments

- x:

  a \`vector“

- file_path:

  the file path supplied by reactive handler

- output:

  shiny object to output too

- arg_name:

  the name of the argument

- user:

  a `vector` containing the username

- valid_users:

  a `vector` containinng valid user

- input_values:

  a `vector` of common and species names in incoming excel sheet.

- db_values:

  a `vector` of common and species naems that are in `tbl_taxonomy`
