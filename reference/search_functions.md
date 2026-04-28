# Search functions

Search functions allow for a dynamic search bar to be displayed to
search taxa and source material

## Usage

``` r
search_table(df, search_term, search_cols)
```

## Arguments

- df:

  the data object being supplied can either be a `tbl_lazy` or
  `data.frame`.

- search_term:

  a dynamic string that contains search terms

- search_cols:

  a dynamic `vector` containing the names of the search columns

## Value

returns a filtered data object that can then be displayed.
