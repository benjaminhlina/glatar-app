# Exclude all observer

This function uses an `observerEvent()` to adjust dropdown menus so that
when other selections beside `"All"` is selected it will not allow
`"All"` to be selected until the entire box is cleared.

## Usage

``` r
exclusive_all_observer(input, session, id)
```

## Arguments

- input:

  a server input value.

- session:

  a shiny session.

- id:

  the dropdown object to apply the observer

## Value

an Event that excludes \`"All"“
