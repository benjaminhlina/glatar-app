# Registration Observer

This function uses an `observerEvent()` to provide server side events
either in the login modal or the registration tab.

## Usage

``` r
register_observer(input, output, session, modal = FALSE)
```

## Arguments

- input:

  a server input value.

- output:

  a server output value.

- session:

  a shiny session.

- modal:

  a logical that allows for this function to be used in modal mode.
  Defaults to `FALSE`.

## Value

an event that sends an email with registeration information.
