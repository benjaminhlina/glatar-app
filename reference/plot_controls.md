# Plot control functions

These functions add different control features to different plots
displayed such as scatter and eventually box plot.

## Usage

``` r
plot_ui(title, plot_id, height, ...)

pallete_selector(...)

alpha_selector(...)

size_selector(...)

shape_selector(...)
```

## Arguments

- title:

  the title of plot

- plot_id:

  the shiny namespace for the plot desired `"scatter_plot"`

- height:

  the height of the plot in pixels

- ...:

  additional parameters to be given for example a `NS()` object.

## Details

`plot_ui()` provides user interface for any plot object throughout the
app.

`pallete_selector()` provides user interface to select different
palletes from [viridis](https://CRAN.R-project.org/package=viridis).

`alpha_selector()` provides user interface to select different values of
`alpha` which controls transparency.

`size_selector()` provides user interface to select different values of
`size` which controls the size of the points.

`shape_selector()` provides user interface to select different values of
`shapes` which controls the shape type.
