# Plot control functions

These functions add different control features to different plots
displayed such as scatter and eventually box plot.

## Usage

``` r
plot_ui(title, plot_id, height, ...)

pallete_selector(...)

pallete_selector_server(input)

alpha_selector(...)

alpha_selector_server(input)

size_selector(...)

size_selector_server(input)

shape_selector(...)

shape_selector_server(input)

zoom_slider_ui(ns, id, label)

zoom_slider_server(input)
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

- input:

  a shiny server input value.

- ns:

  a shiny namespace object e.g., `ns`.

- id:

  a shiny namespace object e.g., `"zoom_x"`.

- label:

  a shiny namespace object e.g., `"Zoom X Axis"`.

## Details

`plot_ui()` provides user interface for any plot object throughout the
app.

`pallete_selector()` provides user interface to select different
palletes from [viridis](https://CRAN.R-project.org/package=viridis).

`pallete_selector_server()` provides the server and reactive value to
select different palletes from
[viridis](https://CRAN.R-project.org/package=viridis).

`alpha_selector()` provides user interface to select different values of
`alpha` which controls transparency.

`alpha_selector_server()` provides the server and reactive value to
select different values of `alpha` which controls transparency.

`size_selector()` provides user interface to select different values of
`size` which controls the size of the points.

`size_selector_server()` provides the server and reactive values of
`size` which controls the size of the points.

`shape_selector()` provides user interface to select different values of
`shapes` which controls the shape type.

`shape_selector_server()` provides the server and reactive values of
`size` which controls the size of the points.

`zoom_slider_ui()` provides the the user interface for zoom slider on
the plot panel.

`zoom_slider_server()` provides the server and reactive values of zoom
slider which controls the zoom on the x and y axis.
