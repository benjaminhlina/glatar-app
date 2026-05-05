# Scatter Plot Sidebar

Provides the sidebar for the scatter plot pane.

Provides the scatter plot tab.

## Usage

``` r
scatter_sidebar_ui(id)

scatter_sidebar_server(id, con, main_input)

view_scatter_plot_ui(id)

scatter_plot_server(id, con, main_input, scatter_sidebar_vals)
```

## Arguments

- id:

  the shiny namespace id name (i.e., `"scatter_plot"`).

- con:

  a `DBI` conection to, in this case PostgreSQL database.

- main_input:

  the shiny input from the main server.

- scatter_sidebar_vals:

  a `reactiveVal()` object produced by `scatter_sidebar_server()`. The
  inputs from this dictate what is displayed in the main panel.

## Details

`scatter_sidebar_ui()` that provides user interface for scatter plot
sidebar.

`scatter_sidebar_server()` provides the scatter plot sidebar server.

`view_scatter_plot_ui()` provides the scatter plot user interface.

`scatter_plot_server()` provides the scatter plot server.
