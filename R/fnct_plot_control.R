# ----- plot ui -----
#' Plot control functions
#'
#' These functions add different control features to different
#' plots displayed such as scatter and eventually box plot.
#'
#' @param title the title of plot
#' @param plot_id the shiny namespace for the plot desired `"scatter_plot"`
#' @param height the height of the plot in pixels
#' @param ... additional parameters to be given for example a `NS()` object.
#'
#' @return `plot_ui()` provides user interface for any plot object throughout
#' the app.
#'
#' @name plot_controls
#' @export

plot_ui <- function(title, plot_id, height, ...) {
  args <- list(...)
  ns <- args$ns

  shiny::fluidRow(
    shinydashboard::box(
      title = title,
      status = "info",
      solidHeader = TRUE,
      width = 12,
      shinycssloaders::withSpinner(
        shiny::plotOutput(ns(plot_id), height = height),
        type = 4,
        caption = "Please wait for the plot to load..."
      )
    )
  )
}


# ------ pallletes ------
#' @param ... additional parameters to be given for example a `NS()` object.
#'
#' @return `pallete_selector()` provides user interface to select different palletes
#' from [viridis](https://CRAN.R-project.org/package=viridis).
#'
#'
#' @name plot_controls
#' @export
pallete_selector <- function(...) {
  args <- list(...)
  ns <- args$ns

  shiny::selectInput(
    inputId = ns("viridis_palette"),
    label = "Colour Palette",
    choices = c(
      "Viridis" = "D",
      "Magma" = "A",
      "Inferno" = "B",
      "Plasma" = "C",
      "Cividis" = "E",
      "Rocket" = "F",
      "Mako" = "G",
      "Turbo" = "H"
    ),
    selected = "B"
  )
}

pallete_selector_server <- function(input) {
  list(
    viridis_palette = shiny::reactive(input$viridis_palette)
  )
}


# ------ alpha -------
#' @param ... additional parameters to be given for example a `NS()` object.
#'
#' @return `alpha_selector()` provides user interface to select different values
#' of `alpha` which controls transparency.
#'
#' @name plot_controls
#' @export
alpha_selector <- function(...) {
  args <- list(...)
  ns <- args$ns
  shiny::tagList(
    shiny::sliderInput(
      inputId = ns("alpha"),
      label = "Transparency",
      min = 0,
      max = 1,
      value = 0.7,
    ),
    shiny::div(
      style = "
        display: flex; 
        justify-content: space-between; 
        margin-top: -10px;
      ",
      shiny::span("Transparent"),
      shiny::span(style = "margin-right: 150px;", "Opaque")
    )
  )
}


alpha_selector_server <- function(input) {
  list(
    alpha = shiny::reactive(input$alpha)
  )
}


# ------ size -------
#' @param ... additional parameters to be given for example a `NS()` object.
#'
#' @return `size_selector` provides user interface to select different values
#' of `sizd` which controls the size of the points.
#'
#' @name plot_controls
#' @export
size_selector <- function(...) {
  args <- list(...)
  ns <- args$ns

  shiny::tagList(
    shiny::sliderInput(
      inputId = ns("size"),
      label = "Size",
      min = 0.5,
      max = 10,
      value = 5
    ),
    shiny::div(
      style = "
        display: flex; 
        justify-content: space-between; 
        margin-top: -10px;
      ",
      shiny::span("Small"),
      shiny::span(style = "margin-right: 150px;", "Big")
    )
  )
}


size_selector_server <- function(input) {
  list(
    size = shiny::reactive(input$size)
  )
}


# ---- shape -----
#' @param ... additional parameters to be given for example a `NS()` object.
#'
#' @return `shape_selector` provides user interface to select different values
#' of `shapes` which controls the shape type.
#'
#' @name plot_controls
#' @export
shape_selector <- function(...) {
  args <- list(...)
  ns <- args$ns

  shiny::selectInput(
    inputId = ns("shape"),
    label = "Shape",
    choices = c(
      "Circle" = 21,
      "Square" = 22,
      "Diamond" = 23,
      "Triangle" = 24
    ),
    selected = 21
  )
}

shape_selector_server <- function(input) {
  list(
    shape = shiny::reactive(input$shape)
  )
}
