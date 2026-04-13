# ------ pallletes ------
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
