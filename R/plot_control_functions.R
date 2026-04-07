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
