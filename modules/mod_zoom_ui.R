# ----- axis zoom -----
zoom_slider_ui <- function(ns, id, label) {
  shiny::sliderInput(
    inputId = ns(id),
    label = label,
    min = 0,
    max = 100,
    value = c(0, 100)
  )
}

zoom_slider_server <- function(input) {
  list(
    zoom_x = shiny::reactive(input$zoom_x),
    zoom_y = shiny::reactive(input$zoom_y)
  )
}
