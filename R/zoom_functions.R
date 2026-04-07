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

# ----- update slider ------
update_zoom_slider <- function(id, vec) {
  shiny::updateSliderInput(
    session = shiny::getDefaultReactiveDomain(),
    inputId = id,
    min = floor(vec[1]),
    max = ceiling(vec[2]),
    value = c(floor(vec[1]), ceiling(vec[2]))
  )
}

syn_var <- function() {
  syn_var <- list(
    "length_mm__total" = "length_mm",
    "length_mm__fork" = "length_mm",
    "length_mm__standard" = "length_mm",
    "energy_units__Joules/g wet weight" = "energy_measurement",
    "energy_units__Joules/g dry weight" = "energy_measurement"
  )
  return(syn_var)
}
