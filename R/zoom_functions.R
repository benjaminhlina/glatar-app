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
