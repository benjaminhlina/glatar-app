# ----- update slider ------
#' Update Zoom Slider
#'
#' This updates the zoom sliders
#'
#' @param id the shiny id used e.g., `zoom_x`.
#' @param vec a `vector` containing the minamum and maximum values.
#'
#' @return  an updated Input object for a given `zoom_*` object.
#'
#' @export

update_zoom_slider <- function(id, vec) {
  shiny::updateSliderInput(
    session = shiny::getDefaultReactiveDomain(),
    inputId = id,
    min = floor(vec[1]),
    max = ceiling(vec[2]),
    value = c(floor(vec[1]), ceiling(vec[2]))
  )
}
