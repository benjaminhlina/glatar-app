# ----- load oinidciator -----

#' Load Indicator functions
#'
#'
#' @param input a shiny `server` input value.
#' @param output a shiny `server` output value.
#'
#' @details
#' `load_indicator()` displays load indicator during file uploading
#'
#' @name load_indicator
#' @export

load_indicator <- function(input, output) {
  shiny::req(input$file_upload)
  shinyjs::show("loading_indicator")
  shinyjs::html("loading_msg", "Processing file, please wait...")
  shinyjs::disable("upload_btn")
}

# ----- hide ------
#' @param input a shiny `server` input value.
#' @param output a shiny `server` output value.
#'
#' @details
#' `load_indicator_hide()` displays load indicator during file uploading
#'
#' @name load_indicator
#' @export
load_indicator_hide <- function(input, output) {
  shinyjs::hide("loading_indicator")
  shinyjs::html("loading_msg", "")
  shinyjs::enable("upload_btn")
}
