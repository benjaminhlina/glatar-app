# ----- load oinidciator -----
load_indicator <- function(input, output) {
  shiny::req(input$file_upload)
  shinyjs::show("loading_indicator")
  shinyjs::html("loading_msg", "Processing file, please wait...")
  shinyjs::disable("upload_btn")
}

# ----- hide ------
load_indicator_hide <- function(input, output) {
  shinyjs::hide("loading_indicator")
  shinyjs::html("loading_msg", "")
  shinyjs::enable("upload_btn")
}
