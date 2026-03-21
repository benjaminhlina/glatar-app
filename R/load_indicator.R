load_indicator <- function(input, output) {
  shiny::req(input$file_upload)
  shinyjs::show("loading_indicator")
  output$loading_msg <- shiny::renderText("Processing file, please wait...")
  shinyjs::disable("upload_btn")

  on.exit({
    shinyjs::hide("loading_indicator")
    shinyjs::enable("upload_btn")
  })
}
