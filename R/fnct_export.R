#' Exporf functions
#'
#' These functions allow the exportation of different objects usually
#' into excel files.
#'
#' @param input_source is the shiny input source
#' @param file_name file name to be used
#' @param output a shiny output
#' @param output_id a shiny output id e.g., `download_summary`
#' @param tabs tab name to be used e.g., `summary_info`
#' @param id shiny id object
#' @param input a shiny input object
#' @param session a shiny session object
#' @param df_name the name of the df to be exported
#'
#' @name export_functions
#' @export

export_excel <- function(
  input_source,
  file_name,
  output,
  output_id,
  tabs,
  id,
  input,
  session,
  df_name
) {
  output[[output_id]] <- shiny::downloadHandler(
    filename = function() {
      paste0(file_name, Sys.Date(), ".xlsx")
    },
    content = function(file) {
      shiny::req(input_source)
      if (df_name == "summary_df") {
        df <- input_source[[df_name]]()()
      } else {
        df <- input_source[[df_name]]()
      }
      shiny::req(df)
      writexl::write_xlsx(df, file)
    }
  )

  shiny::observe({
    shiny::req(input$tabs == tabs)
    shiny::req(input_source)

    if (df_name == "summary_df") {
      df <- input_source[[df_name]]()()
    } else {
      df <- input_source[[df_name]]()
    }

    shinyjs::toggleState(
      session$ns(output_id),
      condition = !is.null(df) && nrow(df) > 0
    )
  })
}
