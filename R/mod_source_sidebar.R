#' Source Table Sidebar
#'
#' Provides the sidebar for the source table pane.
#'
#' @param id the shiny namespace id name (i.e., `"source_sidebar_ui"`).
#'
#' @details `source_sidebar_ui()` provides the summary table sidebar user interface.
#'
#' @name source_sidebar_module
#' @export

source_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      id = ns("source_ui"),
      style = "display:none;",
      shiny::downloadButton(
        ns("download_source"),
        "Download Source as Excel",
        class = "btn-primary",
        style = "margin-left: 15px; margin-top: 10px;
                                width: 245px"
      )
    )
  )
}

# ----- server -----
#' @param id the shiny namespace id name (i.e., `"source_sidebar"`).
#' @param main_input the shiny input from the main server
#'
#' @details `source_sidebar_server()` provides the summary table sidebar server.
#'
#' @name source_sidebar_module
#' @export
source_sidebar_server <- function(id, main_input, source_sidebar_vals) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      shinyjs::toggle(
        id = "source_ui",
        condition = main_input$tabs == "view_source"
      )
    })
    # make this into a function that sidebar exports out
    register_source <- function(input_source) {
      export_excel(
        input_source = input_source,
        file_name = "glatar_source_tbl_",
        output = output,
        output_id = "download_source",
        tabs = "view_source",
        id = id,
        input = input,
        session = session,
        df_name = "source_df"
      )
    }

    # ----- export what we need from the server ----
    # we need grouping and hist variables we also need the function

    return(list(
      register_source = register_source
    ))
  })
}
