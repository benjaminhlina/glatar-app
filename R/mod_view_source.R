#' Source Tab
#'
#' Provides the source tab.
#'
#' @param id the shiny namespace id name (i.e., `"view_source"`).
#'
#' @details `view_source_ui()` provides the source user interface.
#'
#' @name source_module
#' @export

view_source_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::div(
      id = ns("source_ui"),
      style = "display:none;",
      shiny::h2("Source Material"),
      shiny::p(
        "This panel displays information on source material supporting the data that is in GLATAR."
      ),
      shiny::textInput(
        ns("search_bar"),
        label = "Search Source Material",
        placeholder = "Type to filter source material..."
      ),
      shinycssloaders::withSpinner(
        DT::DTOutput(ns("source_output")),
        type = 4,
        caption = "Please wait for the table to load..."
      )
    )
  )
}

# ----- server -----
#' @param id the shiny namespace id name (i.e., `"view_source"`).
#' @param con a `DBI` conection to, in this case PostgreSQL database.
#' @param main_input the shiny input from the main server.
#' @param source_sidebar_vals a `reactiveVal()` object produced by `source_sidebar_server()`.
#' The inputs from this dictate what is displayed in the main panel.
#'
#' @details `view_source_server()` provides the source server.
#'
#' @name source_module
#' @export

view_source_server <- function(id, con, main_input, source_sidebar_vals) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ----- add in ui ------
    shiny::observeEvent(
      main_input$tabs,
      {
        shinyjs::toggle(
          id = "source_ui",
          condition = main_input$tabs == "view_source"
        )
      },
      ignoreInit = TRUE
    )

    # add activation
    source_activated <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(
      main_input$tabs,
      {
        shiny::req(main_input$tabs == "view_source")
        source_activated(TRUE)
      },
      ignoreInit = TRUE
    )

    # reactive export df
    source_export_df <- shiny::reactiveVal(NULL)

    # # create summary data
    source_data <- create_source_data(
      con = con,
      main_input = main_input,
      # input_source = source_sidebar_vals,
      tab = "view_source",
      activated = source_activated()
    )
    observe({
      cli::cli_alert_info("Class of source: {.val {class(source_data())}}")
    })

    filtered_source <- create_searching_data(
      input = input,
      source_data = source_data,
      collect = FALSE
    )

    observe({
      cli::cli_alert_info(
        "Class of filtered data: {.val {class(filtered_source())}}"
      )
    })
    display_table(
      data = filtered_source,
      output,
      output_id = "source_output",
      search = FALSE
    )

    # # ---- run exporte -----
    shiny::observe({
      source_export_df(filtered_source())
    })

    # ----- return this so it can be exported -----
    return(list(
      source_df = filtered_source
    ))
  })
}
