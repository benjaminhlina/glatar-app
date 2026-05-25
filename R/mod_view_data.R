#' View Raw Data Tab
#'
#' Provides the raw data tab.
#'
#' @param id the shiny namespace id name (i.e., `"view_data"`).
#'
#' @details `view_data_ui()` provides the view data user interface.
#'
#' @name view_data_module
#' @export

view_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::div(
      id = ns("raw_data_ui"),
      style = "display:none;",
      shiny::h2("Raw Data"),
      shiny::p(
        "This panel displays user specific submitted raw data. Use the theme dropdown to select a theme of data of interest.
        Select the variables of interest and use the other dropdowns to filter the raw data."
      ),
      shiny::fluidRow(
        shinydashboard::box(
          title = "Raw Data Table",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          shiny::uiOutput(ns("no_data_message")),
          shiny::div(
            style = "overflow-x: auto; width: 100%;",
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("raw_data_output")),
              type = 4,
              caption = "Please wait for the table to load..."
            )
          )
        )
      )
    )
  )
}


# ---- server ----
#' @param id the shiny namespace id name (i.e., `"view_data"`).
#' @param con a `DBI` conection to, in this case PostgreSQL database.
#' @param main_input the shiny input from the main server.
#' @param raw_sidebar_vals a `reactiveVal()` object produced by `raw_data_sidebar_server()`.
#' The inputs from this dictate what is displayed in the main panel.
#' @param auth_state the authorization state to login.
#'
#' @details `view_data_server()` provides the raw data tab server.
#'
#' @name view_data_module
#' @export

view_data_server <- function(
  id,
  con,
  main_input,
  raw_sidebar_vals,
  auth_state
) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(
      main_input$tabs,
      {
        shinyjs::toggle(
          id = "raw_data_ui",
          condition = main_input$tabs == "view_data"
        )
      },
      ignoreInit = TRUE
    )

    # ---- namespaces -----
    ns <- session$ns

    # reactive export df
    raw_export_df <- shiny::reactiveVal(NULL)

    # reactive when raw is actived
    raw_activated <- shiny::reactiveVal(FALSE)

    #  ----- first create raw data -----
    # raw actived_true only if
    shiny::observeEvent(
      main_input$tabs,
      {
        shiny::req(auth_state())
        shiny::req(main_input$tabs == "view_data")
        raw_activated(TRUE)
      },
      ignoreInit = TRUE
    )

    # create summary data
    raw_data <- create_raw_data(
      con = con,
      main_input = main_input,
      input_source = raw_sidebar_vals,
      tab = "view_data",
      var_field = "y_variable",
      activated = raw_activated
    )

    # filtered summary by waterbody and species
    filtered_raw_data <- create_filtered_data(
      input_source = raw_sidebar_vals,
      data = raw_data,
      pane = "view_data"
    )

    filtered_raw_data_df_names <- shiny::reactive({
      shiny::req(auth_state())
      shiny::req(filtered_raw_data())
      filtered_raw_data() |>
        dplyr::rename_with(~ convert_nice_name(.x))
    })

    # display_warning(
    #   output = output,
    #   output_id = "no_data_message",
    #   input_source = raw_sidebar_vals
    # )

    output[["no_data_message"]] <- shiny::renderUI({
      if (!isTRUE(raw_sidebar_vals$has_data())) {
        shiny::div(
          class = "alert alert-warning",
          shiny::icon("triangle-exclamation"),
          "No data is available. For your raw data to be viewable on this
          pane, please submit data through the upload pane."
        )
      }
    })

    display_table(
      data = filtered_raw_data_df_names,
      output,
      output_id = "raw_data_output"
    )

    # ---- run exporte -----
    shiny::observe({
      shiny::req(auth_state())
      raw_export_df(filtered_raw_data_df_names())
    })

    # ----- return this so it can be exported -----
    return(list(
      raw_df = filtered_raw_data_df_names
    ))
  })
}
