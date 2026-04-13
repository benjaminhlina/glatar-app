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
      activated = raw_activated()
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
