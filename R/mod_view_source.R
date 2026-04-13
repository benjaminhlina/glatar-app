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

view_source_server <- function(id, con, main_input) {
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
    # source_export_df <- shiny::reactiveVal(NULL)

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
    # shiny::observe({
    #   raw_export_df(filtered_raw_data_df_names())
    # })

    # ----- return this so it can be exported -----
    # return(list(
    #   raw_df = filtered_raw_data_df_names
    # ))
  })
}
