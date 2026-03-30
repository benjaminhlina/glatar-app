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
      shiny::fluidRow(
        shinydashboard::box(
          title = "Source Material Table",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          div(
            style = "overflow-x: auto; width: 100%;",
            
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("source_output")), 
              type = 4, caption = "Please wait for table to load..."
          )
        )
      )
    )
  )
}

view_source_server <- function(id, main_input, con, source_sidebar_vals) {
  shiny::moduleServer(id, function(input, output, session) {
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
    ns <- session$ns

    # reactive export df
    source_export_df <- shiny::reactiveVal(NULL)

    # reactive when raw is actived
    source_activated <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(
      main_input$tabs,
      {
        shiny::req(main_input$tabs == "view_source")
        source_activated(TRUE)
      },
      ignoreInit = TRUE
    )

  })
}
