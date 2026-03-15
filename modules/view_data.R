view_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    useShinyjs(),
    div(
      id = ns("raw_data_ui"),
      style = "display:none;",
      shiny::h2("Raw Data"),
      shiny::p(
        "This panel displays raw data."
      ),
      shiny::fluidRow(
        shinydashboard::box(
          title = "Raw Data Table",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          div(
            style = "overflow-x: auto; width: 100%;",
            DT::DTOutput(ns("raw_data_output"))
          )
        )
      )
    )
  )
}


# ---- server ----
view_data_server <- function(id, con, main_input, raw_sidebar_vals) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
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

    #   # Get all table names and update selectInput dynamically
    #   observe({
    #     table_names <- DBI::dbListTables(con)
    #     table_names <- setdiff(table_names, "tbl_submission")
    #     updateSelectInput(session, "table_select", choices = table_names)
    #   })
    #   # Render the selected table
    #   output$selected_table <- DT::renderDT(
    #     {
    #       allowed_tables <- c(
    #         "tbl_amino_acid",
    #         "tbl_calorimetry",
    #         "tbl_contaminants",
    #         "tbl_data_dictionary",
    #         "tbl_fatty_acid",
    #         "tbl_isotope",
    #         "tbl_length",
    #         "tbl_lipid_composition",
    #         "tbl_location",
    #         "tbl_proxcomp",
    #         "tbl_samples",
    #         "tbl_source",
    #         "tbl_taxonomy",
    #         "tbl_thiamine"
    #       )
    #       req(input$table_select %in% allowed_tables)
    #       safe_name <- DBI::dbQuoteIdentifier(con, input$table_select)
    #       DBI::dbGetQuery(con, paste0("SELECT * FROM ", safe_name))
    #     },
    #     options = list(
    #       pageLength = 15,
    #       scrollX = TRUE
    #     )
    #   )
  })
}
