upload_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = id,
                          shiny::h2("Upload Excel File"),

                          shiny::fileInput(ns("file_upload"),
                                           "Choose an Excel File",
                                           accept = c(".xlsx")),

                          shiny::actionButton(ns("upload_btn"),
                                              "Upload & Process",
                                              icon = icon("upload")),

                          shinyjs::useShinyjs(),

                          shiny::actionButton(
                            ns("submit_btn"),
                            "Submit to Database",
                            icon = icon("database"),
                            disabled = TRUE
                          ),

                          shiny::uiOutput(ns("upload_status"))
  )
}


upload_data_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    validated_samples <- reactiveVal(NULL)

    observeEvent(input$upload_btn, {

      req(input$file_upload)

      shinyjs::disable("submit_btn")

      file_path <- input$file_upload$datapath
      sheets <- readxl::excel_sheets(file_path)

      if (!all(c("tbl_samples", "tbl_sources") %in% sheets)) {
        output$upload_status <- renderUI({
          p("Error: Missing required sheets.",
            style = "color:red;")
        })
        return()
      }

      tbl_samples <- readxl::read_excel(
        file_path,
        sheet = "tbl_samples",
        skip = 4
      ) |>
        janitor::clean_names()


      # ---- run pointblank validation ----
      agent <- validate_tbl_samples(tbl_samples)

      if (all_passed(agent)) {

        validated_samples(tbl_samples)

        shinyjs::enable("submit_btn")

        output$upload_status <- renderUI({
          tagList(
            p("✔ All validations passed",
              style = "color:green; font-weight:600;"),
            p(paste0("Ready to submit ", nrow(tbl_samples), " rows to database."),
              style = "color:green;")
          )
        })

      } else {

        validated_samples(NULL)

        output$upload_status <- renderUI({
          tagList(
            p("✖ Validation failed",
              style = "color:red; font-weight:600;"),
            shiny::tableOutput(ns("error_table"))
          )
        })

        output$error_table <- shiny::renderTable({
          error_report
        })
      }
    })

    # ---- submit to database ----
    observeEvent(input$submit_btn, {

      req(validated_samples())

      DBI::dbAppendTable(
        con,
        "tbl_samples",
        validated_samples()
      )

      output$upload_status <- renderUI({
        p("✔ Data successfully appended to database",
          style = "color:green; font-weight:600;")
      })

      shinyjs::disable("submit_btn")
    })
  })
}
