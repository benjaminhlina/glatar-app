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

      required_sheets <- c("tbl_submission", "tbl_sources", "tbl_samples")
      missing_sheets  <- setdiff(required_sheets, sheets)

      if (length(missing_sheets) > 0) {

        output$upload_status <- renderUI({
          p(
            paste0(
              "✖ Error: Missing required sheet(s): ",
              paste(missing_sheets, collapse = ", ")
            ),
            style = "color:red; font-weight:600;"
          )
        })

        return()
      }
      # ----- start validation processes ------

      # ----- validate tbl_submission ------
      source_col_type <- rep("text", 3)

      col_types <-  c(rep("text", 3),
                      "date", "numeric", "text",
                      "numeric", rep('text', 10),
                      rep("numeric", 2), "text", "numeric",
                      rep("text", 2), "numeric",
                      rep("text", 6), rep("numeric", 3), "text",
                      rep("numeric", 2), 'text', 'numeric',
                      "text", rep('numeric', 2),
                      rep("text", 2), rep('numeric', 10)
      tbl_source_submitted <- readxl::read_excel(
        file_path,
        sheet = "tbl_submission",
        skip = 3,
        col_types = source_col_type
      ) |>
        janitor::clean_names()
      agent_submission <- validate_tbl_submission(tbl_source_submitted)
      )

      # ---- get tbl sample -----
      tbl_samples_submitted <- readxl::read_excel(
        file_path,
        sheet = "tbl_samples",
        col_types = col_types,
        skip = 4
      ) |>
        janitor::clean_names() |>
        rename_to_db_col(con, "tbl_samples") |>
        rename_to_db_col(con, "tbl_location") |>


      # ----- get species list -----
      species_list <- tbl(con, "tbl_taxonomy")

      # ---- add valid taxoonmy -----
      tbl_samples_submitted <- add_valid_taxonomy(tbl_samples_submitted,
                                                  species_list)

      # ---- run validtor validation ----
      agent <- validate_tbl_samples(tbl_samples_submitted)

      if (all(agent)) {

        validated_samples(tbl_samples_submitted)

        tbl_samples_submitted <- tbl_samples_submitted |>
          mutate(
            across(common_name:class_sci, ~  stringr::str_to_sentence(.x)),
            .energy_units = paste(energy_units,
                                  sample_weight_type, "weight", sep = " ")
          ) |>
          select(-energy_units) |>
          rename(
            energy_units = .energy_units
          )



        shinyjs::enable("submit_btn")

        output$upload_status <- renderUI({
          tagList(
            p("✔ All validations passed",
              style = "color:green; font-weight:600;"),
            p(paste0("Ready to submit ", nrow(tbl_samples_submitted),
                     " rows to database."),
              style = "color:green;")
          )
        })

      } else {

        validated_samples(NULL)

        error_report <- pretty_validate_report(agent)

        output$upload_status <- renderUI({
          tagList(
            p("✖ Validation failed - please fix the following issues:",
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
