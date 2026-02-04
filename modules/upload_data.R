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

    # ---- reactive validate ------
    validated_samples <- reactiveVal(NULL)
    validated_source <- reactiveVal(NULL)
    validated_submission <- reactiveVal(NULL)

    observeEvent(input$upload_btn, {

      # ---- get file upload -----
      req(input$file_upload)

      shinyjs::disable("submit_btn")

      # get path
      file_path <- input$file_upload$datapath
      # get sheets
      sheets <- readxl::excel_sheets(file_path)

      # check if all sheets are there
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

      tbl_submission_submitted <- readxl::read_excel(
        file_path,
        sheet = "tbl_submission",
        skip = 3,
      ) |>
        janitor::clean_names()

      agent_submission <- validate_tbl_submission(tbl_submission_submitted)

      # ----- validate tbl_soruce -----
      tbl_source_submitted <- readxl::read_excel(
        file_path,
        sheet = "tbl_sources",
        skip = 3
      ) |>
        janitor::clean_names() |>
        rename_to_db_col(con, "tbl_source")


      agent_source <- validate_tbl_source(tbl_source_submitted)

      # ----- validate tbl sample ----

      col_types <- c(
        rep("guess", 3),
        "date",
        rep("guess", 48)
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
        rename_to_db_col(con, "tbl_location")

      # ----- get species list -----
      species_list <- tbl(con, "tbl_taxonomy")

      # ---- add valid taxoonmy -----
      tbl_samples_submitted <- add_valid_taxonomy(tbl_samples_submitted,
                                                  species_list)

      num_cols <- c(
        "length_mm","weight_g","age","composite_n","latitude","longitude",
        "calorimeter_conversion_factor","sample_weight","energy_measurement",
        "percent_water","percent_ash","percent_lipid","percent_protein",
        "percent_carbon","percent_nitrogen","d13c","d15n","d34s","c_n"
      )

      tbl_samples_submitted <- tbl_samples_submitted |>
        mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.))))

      # ---- run validtor validation ----
      agent_sample <- validate_tbl_samples(tbl_samples_submitted)




      if (all(agent_submission) && all(agent_source) && all(agent_sample)) {

        # ---- make all of them reactive vals -----
        validated_submission(tbl_samples_submitted)
        validated_source(tbl_source_submitted)
        validated_samples(tbl_samples_submitted)

        # ----- get the next submission id ------
        next_submission_id <- DBI::dbGetQuery(
          con,
          glue::glue("SELECT gen_random_uuid() AS next_id")
        )


        # ---- splap submisison id on to source and submission -----
        tbl_submission_submitted <- tbl_submission_submitted |>
          mutate(submission_id = next_submission_id$next_id)

        tbl_source_submitted <- tbl_source_submitted |>
          mutate(submission_id = next_submission_id$next_id)
        tables_ids <- dbGetQuery(con, "
        SELECT table_name, column_name
        FROM information_schema.columns
        WHERE table_schema = 'public'
        AND column_name LIKE '%_id'
        AND table_name <> 'tbl_submission'") |>
          filter(!column_name %in%  c("submission_id",
                                      "percent_lipid"))



        # filter(table_name %in% c("tbl_samples", "tbl_source", "tbl_length", "tbl_location"))
        max_ids <- purrr::pmap(
          tables_ids,
          ~ id_max(..1, ..2)
        ) |>
          set_names(tables_ids$column_name)

        max_ids
        # ------ get tables to split -----
        tables_to_split <- get_column_map(con) |>
          filter(!table_name %in% c("tbl_source", "tbl_submission")) |>
          collect() |>
          (\(.) split(., .$table_name))()

        # ----- do the same for source id -----
        tbl_source_submitted <- tbl_source_submitted |>
          mutate(.source_id = seq(from = max_ids[["source_id"]] + 1,
                                  length.out = n()))

        # ----- we need to do a couple things to tbl_submission before submitting
        tbl_samples_submitted <- tbl_samples_submitted |>
          mutate(
            submission_id = next_submission_id$next_id,
            sample_id = seq(from = max_ids[["sample_id"]] + 1,
                            length.out = n()),
            across(common_name:family, ~  stringr::str_to_sentence(.x)),
            .energy_units = paste(energy_units,
                                  sample_weight_type, "weight", sep = " ")
          ) |>
          select(-energy_units) |>
          rename(
            energy_units = .energy_units
          )


        # ---- add source id based on user supplied id ------
        tbl_samples_submitted <- tbl_samples_submitted |>
          left_join(
            tbl_source_submitted |>
              select(source_id, .source_id)
          ) |>
          select(-source_id) |>
          rename(source_id = .source_id)


        # etc for other tables


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

        validated_submission(NULL)
        validated_source(NULL)
        validated_samples(NULL)

        error_report <- clean_all_validations(
          tbl_submssion = agent_submission,
          tbl_source = agent_source,
          tbl_samples = agent_sample
        )

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

      req(validated_submission())
      req(validated_source())
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
