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

                          shiny::uiOutput(ns("upload_status")),
                          shiny::uiOutput(ns("location_map"))

  )
}


upload_data_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ---- reactive validate ------
    validated_samples <- reactiveVal(NULL)
    validated_source <- reactiveVal(NULL)
    validated_submission <- reactiveVal(NULL)
    tables_to_submit<- reactiveVal(NULL)
    tables_split_full <- reactiveVal(NULL)

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

      # ----- first get the number of columns -----
      col_count <- ncol(readxl::read_excel(
        file_path,
        sheet = "tbl_samples",
        skip = 4,
        n_max = 1
      ))


      # ---- then dynamically create col_types
      col_types <- c(
        rep("guess", 3),
        "date",
        rep("guess", col_count - 4)
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
        "length_mm","weight_g","age_year","composite_n","latitude","longitude",
        "calorimeter_conversion_factor","sample_weight","energy_measurement",
        "percent_water","percent_ash","percent_lipid","percent_protein",
        "percent_carbon","percent_nitrogen","d13c","d15n","d34s","c_n"
      )

      # num_col <- get_column_map(con) |>
      #   filter(field_class %in% c("integer", "numeric")) |>
      #   select(field_name) |>
      #   arrange(field_name) |>
      #   pull()

      tbl_samples_submitted <- tbl_samples_submitted |>
        mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.))))

      # ---- run validtor validation ----
      agent_sample <- validate_tbl_samples(tbl_samples_submitted)


      cli::cli_h2("Agent validation checks")

      log_agent(agent_submission, "agent_submission")
      log_agent(agent_source, "agent_source")
      log_agent(agent_sample, "agent_sample")

      ok_submission <- all(unlist(values(agent_submission)), na.rm = TRUE)
      ok_source <- all(unlist(values(agent_source)), na.rm = TRUE)
      ok_sample <- all(unlist(values(agent_sample)),  na.rm = TRUE)

      cli::cli_alert_info(
        "Gate status to submission: {ok_submission},
        source: {ok_source}, sample: {ok_sample}"
      )

      # if all agents are good process and get ready to submitt
      if (isTRUE(ok_submission) && isTRUE(ok_source) && isTRUE(ok_sample)) {


        # ---- make all of them reactive vals -----
        validated_submission(tbl_samples_submitted)
        validated_source(tbl_source_submitted)
        validated_samples(tbl_samples_submitted)

        # ----- get the next submission id ------
        next_submission_id <- DBI::dbGetQuery(
          con,
          glue::glue("SELECT gen_random_uuid() AS next_id")
        )

        cli::cli_alert_info("Submission id is: \
                            {.val {next_submission_id$next_id}}")


        # ---- splap submisison id on to source and submission -----
        tbl_submission_submitted <- tbl_submission_submitted |>
          mutate(submission_id = next_submission_id$next_id)

        tbl_source_submitted <- tbl_source_submitted |>
          mutate(submission_id = next_submission_id$next_id)

        # ---- pull column names that are all have id
        tables_ids <- dbGetQuery(con, "
        SELECT table_name, column_name
        FROM information_schema.columns
        WHERE table_schema = 'public'
        AND column_name LIKE '%_id'
        AND table_name <> 'tbl_submission'") |>
          filter(!column_name %in% c("submission_id",
                                     "percent_lipid",
                                     "user_sample_id")
          )


        cli::cli_alert_info("Columns that have id are: {.val {tables_ids}}")

        # ---- get max id for each -----
        max_ids <- purrr::pmap(
          tables_ids,
          ~ id_max(..1, ..2)
        ) |>
          set_names(tables_ids$column_name)

        max_ids_str <- paste(names(max_ids), unlist(max_ids), sep = " = ",
                             collapse = ", ")
        cli::cli_alert_info("Next id for each starts here: {max_ids_str}")
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
            length_type = tolower(length_type),
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



        tbl_samples_submitted <- tbl_samples_submitted |>
          left_join(species_list |>
                      collect())

        # doo the same to source -----
        tbl_source_submitted <- tbl_source_submitted |>
          select(-source_id) |>
          rename(source_id = .source_id)

        # ----- split by table name ----

        tables_split <- lapply(names(tables_to_split), function(tbl_name) {

          map  <- tables_to_split[[tbl_name]]
          cols <- map$field_name

          # --- select only payload columns first
          payload <- tbl_samples_submitted |>
            dplyr::select(any_of(cols))

          # --- does this table actually have data (ignore sample_id)?
          has_data <- nrow(payload) > 0 && any(!is.na(as.matrix(payload)))

          if (has_data) {

            # --- always include sample_id
            id_cols <- c("sample_id")

            # --- add submission_id only for tbl_samples
            if (tbl_name == "tbl_samples") {
              id_cols <- c(id_cols, "submission_id")
            }

            # --- attach IDs while preserving row order
            out <- dplyr::bind_cols(
              tbl_samples_submitted |>
                dplyr::select(any_of(id_cols)),
              payload
            )

            cli::cli_alert_success("Using data for table: {tbl_name}")
            out

          } else {
            cli::cli_alert_info("Skipping empty table: {tbl_name}")
            NULL
          }
        }) |>
          set_names(names(tables_to_split)) |>
          purrr::compact()

        # --- add in ids -----

        tables_split_full <- assign_table_ids(
          tables_split,
          tables_ids,
          max_ids
        )

        tables_split_full$tbl_source <- tbl_source_submitted
        tables_split_full$tbl_submission <- tbl_submission_submitted


        # Define the "priority" tables to go first
        priority_tables <- c("tbl_submission", "tbl_source", "tbl_samples")

        # Find which priority tables exist in your list
        existing_priority <- intersect(priority_tables, names(tables_split_full))

        # Get remaining tables
        other_tables <- setdiff(names(tables_split_full), existing_priority)

        # Combine: priority first, then the rest
        tables_ordered <- c(existing_priority, other_tables)

        # Reorder your list
        tables_split_full <- tables_split_full[tables_ordered]

        # Optional: check order
        cli::cli_alert_info("Tables will be submitted in this order:
                            {paste(names(tables_split_full), collapse = ' -> ')}")
        # etc for other tables
        tables_to_submit(tables_split_full)

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
        tables_to_submit(NULL)

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
          tables_split_full(NULL)

    # ---- submit to database ----
    observeEvent(input$submit_btn, {

      req(validated_submission())
      req(validated_source())
      req(validated_samples())
      req(tables_to_submit())

      # --- assume tables_split_full is a named list ---
      tables_to_submit <- tables_to_submit()

      # Create a list to store submission results
      submission_results <- list()

      for(tbl_name in names(tables_to_submit)) {
        df <- tables_to_submit[[tbl_name]]

        if(nrow(df) > 0) {
          # Append table to database
          cli::cli_alert_info("Submitting table: {tbl_name} with {nrow(df)} rows...")

          DBI::dbAppendTable(con, tbl_name, df)
          cli::cli_alert_success("{tbl_name} submitted successfully")

          # Store result info
          submission_results[[tbl_name]] <- list(
            rows_submitted = nrow(df),
            submission_id = if("submission_id" %in% colnames(df)) unique(df$submission_id) else NA
          )
        } else {
          submission_results[[tbl_name]] <- list(
            rows_submitted = 0,
            submission_id = NA
          )
          cli::cli_alert_info("{tbl_name} has no rows to submit, skipping.")
        }
      }

      # Create a message to display
      msg <- lapply(names(submission_results), function(tbl_name) {
        res <- submission_results[[tbl_name]]
        paste0("✔ ", tbl_name, ": ", res$rows_submitted,
               " rows submitted",
               if(!is.na(res$submission_id)) paste0(", submission_id = ", res$submission_id) else "")
      })

      output$upload_status <- renderUI({
        HTML(
          paste0(
            "<span style='color: green;'>",
            paste(msg, collapse = "<br>"),
            "</span>"
          )
        )
      })

      shinyjs::disable("submit_btn")
    })
  })
}
