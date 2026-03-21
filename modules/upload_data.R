upload_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = id,
    shiny::h2("Upload Excel File"),
    shiny::p(
      "This panel allows you to upload data to the GLATAR database.
                   First, select the Excel file you would like to upload. Then click
                   'Upload & Process'. The system will run a series of validation checks and
                   process the data to adhere to database standards. Validation checks will
                   notify you of any issues, including the specific rows, columns,
                   and descriptions of the errors.

                   If the upload is successful, a confirmation message will appear and an
                   interactive map will be displayed so you can verify that your sampling
                   locations are correct. Once you have reviewed the map, click the
                   'Submit to Database' button to finalize the upload.
                   A successful submission will generate a confirmation message (displayed
                   in green) and you will receive a confirmation email."
    ),
    shiny::fileInput(
      ns("file_upload"),
      "Choose an Excel File",
      accept = c(".xlsx")
    ),

    shiny::actionButton(
      ns("upload_btn"),
      "Upload & Process",
      icon = icon("upload")
    ),

    shinyjs::useShinyjs(),

    shiny::actionButton(
      ns("submit_btn"),
      "Submit to Database",
      icon = icon("database"),
      disabled = TRUE
    ),
    shinyjs::hidden(
      shiny::div(
        id = ns("loading_indicator"),
        style = "display: flex; align-items: center; gap: 8px; margin-top: 10px; color: #555;",
        tags$i(class = "fa fa-spinner fa-spin fa-lg"),
        shiny::textOutput(ns("loading_msg"), inline = TRUE)
      )
    ),
    shiny::uiOutput(ns("upload_status")),
    shiny::uiOutput(ns("location_map"))
  )
}


upload_data_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    options(shiny.maxRequestSize = 20 * 1024^2)
    ns <- session$ns

    # ---- reactive validate ------
    validated_samples <- shiny::reactiveVal(NULL)
    validated_sources <- shiny::reactiveVal(NULL)
    validated_submission <- shiny::reactiveVal(NULL)
    tables_to_submit <- shiny::reactiveVal(NULL)
    tables_split_full <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$upload_btn, {
      # ---- get file upload -----
      load_indicator(input, output)

      shinyjs::disable("submit_btn")
      file_path <- input$file_upload$datapath
      check_sheets(file_path, output)
      # ----- start validation processes ------

      # -----  read submission  ------

      tbl_submission_submitted <- read_xl(
        file_path,
        tbl_name = "tbl_submission",
        skip = 3,
        rename = FALSE
      )
      # ---- validate tbl_submission -----
      agent_submission <- validate_tbl_submission(tbl_submission_submitted)

      # ----- read_table source  -----
      tbl_source_submitted <- read_xl(
        file_path,
        tbl_name = "tbl_sources",
        skip = 3,
        con = con,
        rename = TRUE
      )
      # ------ validate tabale soruce ------
      agent_sources <- validate_tbl_sources(tbl_source_submitted)

      # ----- validate tbl sample ----

      # ----- first get the number of columns -----
      col_types <- read_col_types(
        file_path,
        tbl_name = "tbl_samples",
        skip = 4,
        n_max = 1
      )

      # ---- get tbl sample -----
      tbl_samples_submitted <- read_xl(
        file_path,
        tbl_name = "tbl_samples",
        con = con,
        col_types = col_types,
        skip = 4,
        rename = TRUE,
        rename_twice = "tbl_location"
      )
      # ----- get species list -----
      species_list <- dplyr::tbl(con, "tbl_taxonomy")

      # ---- add valid taxoonmy -----
      tbl_samples_submitted <- add_valid_taxonomy(
        tbl_samples_submitted,
        species_list
      )

      # ---- add validator cols -----
      tbl_samples_submitted <- add_valid_cols(
        df = tbl_samples_submitted,
        valid_values = valid_values
      )
      # ---- run validtor validation ----
      agent_samples <- the_golden_lance(tbl_samples_submitted)

      cli::cli_h2("Agent validation checks")

      # ----  log all agents lets put them into a list an itterate ------

      all_agents <- list(
        agent_submission = agent_submission,
        agent_sources = agent_sources,
        agent_samples = agent_samples
      )

      all_agents |>
        purrr::imap(
          ~ .x |>
            log_agent(.x, .y)
        )

      # ----- unlist them alll to see if they're okay to enter if statment ------
      ok <- purrr::imap_lgl(
        all_agents,
        ~ all(unlist(validate::values(.x)), na.rm = TRUE)
      )

      cli::cli_alert_info(
        "Gate status — submission: {ok['agent_submission']}, 
        sources: {ok['agent_sources']}, samples: {ok['agent_samples']}"
      )

      # if all agents are good process and get ready to submitt
      if (
        isTRUE(ok['agent_submission']) &&
          isTRUE(ok['agent_sources']) &&
          isTRUE(ok['agent_samples'])
      ) {
        # ---- make all of them reactive vals -----
        validated_submission(tbl_samples_submitted)
        validated_source(tbl_source_submitted)
        validated_samples(tbl_samples_submitted)

        # ----- get the next submission id ------
        next_submission_id <- get_submission_id(con)

        new_id <- next_submission_id$next_id

        cli::cli_alert_info(
          "Submission id is: {.val {new_id}}"
        )

        # ---- splap submisison id on to source and submission -----

        tbl_submission_submitted <- tbl_submission_submitted |>
          dplyr::mutate(submission_id = new_id)

        tbl_source_submitted <- tbl_source_submitted |>
          dplyr::mutate(submission_id = new_id)

        tbl_samples_submitted <- tbl_samples_submitted |>
          dplyr::mutate(submission_id = new_id)

        # ---- pull column names that are all have id
        tables_ids <- get_id_col(con)
        col_names_id <- tables_ids$column_name

        cli::cli_alert_info("Columns that have id are: {.val {tables_ids}}")

        # ---- get max id for each -----
        max_ids <- purrr::pmap(
          tables_ids,
          ~ get_id_max(..1, ..2)
        ) |>
          rlang::set_names(col_names_id)

        max_ids_str <- paste(
          names(max_ids),
          unlist(max_ids),
          sep = " = ",
          collapse = ", "
        )
        cli::cli_alert_info("Next id for each starts here: {max_ids_str}")

        # ----- do the same for source id -----
        tbl_source_submitted <- add_new_id(
          df = tbl_source_submitted,
          id_name = "source_id",
          max_ids = max_ids
        )

        # ----- we need to do a couple things to tbl_submission before submitting
        tbl_samples_submitted <- add_new_id(
          df = tbl_samples_submitted,
          id_name = "sample_id",
          max_ids = max_ids
        )

        # ---- fix case types -----
        tbl_samples_submitted <- fix_case_types(tbl_samples_submitted)

        # ---- add source id based on user supplied id ------
        tbl_samples_submitted <- add_source_id(
          tbl_samples = tbl_samples_submitted,
          tbl_sources = tbl_source_submitted
        )

        tbl_samples_submitted <- tbl_samples_submitted |>
          dplyr::left_join(
            species_list |>
              dplyr::collect()
          )

        # doo the same to source -----
        tbl_source_submitted <- tbl_source_submitted |>
          dplyr::select(-source_id) |>
          dplyr::rename(source_id = .source_id)

        # ------ get tables to split -----
        tables_to_split <- get_column_map(con) |>
          dplyr::filter(!table_name %in% c("tbl_sources", "tbl_submission")) |>
          dplyr::collect() |>
          (\(.) split(., .$table_name))()
        # ----- split by table name ----

        tables_split <- lapply(names(tables_to_split), function(tbl_name) {
          map <- tables_to_split[[tbl_name]]
          cols <- map$field_name

          # --- select only payload columns first
          payload <- tbl_samples_submitted |>
            dplyr::select(dplyr::any_of(cols))

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
                dplyr::select(dplyr::any_of(id_cols)),
              payload
            )

            cli::cli_alert_success("Using data for table: {tbl_name}")
            out
          } else {
            cli::cli_alert_info("Skipping empty table: {tbl_name}")
            NULL
          }
        }) |>
          purrr::set_names(names(tables_to_split)) |>
          purrr::compact()

        # --- add in ids -----

        tables_split_full <- assign_table_ids(
          tables_split,
          tables_ids,
          max_ids
        )

        tables_split_full$tbl_sources <- tbl_sources_submitted
        tables_split_full$tbl_submission <- tbl_submission_submitted

        # Define the "priority" tables to go first
        priority_tables <- c("tbl_submission", "tbl_source", "tbl_samples")

        # Find which priority tables exist in your list
        existing_priority <- intersect(
          priority_tables,
          names(tables_split_full)
        )

        # Get remaining tables
        other_tables <- setdiff(names(tables_split_full), existing_priority)

        # Combine: priority first, then the rest
        tables_ordered <- c(existing_priority, other_tables)

        # Reorder your list
        tables_split_full <- tables_split_full[tables_ordered]

        # Optional: check order
        cli::cli_alert_info(
          "Tables will be submitted in this order:
                            {paste(names(tables_split_full), collapse = ' -> ')}"
        )
        # etc for other tables
        tables_to_submit(tables_split_full)

        shinyjs::enable("submit_btn")

        output$map <- leaflet::renderLeaflet({
          shiny::req(tables_split_full$tbl_location)

          location_summary <- tables_split_full$tbl_location |>
            dplyr::left_join(
              tables_split_full$tbl_samples |>
                dplyr::select(sample_id, user_sample_id)
            ) |>
            dplyr::group_by(latitude, longitude) |>
            dplyr::summarise(
              sample_ids = paste(user_sample_id, collapse = ", "),
              n_samples = dplyr::n(),
            ) |>
            dplyr::ungroup()

          # Show the actual coordinates for debugging
          cli::cli_alert_info(
            "Locations validated: {nrow(location_summary)} location{?s} ({min(location_summary$n_samples)}-{max(location_summary$n_samples)} samples per location)"
          )
          cli::cli_alert_info(
            "Coordinates: lat range [{min(location_summary$latitude, na.rm=TRUE)}, {max(location_summary$latitude, na.rm=TRUE)}], lon range [{min(location_summary$longitude, na.rm=TRUE)}, {max(location_summary$longitude, na.rm=TRUE)}]"
          )

          # Check for issues
          if (
            any(is.na(location_summary$latitude)) ||
              any(is.na(location_summary$longitude))
          ) {
            cli::cli_alert_warning("Some coordinates are NA!")
          }
          if (any(abs(location_summary$latitude) > 90, na.rm = TRUE)) {
            cli::cli_alert_warning(
              "Some latitudes are out of range (-90 to 90)!"
            )
          }
          if (any(abs(location_summary$longitude) > 180, na.rm = TRUE)) {
            cli::cli_alert_warning(
              "Some longitudes are out of range (-180 to 180)!"
            )
          }

          leaflet::leaflet(location_summary) |>
            leaflet::addTiles() |>
            leaflet::addCircleMarkers(
              lng = ~longitude,
              lat = ~latitude,
              radius = 8,
              color = "#0066cc",
              fillColor = "#3399ff",
              fillOpacity = 0.7,
              popup = ~ paste0(
                "<b>Number of samples:</b> ",
                n_samples,
                "<br>",
                "<b>Sample ID(s):</b> ",
                sample_ids
              ),
              label = ~ paste0(n_samples, " sample(s)")
            )
        })

        output$upload_status <- shiny::renderUI({
          shiny::tagList(
            shiny::p(
              "✔ All validations passed",
              style = "color:green; font-weight:600;"
            ),
            shiny::p(
              paste0(
                "Ready to submit ",
                nrow(tbl_samples_submitted),
                " rows to database."
              ),
              style = "color:green;"
            )
          )
        })

        output$location_map <- shiny::renderUI({
          shiny::req(tables_split_full)

          shiny::req(
            validated_submission(),
            validated_sources(),
            validated_samples()
          )
          tbl_loc <- tables_split_full$tbl_location
          if (all(is.na(tbl_loc$latitude)) & all(is.na(tbl_loc$longitude))) {
            shiny::tagList(
              shiny::h4(
                "No locations were detected in the longtiude and latitude
                  columns of your submitted data.
                  If this is correct, please proceed to submitting
                  the data to the database",
                style = "margin-top: 20px; margin-bottom: 10px;"
              )
            )
          } else {
            shiny::tagList(
              shiny::h4(
                "Please check that your sample locations, the number of samples,
                  and their corresponding ids are correct prior to submitting to
                  the database. To check, click on each point
                  to view the number of samples and the user submitted sample ids.",
                style = "margin-top: 20px; margin-bottom: 10px;"
              ),
              leaflet::leafletOutput(ns("map"), height = "500px")
            )
          }
        })
      } else {
        validated_submission(NULL)
        validated_sources(NULL)
        validated_samples(NULL)
        tables_to_submit(NULL)
        tables_split_full(NULL)

        error_report <- clean_all_validations(
          tbl_submssion = agent_submission,
          tbl_sources = agent_sources,
          tbl_samples = agent_samples
        )

        output$upload_status <- shiny::renderUI({
          shiny::tagList(
            shiny::p(
              "✖ Validation failed - please fix the following issues:",
              style = "color:red; font-weight:600;"
            ),
            shiny::tableOutput(ns("error_table"))
          )
        })

        output$error_table <- shiny::renderTable({
          error_report
        })
      }
    })

    # ---- submit to database ----
    shiny::observeEvent(input$submit_btn, {
      shiny::req(validated_submission())
      shiny::req(validated_sources())
      shiny::req(validated_samples())
      shiny::req(tables_to_submit())

      # --- assume tables_split_full is a named list ---
      tables_to_submit <- tables_to_submit()

      # Create a list to store submission results
      submission_results <- list()

      # ----- begiging concection -----
      DBI::dbBegin(con)

      # keep looop but now wrap in try catch so that it won't let you
      # submit if this errors
      upload_succeeded <- tryCatch(
        {
          for (tbl_name in names(tables_to_submit)) {
            df <- tables_to_submit[[tbl_name]]

            if (nrow(df) > 0) {
              cli::cli_alert_info(
                "Submitting table: {tbl_name} with {nrow(df)} rows..."
              )

              DBI::dbAppendTable(con, tbl_name, df) # all writes inside the single transaction

              cli::cli_alert_success("{tbl_name} submitted successfully")
              submission_results[[tbl_name]] <- list(
                rows_submitted = nrow(df),
                submission_id = if ("submission_id" %in% colnames(df)) {
                  unique(df$submission_id)
                } else {
                  NA
                }
              )
            } else {
              submission_results[[tbl_name]] <- list(
                rows_submitted = 0,
                submission_id = NA
              )
              cli::cli_alert_info("{tbl_name} has no rows to submit, skipping.")
            }
          }

          DBI::dbCommit(con)
          shiny::showNotification("Upload successful!", type = "message")
          TRUE
        },
        error = function(e) {
          # if errors it will rolle back and display an alert
          DBI::dbRollback(con)
          cli::cli_alert_danger(
            "Upload failed due to inconsistances,
                              rolled back: {e$message}"
          )
          shiny::showNotification(
            "Upload failed. No data was saved.",
            type = "error"
          )
          FALSE
        }
      )

      # Create a message to display
      output$upload_status <- shiny::renderUI({
        if (!upload_succeeded) {
          shiny::HTML(
            "<span style='color: red;'>
           ✘ Upload failed — no data was saved. Please check your data and try again.
         </span>"
          )
        } else {
          msg <- lapply(names(submission_results), function(tbl_name) {
            res <- submission_results[[tbl_name]]
            paste0(
              "✔ ",
              tbl_name,
              ": ",
              res$rows_submitted,
              " rows submitted",
              if (!is.na(res$submission_id)) {
                paste0(", submission_id = ", res$submission_id)
              } else {
                ""
              }
            )
          })

          shiny::HTML(
            paste0(
              "<span style='color: green;'>",
              paste(msg, collapse = "<br>"),
              "</span>"
            )
          )
        }
      })

      shinyjs::disable("submit_btn")
    })
  })
}
