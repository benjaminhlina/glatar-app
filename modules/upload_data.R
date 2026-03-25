upload_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = id,
    shinyjs::useShinyjs(),
    shiny::h2("Upload Excel File"),
    shiny::h4("Instructions"),
    shiny::tags$ol(
      shiny::tags$li("Select the Excel file you would like to upload."),
      shiny::br(),
      shiny::tags$li(
        "Click 'Upload & Process'. The system will run a series of 
      validation checks and process the data to adhere to database standards."
      ),
      shiny::br(),
      shiny::tags$li(
        "Review any validation errors — these will identify the 
      specific rows, columns, and descriptions of any issues found."
      ),
      shiny::br(),
      shiny::tags$li(
        "If the upload is successful, verify your sampling 
      locations using the interactive map that appears."
      ),
      shiny::br(),
      shiny::tags$li(
        "Click 'Submit to Database' to finalize the upload. A 
      confirmation message (displayed in green) will appear and 
      you will receive a confirmation email."
      )
    ),
    # shiny::p(
    #   "This panel allows you to upload data to the GLATAR database.
    #                First, select the Excel file you would like to upload. Then click
    #                'Upload & Process'. The system will run a series of validation checks and
    #                process the data to adhere to database standards. Validation checks will
    #                notify you of any issues, including the specific rows, columns,
    #                and descriptions of the errors.

    #                If the upload is successful, a confirmation message will appear and an
    #                interactive map will be displayed so you can verify that your sampling
    #                locations are correct. Once you have reviewed the map, click the
    #                'Submit to Database' button to finalize the upload.
    #                A successful submission will generate a confirmation message (displayed
    #                in green) and you will receive a confirmation email."
    # ),
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
        tags$span(id = ns("loading_msg"))
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

      shinyjs::disable(ns("submit_btn"))
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
          ~ log_agent(.x, .y)
        )

      # ----- unlist them alll to see if they're okay to enter if statment ------
      ok <- purrr::imap(
        all_agents,
        ~ all(validate::as.data.frame(.x)$passes, na.rm = TRUE)
      )

      cli::cli_alert_info(
        "Gate status — submission: {ok[['agent_submission']]}, 
        sources: {ok[['agent_sources']]}, samples: {ok[['agent_samples']]}"
      )

      # if all agents are good process and get ready to submitt
      if (
        isTRUE(ok[['agent_submission']]) &&
          isTRUE(ok[['agent_sources']]) &&
          isTRUE(ok[['agent_samples']])
      ) {
        # ---- make all of them reactive vals -----
        tryCatch(
          {
            validated_submission(tbl_submission_submitted)
            validated_sources(tbl_source_submitted)
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

            tbl_samples_submitted <- add_taxonomic_groups(
              tbl_samples_submitted,
              species_list = species_list
            )

            # doo the same to source -----
            tbl_source_submitted <- tbl_source_submitted |>
              dplyr::select(-source_id) |>
              dplyr::rename(source_id = .source_id)

            # ------ get tables to split -----
            tables_to_split <- get_column_map(con) |>
              dplyr::filter(
                !table_name %in% c("tbl_sources", "tbl_submission")
              ) |>
              dplyr::collect() |>
              (\(.) split(., .$table_name))()
            # ----- split by table name ----

            tables_split <- split_tables(
              df = tbl_samples_submitted,
              tables_to_split = tables_to_split
            )

            # --- add in ids -----

            tables_split_ready <- assign_table_ids(
              tables_split,
              tables_ids,
              max_ids
            )

            tables_split_ready <- add_sub_sor_tbl(
              split_tables = tables_split_ready,
              sub_tbl = tbl_submission_submitted,
              sor_tbl = tbl_source_submitted
            )

            tables_split_ready <- fix_table_order(
              split_tables = tables_split_ready
            )
            # etc for other tables
            tables_to_submit(tables_split_ready)
            tables_split_full(tables_split_ready)
            cli::cli_alert_danger(
              "Colnmn names are the following: {.field {tables_split_ready |> 
            purrr::map(~ colnames(.x))}}"
            )

            purrr::iwalk(
              tables_split_ready,
              ~ cli::cli_alert_info(
                "Table {.field {.y}}: {.val {nrow(.x)}} rows"
              )
            )

            display_submission_map(
              output = output,
              ns = ns,
              output_id = "map",
              split_tables = tables_split_full()
            )

            display_validation_status(
              output = output,
              ns = ns,
              output_id = "upload_status",
              split_tables = tables_split_full()
            )

            display_sub_map_msg(
              output = output,
              ns = ns,
              output_id = "location_map",
              split_tables = tables_split_full(),
              validated_submission = validated_submission,
              validated_sources = validated_sources,
              validated_samples = validated_samples
            )

            success <- TRUE
          },
          error = function(e) {
            cli::cli_alert_danger("Processing failed: {conditionMessage(e)}")
          }
        )
        if (isTRUE(success)) {
          shinyjs::enable("submit_btn")
        } else {
          shinyjs::disable("submit_btn")
        }
      } else {
        # ---- this else statment is if validations fails then do this

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

        display_validation_status(
          output = output,
          ns = ns,
          output_id = "upload_status",
          validated = FALSE
        )

        output$error_table <- shiny::renderTable({
          error_report
        })
      }
      load_indicator_hide(input, output)
      # shinyjs::reset("file_upload")
    })

    # ---- submit to database ----
    shiny::observeEvent(input$submit_btn, {
      shiny::req(validated_submission())
      shiny::req(validated_sources())
      shiny::req(validated_samples())
      shiny::req(tables_to_submit())

      cli::cli_h3("Reactive state check")

      cli::cli_alert_info(
        "validated_submission is NULL: {is.null(validated_submission())}"
      )

      cli::cli_alert_info(
        "validated_sources is NULL: {is.null(validated_sources())}"
      )

      cli::cli_alert_info(
        "validated_samples is NULL: {is.null(validated_samples())}"
      )

      cli::cli_alert_info(
        "tables_to_submit is NULL: {is.null(tables_to_submit())}"
      )

      cli::cli_alert_info(
        "tables_split_full is NULL: {is.null(tables_split_full())}"
      )

      # --- assume tables_split_full is a named list ---
      tables_to_submit <- tables_to_submit()

      # ----- begiging concection -----
      upload_result <- upload_to_db(con, tables_to_submit = tables_to_submit)
      upload_succeeded <- upload_result$succeeded
      submission_results <- upload_result$results

      # Create a message to display
      display_upload_status(
        output = output,
        ns = ns,
        output_id = "upload_status",
        upload_succeeded = upload_succeeded,
        submission_results = submission_results
      )

      shinyjs::disable("submit_btn")
      # shinyjs::reset("file_upload")
    })
  })
}
