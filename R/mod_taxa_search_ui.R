taxa_search_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = id,
    shiny::h2("Search Taxa in the Database"),
    shiny::p(
      "Use the search bar to look up the taxa that are in the database.
      This tab servers two purposes 1)
      to let the user know what species exist in the database and 2) when
      uploading new data, the user can
      check validation errors to match what species are in the database."
    ),
    shiny::textInput(
      ns("search_bar"),
      label = "Search Taxa",
      placeholder = "Type to filter taxa..."
    ),
    DT::DTOutput(ns("taxa_table")),
    shiny::h2("Suggest Taxa to the Database"),
    add_taxa_ui(ns = ns, con = con),
    DT::DTOutput(ns("add_taxa"))
  )
}

# ----- server taxa search -----
taxa_search_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    filtered_taxa <- create_searching_data(
      con = con,
      tbl_name = "tbl_taxonomy",
      input = input,
      collect = TRUE
    )
    output$taxa_table <- DT::renderDT({
      DT::datatable(
        filtered_taxa(),
        options = list(pageLength = 25, scrollX = TRUE, searching = FALSE),
        rownames = FALSE
      )
    })
    # ----- sbumisison ===
    submitted_flag <- shiny::reactiveVal(FALSE)
    # All column names in order
    cols <- get_taxa_col(con)

    # Reactive store for accumulated rows
    taxa_data <- shiny::reactiveVal(clear_table(cols))

    # ----- make data -----

    shiny::observeEvent(input$add_row, {
      new_row <- data.frame(
        common_name = input$common_name,
        scientific_name = input$scientific_name,
        genus = input$genus,
        tribe = input$tribe,
        subfamily = input$subfamily,
        family = input$family,
        superfamily = input$superfamily,
        suborder = input$suborder,
        order_sci = input$order_sci,
        superorder = input$superorder,
        class_sci = input$class_sci,
        superclass = input$superclass,
        phylum = input$phylum,
        kingdom = input$kingdom,
        organism_type = input$organism_type,
        tsn = input$tsn,
        stringsAsFactors = FALSE
      )
      taxa_data(rbind(taxa_data(), new_row))
      clear_fields(session, cols)
    })
    # ----- clear fileds will clear roow -----
    shiny::observeEvent(input$clear_row, {
      clear_fields(session, cols)
    })

    # ----- clear all will reset the table ----
    shiny::observeEvent(input$clear_all, {
      taxa_data(clear_table(cols))
    })

    taxa_data_clean <- shiny::reactive({
      taxa_data() |>
        dplyr::rename_with(~ convert_nice_name(.x))
    })
    # ---- dispaly add table -----
    display_add_taxa(
      data = taxa_data_clean,
      output = output,
      session = session,
      output_id = "add_taxa"
    )

    # ----- allow deteation -----
    shiny::observeEvent(input$delete_row, {
      df <- taxa_data()
      if (input$delete_row >= 1 && input$delete_row <= nrow(df)) {
        taxa_data(df[-input$delete_row, , drop = FALSE])
      }
    })
    # ----- email -----
    shiny::observeEvent(input$submit_to_manager, {
      if (submitted_flag()) {
        return()
      }
      submitted_flag(TRUE)

      on.exit(submitted_flag(FALSE), add = TRUE)
      # req(input$submit_to_manager > 0)
      df <- taxa_data()

      email <- trimws(input$submitter_email)
      email_valid <- grepl(
        "^[a-zA-Z0-9._%+\\-]+@[a-zA-Z0-9.\\-]+\\.[a-zA-Z]{2,}$",
        email
      )

      if (nchar(email) == 0) {
        shiny::showNotification(
          "Please enter your email address before submitting.",
          type = "warning"
        )
        return()
      }

      if (!email_valid) {
        shiny::showNotification(
          "Please enter a valid email address (e.g. you@example.com).",
          type = "error"
        )
        return()
      }

      # Guard: nothing to send
      if (nrow(df) == 0) {
        shiny::showNotification("No taxa rows to submit.", type = "warning")
        return()
      }

      # Strip the delete-button column before export
      df$delete <- NULL

      # Write to a temp file
      tmp <- tempfile(fileext = ".xlsx")
      on.exit(unlink(tmp), add = TRUE)
      openxlsx::write.xlsx(df, tmp, rowNames = FALSE)

      # Build email and send
      email_text <- taxa_submission_email_body(
        n_rows = nrow(df),
        submitted_by = input$submitter_email # swap for however you capture this
      )

      tryCatch(
        {
          send_email(
            to_user = input$submitter_email,
            email_text = email_text,
            attachment_path = tmp
          )
          unlink(tmp)
          shiny::showNotification(
            "Submission sent successfully!",
            type = "message"
          )
          taxa_data(clear_table(cols)) # reset table after successful send
        },
        error = function(e) {
          shiny::showNotification(
            paste("Email failed:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
    })
  })
}
