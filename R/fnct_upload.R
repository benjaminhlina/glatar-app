upload_to_db <- function(con, tables_to_submit) {
  # ---- create empty list -----
  submission_results <- list()
  # ---- begin connections
  DBI::dbBegin(con)

  submitted_tbls <- tryCatch(
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
  list(
    succeeded = submitted_tbls,
    results = submission_results
  )
}
