
assign_table_ids <- function(tables_split, tables_ids, max_ids) {

  purrr::imap(tables_split, function(df, tbl_name) {

    # --- get candidate id columns for this table
    id_col <- tables_ids |>
      dplyr::filter(table_name == tbl_name) |>
      dplyr::pull(column_name)

    # --- drop foreign keys you never want to generate
    id_col <- setdiff(id_col, c("sample_id", "source_id", "submission_id"))

    if (length(id_col) == 0) {
      cli::cli_alert_info("No primary ID to assign for {tbl_name}")
      return(df)
    }

    # --- use first remaining (should be only one)
    id_col <- id_col[1]

    # --- current max from DB (0 if none)
    start_id <- max_ids[[id_col]]
    if (is.null(start_id) || is.na(start_id)) start_id <- 0

    # --- do we need to create IDs?
    needs_id <- !id_col %in% names(df) || all(is.na(df[[id_col]]))

    if (needs_id) {

      n <- nrow(df)

      df <- df |>
        dplyr::mutate(
          !!rlang::sym(id_col) := seq.int(start_id + 1, length.out = n)
        )

      cli::cli_alert_success(
        "Assigned {id_col} for {tbl_name} starting at {start_id + 1}"
      )

    } else {
      cli::cli_alert_info("{tbl_name} already has {id_col}")
    }

    df
  })
}
