clean_all_validations <- function(...) {
  dots <- rlang::enquos(...)

  reports <- purrr::imap(dots, function(x, nm) {
    res <- pretty_validate_report(
      rlang::eval_tidy(x),
      table_name = nm
    )
    if (!is.null(res)) {
      res$Table <- nm
    }
    res
  }) |>
    purrr::compact()

  if (length(reports) == 0) {
    return(NULL)
  }

  reports_combo <- dplyr::bind_rows(reports) |>
    dplyr::select(Table, Column, Issue, `Row Index`, Suggestion) |>
    dplyr::arrange(Table, Column, Issue)

  return(reports_combo)
}

clean_db_constraints <- function(clause) {
  # Try ARRAY form first
  array_match <- stringr::str_match(clause, "ARRAY\\[(.+?)\\]")
  if (!is.na(array_match[1])) {
    vals <- stringr::str_extract_all(
      array_match[2],
      "'([^']+)'",
      simplify = FALSE
    )[[1]]
    vals_cleaned <- stringr::str_remove_all(vals, "'")

    return(vals_cleaned)
  }

  # Fall back to IN (...) form
  in_match <- stringr::str_match(clause, "IN\\s*\\((.+?)\\)")
  if (!is.na(in_match[1])) {
    vals <- stringr::str_extract_all(
      in_match[2],
      "'([^']+)'",
      simplify = FALSE
    )[[1]]

    vals_clean <- stringr::str_remove_all(vals, "'")
    return(vals_clean)
  }

  NULL
}

# server.R / server function

# Option A: static — constraints won't change during the session
# valid_values <- get_valid_values(con) # your query + clean_db_constraints pipeline

# # Option B: reactive — if you want refresh capability
# valid_values <- reactive({
#   get_valid_values(con)
# }) |>
#   bindCache("constraints") # cache so it doesn't re-query on every use

# # Then wherever you validate uploaded/edited data:
# validated_df <- add_valid_cols(raw_df, valid_values) # Option A
# validated_df <- add_valid_cols(raw_df, valid_values()) # Option B (unwrap reactive)
