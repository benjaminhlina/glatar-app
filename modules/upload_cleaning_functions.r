# Match Excel column to DB column using progressive matching
match_to_db_col <- function(col_name, db_cols) {

  # Strip example: split on "_e_g_" or "_i_e_" and take the first part
  # ., "common_name_e_g_lake_trout" -> "common_name"
  candidate_col <- col_name

  if (grepl("_e_g_|_i_e_", col_name)) {
    candidate_col <- strsplit(col_name, "_e_g_|_i_e_")[[1]][1]
  }

  # ---- final match ----
  if (candidate_col %in% db_cols) {
    return(candidate_col)
  }

  # ---- No match found ----
  return(col_name)
}

# Rename Excel columns to match database schema
rename_to_db_col <- function(df, con, table_name) {

  db_cols <- get_column_map(con) |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(field_name)

  cli::cli_alert_info("Found {length(db_cols)} fields in {table_name}")
  cli::cli_alert_info("Found {.field {db_cols}} fields in {table_name}")

  old_names <- names(df)

  new_names <- vapply(
    old_names,
    match_to_db_col,
    character(1),
    db_cols = db_cols
  )

  # log matches
  changed <- old_names != new_names

  if (any(changed)) {
    purrr::walk2(
      old_names[changed],
      new_names[changed],
      ~ cli::cli_alert_info("Matching: {.field { .x }} -> {.field { .y }}")
    )
    cli::cli_alert_success("Renamed {sum(changed)} columns")
  }

  names(df) <- new_names

  cli::cli_alert_info("Columns after rename: {paste(names(df), collapse = ', ')}")

  # drop example cols
  return(df)

}
