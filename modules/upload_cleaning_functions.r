# Match Excel column to DB column using progressive matching
match_to_db_col <- function(col_name, db_cols) {
  # Special cases that can't be derived
  special <- c(
    latitude = "lat",
    longitude = "lon",
    genus_species = "scientific_name",
    class_sci = "class",
    order_sci = "order"
  )

  # Check special cases first
  if (col_name %in% names(special)) {
    return(special[[col_name]])
  }

  # If exact match exists, use it

  if (col_name %in% db_cols) {
    return(col_name)
  }

  # Split by underscore and try progressively shorter names
  parts <- strsplit(col_name, "_")[[1]]
  for (i in seq(length(parts) - 1, 1)) {
    candidate <- paste(parts[1:i], collapse = "_")
    if (candidate %in% db_cols) {
      return(candidate)
    }
  }

  # No match found
  return(col_name)
}

# Rename Excel columns to match database schema
rename_to_db_col <- function(df, con, table_name) {
  db_cols <- get_column_map(con) |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(field_name)

  df_cols <- colnames(df)

  cli::cli_alert_info("Found {length(db_cols)} fields in {table_name}")

 rename_vec <- sapply(df_cols, function(excel_col) {
    cleaned <- clean_excel_col(excel_col)
    if (cleaned %in% db_cols) return(cleaned)
    if (excel_col %in% db_cols) return(excel_col)
    return(excel_col)
  })

  rename_vec <- rename_vec[rename_vec != names(rename_vec)]

  if (length(rename_vec) > 0) {
    cli::cli_alert_success("Renamed {length(rename_vec)} columns")
    rename_vec <- setNames(names(rename_vec), rename_vec)
    df <- df |> dplyr::rename(dplyr::all_of(rename_vec))
  }

  matched_cols <- intersect(colnames(df), db_cols)
  cli::cli_alert_info("Selecting {length(matched_cols)} matching columns")

  df |> dplyr::select(dplyr::any_of(db_cols))
}