# ---- clean all_validations -----
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

# ------ clean data types -----

clean_data_tables <- function(
  df,
  flag_cols,
  type,
  group_cols,
  filter_coords = TRUE
) {
  group_vars <- names(tidyselect::eval_select(rlang::enquo(group_cols), df))

  if (isTRUE(filter_coords)) {
    df <- df |>
      dplyr::filter(!is.na(longitude))
  }

  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(flag_cols), ~ dplyr::coalesce(., 0L))
    ) |>
    # Use group_by + summarise (not distinct) so flags are OR'd across samples
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      n_samples = dplyr::n(),
      dplyr::across(dplyr::all_of(flag_cols), ~ max(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      data_tables = purrr::pmap_chr(
        dplyr::pick(dplyr::all_of(flag_cols)),
        \(...) {
          flags <- c(...)
          labels <- unname(type)[as.logical(flags)]
          if (length(labels) == 0) {
            "None"
          } else {
            paste(labels, collapse = ", ")
          }
        }
      )
    )

  return(df)
}


# ----- clean db_constraints -----
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

# Match Excel column to DB column using progressive matching
clean_match_to_db_col <- function(col_name, db_cols) {
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
clean_rename_to_db_col <- function(df, con, table_name) {
  db_cols <- get_column_map(con) |>
    dplyr::filter(table_name == !!table_name) |>
    dplyr::pull(field_name)

  cli::cli_alert_info("Found {length(db_cols)} fields in {table_name}")
  cli::cli_alert_info("Found {.field {db_cols}} fields in {table_name}")

  old_names <- names(df)

  new_names <- vapply(
    old_names,
    clean_match_to_db_col,
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

  cli::cli_alert_info(
    "Columns after rename: {paste(names(df), collapse = ', ')}"
  )

  # drop example cols
  return(df)
}
