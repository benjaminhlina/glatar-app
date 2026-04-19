# ---- clean all_validations -----

#' Clean functions
#'
#' These functions clean up `data.frames` and `vectors` that
#' are coming into the the databae prior to submission,
#' validation reports, or are in the database and need need to be
#' cleaned prior to being displayed (e.g., schema constraints)
#'
#' @param ... objects to be cleaned. Usually validation reports
#'
#' @name clean_functions
#' @export

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
#' @param df a `data.frame` objects to be cleaned prior to being
#' dispalyed. For example, number of samples at a location
#' @param flag_cols collumns to flag and coalesce
#' @param type is the data table type that is being supplied e.g., tbl_isotope
#' @param group_cols columns that will be grouping our summariziing e.g.,
#' lat and lon, pi name, waterbody ect.
#'
#' @name clean_functions
#' @export
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
#' @param clause constraint clause to be cleaned
#'
#' @name clean_functions
#' @export

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
# ----- clean_match_to_db_col -----
# Match Excel column to DB column using progressive matching

#' @param col_name column name to be cleaned to match database column names
#' @param db_cols column names in the database to be used
#'
#' @name clean_functions
#' @export
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
# ----- clean_rename_to_db_col -----
#' @param df a `data.frame` of imported new data
#' @param con PostgreSQL connection object using `{DBI}`
#' @param table_name the table name in the database that each
#' column name is assinged to.
#'
#' @name clean_functions
#' @export
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

# ----- clean_row_index -----
#' @param row_index_str a column containing the row index from submitted data during
#' validation
#' @param min_run minimum number of consecutive numbers before it condenses
#' the row numbers. Default is `5`.
#' @name clean_functions
#' @export

clean_row_index <- function(row_index_str, min_run = 5) {
  # Parse the numbers
  nums <- strsplit(row_index_str, ",")[[1]] |>
    trimws() |>
    as.integer()
  nums <- sort(unique(nums))

  if (length(nums) == 0) {
    return(row_index_str)
  }

  # Group into consecutive runs
  groups <- list()
  start <- nums[1]
  end <- nums[1]

  for (i in seq_along(nums)[-1]) {
    if (nums[i] == end + 1) {
      end <- nums[i]
    } else {
      groups <- c(groups, list(c(start, end)))
      start <- nums[i]
      end <- nums[i]
    }
  }
  groups <- c(groups, list(c(start, end)))

  # Format each group
  parts <- sapply(groups, function(g) {
    run_length <- g[2] - g[1] + 1
    if (run_length >= min_run) {
      paste0(g[1], " - ", g[2]) # e.g. "9625-9708"
    } else if (run_length == 1) {
      as.character(g[1]) # single number
    } else {
      paste(g[1]:g[2], collapse = ", ") # short run, keep expanded
    }
  })

  out <- paste(parts, collapse = ", ")
  return(out)
}
