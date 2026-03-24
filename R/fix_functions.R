fix_case_types <- function(df) {
  df <- df |>
    dplyr::mutate(
      dplyr::across(common_name:family, ~ stringr::str_to_sentence(.x)),
      length_type = tolower(length_type),
      waterbody = tools::toTitleCase(waterbody),
      data_type = stringr::str_to_sentence(data_type),
      .energy_units = dplyr::if_else(
        is.na(energy_units),
        true = NA,
        false = paste(
          energy_units,
          sample_weight_type,
          "weight",
          sep = " "
        )
      ),
      calorimetry_method = stringr::str_to_sentence(calorimetry_method) |>
        stringr::str_replace("Gentry-weigert", "Gentry-Weigert")
    ) |>
    dplyr::select(-energy_units) |>
    dplyr::rename(
      energy_units = .energy_units,
      sample_id = .sample_id
    )
  return(df)
}

# ---- fix table order -----
fix_table_order <- function(split_tables) {
  # Define the "priority" tables to go first
  priority_tables <- c("tbl_submission", "tbl_sources", "tbl_samples")

  # Find which priority tables exist in your list
  existing_priority <- intersect(
    priority_tables,
    names(split_tables)
  )

  # Get remaining tables
  other_tables <- setdiff(names(split_tables), existing_priority)

  # Combine: priority first, then the rest
  tables_ordered <- c(existing_priority, other_tables)

  # Reorder your list
  fixed_table_order <- split_tables[tables_ordered]

  # Optional: check order
  cli::cli_alert_info(
    "Tables will be submitted in this order:
                            {paste(names(fixed_table_order), collapse = ' -> ')}"
  )
  return(fixed_table_order)
}


# ---- fix tittle label -----
fix_title_label <- function(x, max = NULL) {
  if (is.null(max)) {
    max <- 4
  }
  if (length(x) <= max) {
    paste(x, collapse = ", ")
  } else {
    paste0(
      paste(utils::head(x, max), collapse = ", "),
      ", <br>… (",
      length(x) - max,
      " more)"
    )
  }
}
# ---- fix_var_gneric ----
fix_var_generic <- function(df, var_raw) {
  # detect if it's one of the synthetic length vars
  if (grepl("^length_mm__", var_raw)) {
    # split if length_mm__ is presnet in

    parts <- strsplit(var_raw, "__")[[1]]
    # grab the second element of part
    var_type <- parts[2]

    # Filter df to matching length type
    df <- df |>
      dplyr::filter(length_type == var_type)

    # Dynamic label
    var_label <- paste0(stringr::str_to_title(var_type), " Length (mm)")
    var <- "length_mm"
  } else if (grepl("^energy_units__", var_raw)) {
    parts <- strsplit(var_raw, "__")[[1]]
    # grab the second element of part
    var_type <- parts[2]
    cli::cli_alert_danger("var_type is: {.field {var_type}}")

    df <- df |>
      dplyr::filter(energy_units == var_type)

    eu <- df |>
      dplyr::distinct(energy_units) |>
      dplyr::pull()
    cli::cli_alert_danger("units is: {.field {eu}}")
    # Dynamic label
    var_label <- paste0("Energy Density (", var_type, ")")
    var <- "energy_measurement"

    cli::cli_alert_danger("var_label is: {.field {var_label}}")
  } else {
    # else if (grepl("amino_acid_type__", var_raw)) {
    #   parts <- strsplit(var_raw, "__")[[1]]
    #   # grab the second element of part
    #   var_type <- parts[2]
    #   cli::cli_alert_danger("var_type is: {.field {var_type}}")

    #   df <- df |>
    #     dplyr::filter(amino_acid_type == var_type)

    #   eu <- df |>
    #     distinct(amino_acid_type) |>
    #     pull()

    #   cli::cli_alert_danger("units is: {.field {eu}}")
    #   # Dynamic label
    #   var_label <- paste0(
    #     stringr::str_to_sentence(var_types)
    #     # " (",
    #     # amino_acid_unit,
    #     # ")"
    #   )
    #   var <- "amino_acid_measurement"
    #   cli::cli_alert_danger("var_label is: {.field {var_label}}")
    # }
    cli::cli_alert_info("Checking for {.field {var_raw}} in columns...")
    cli::cli_inform("Available columns: {.val {colnames(df)}}")
    cli::cli_inform("var_raw %in% colnames(df): {var_raw %in% colnames(df)}")
    req(var_raw %in% colnames(df))
    # Normal variable
    var <- var_raw
    var_label <- convert_nice_name(var)[[1]]
  }

  list(
    df = df,
    var = var,
    var_label = var_label
  )
}
