# ---- add_valid_col

add_valid_cols <- function(df) {
  df <- df |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character) &
        !dplyr::any_of(c(
          "fatty_acid_type",
          "thiamine_type",
          "energy_measurment_units"
        )),
      tolower
    ))

  constraint_exprs <- purrr::imap(valid_values, function(vals, col) {
    if (!col %in% names(df)) {
      return(NULL)
    }
    sym <- rlang::sym(col)
    rlang::expr(is.na(!!sym) | !!sym %in% !!vals)
  }) |>
    purrr::compact()

  names(constraint_exprs) <- paste0(".", names(constraint_exprs))

  df <- df |>
    dplyr::mutate(!!!constraint_exprs)

  # 3. Bespoke checks that can't be expressed as simple %in% -------------
  df <- df |>
    dplyr::mutate(
      .date = is.na(date) | grepl("^\\d{4}-\\d{2}-\\d{2}$", date),
      .month = is.na(month) | (month >= 1 & month <= 12),
      .ed = dplyr::case_when(
        is.na(sample_weight_type) ~ NA,
        sample_weight_type == "wet" ~ energy_measurement >= 0 &
          energy_measurement <= 13000,
        sample_weight_type == "dry" ~ energy_measurement >= 1000 &
          energy_measurement <= 43000,
        .default = NA
      )
    )

  return(df)
}

# ----- add valid txaomnmy ------

add_valid_taxonomy <- function(df, species_list) {
  # ---- get tax from db ----
  valid_taxonomy <- valid_taxonomy(species_list)

  valid_common_sentence <- stringr::str_to_sentence(valid_taxonomy$valid_common)
  valid_sci_sentence <- stringr::str_to_sentence(
    valid_taxonomy$valid_scientific
  )

  df <- df |>
    dplyr::mutate(
      common_name = stringr::str_to_sentence(common_name),
      scientific_name = stringr::str_to_sentence(scientific_name)
    )

  # Store validation results in df attributes for later use
  common_check <- check_taxonomy_match(df$common_name, valid_common_sentence)
  sci_check <- check_taxonomy_match(df$scientific_name, valid_sci_sentence)

  cli::cli_alert_info("comon_check {.val {common_check}}")
  cli::cli_alert_info("sci_check {.val {sci_check}}")

  attr(df, "common_name_suggestions") <- common_check$suggestions
  attr(df, "scientific_name_suggestions") <- sci_check$suggestions

  df <- df |>
    dplyr::mutate(
      .valid_common_name = common_check$valid,
      .valid_scientific_name = sci_check$valid
    )

  return(df)
}
