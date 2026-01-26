
# ----- validate tbl_samples ------
validate_tbl_samples <- function(df) {

  required_cols <- c(
    "pi_name", "source_id", "user_sample_id", "date", "month",
    "season", "sample_year", "common_name", "genus_species", "genus",
    "family", "order_sci", "class_sci", "sex", "lifestage", "wild_lab",
    "trt_description", "age", "length_mm", "length_type", "weight",
    "weight_units", "composite", "composite_n", "tissue_type", "sample_procedure",
    "location", "waterbody", "area", "site", "site_depth", "latitude",
    "longitude", "calorimetry_method", "calorimeter_conversion_factor",
    "sample_weight", "sample_weight_type", "energy_measurement",
    "energy_units", "slope", "intercept", "x_units", "y_units", "percent_water",
    "percent_ash", "percent_lipid", "percent_protein", "percent_carbon",
    "percent_nitrogen", "d13c", "d15n", "d34s", "c_n")

  report <- create_agent(df) |>
    col_exists(vars(required_cols)
    ) |>

    col_vals_not_null(
      vars(pi_name, genus_species, wild_lab, tissue_type,
           sample_procedure)
    ) |>

    # Excel dates are read as POSIXct, not Date - check for date-like class
    col_is_posix(date) |>

    col_vals_between(
      month, 1, 12,
      preconditions = ~ !is.na(month)
    ) |>

    col_vals_in_set(
      season,
      c("spring", "summer", "fall", "winter"),
      preconditions =  ~ !is.na(season)
    ) |>

    col_vals_in_set(
      sex,
      c("male", "female", "unknown"),
      preconditions = ~ !is.na(sex)
    ) |>

    col_is_numeric(vars(length_mm, weight)) |>

    interrogate()

  return(report)
}

# ----- pretty pointblank -----
pretty_pointblank_report <- function(agent) {

  report_df <- pointblank::get_agent_x_list(agent)$validation_set

  failed <- report_df |>
    dplyr::filter(!all_passed) |>
    dplyr::transmute(
      Column = vapply(column, function(x) paste(x, collapse = ", "), character(1)),
      Issue = dplyr::case_when(
        assertion_type == "col_exists" ~ "Column is missing from the data",
        assertion_type == "col_is_posix" ~ "Must be a valid date",
        assertion_type == "col_is_date" ~ "Must be a valid date (YYYY-MM-DD)",
        assertion_type == "col_vals_in_set" ~ paste0(
          "Contains invalid values. Allowed: ",
          vapply(values, function(v) paste(v, collapse = ", "), character(1))
        ),
        assertion_type == "col_vals_between" ~ paste0(
          "Values out of range. Must be between ", values[[1]][1], " and ", values[[1]][2]
        ),
        assertion_type == "col_vals_not_null" ~ "Required field - cannot be empty",
        assertion_type == "col_is_numeric" ~ "Must contain only numbers",
        assertion_type == "col_is_character" ~ "Must contain text values",
        TRUE ~ assertion_type
      ),
      `Rows Failed` = n_failed
    )

  if (nrow(failed) == 0) {
    return(NULL)
  }

  return(failed)
}

