
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
    col_is_date(date) |>

    col_vals_between(month, 1, 12) |>

    col_vals_in_set(
      season,
      c("spring", "summer", "fall", "winter")
    ) |>

    col_vals_in_set(
      sex,
      c("male", "female", "unknown")
    ) |>


    # col_vals_between(latitude, -90, 90) |>
    # col_vals_between(longitude, -180, 180) |>

    col_is_numeric(vars(length_mm, weight)) |>

    interrogate()
  return(report)
}

# ----- pretty pointblank -----
pretty_pointblank_report <- function(agent) {

  x <- agent |>
    pointblank::get_agent_report() |>
    dplyr::filter(n_failed > 0) |>
    dplyr::transmute(
      Column = columns,
      Rule = brief,
      Failed_Rows = n_failed,
      Message = dplyr::case_when(
        grepl("col_is_date", type) ~ "Must be a valid date (YYYY-MM-DD)",
        grepl("col_vals_in_set", type) ~ "Contains invalid values",
        grepl("col_vals_between", type) ~ "Values out of allowed range",
        grepl("col_vals_not_null", type) ~ "Missing required values",
        grepl("col_is_numeric", type) ~ "Must be numeric",
        TRUE ~ brief
      )
    )

  if (nrow(x) == 0) {
    return(data.frame(Status = "✅ All checks passed"))
  }

  return(x)
}

