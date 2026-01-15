
validate_tbl_samples <- function(df) {

  create_agent(df) |>
    col_exists(
      pi_name, source_id, user_sample_id, date, month, season,
      sample_year, common_name, genus_species, genus, family,
      order_sci, class_sci, sex, lifestage, wild_lab,
      age, length_mm, weight, latitude, longitude,
      d13c, d15n, c_n
    ) |>

    col_vals_not_null(
      pi_name, source_id, user_sample_id, date, sample_year
    ) |>

    col_vals_regex(
      date, "^\\d{4}-\\d{2}-\\d{2}$"
    ) |>

    col_vals_between(month, 1, 12) |>

    col_vals_in_set(
      season, c("spring", "summer", "fall", "winter")
    ) |>

    col_vals_in_set(
      sex, c("male", "female", "unknown")
    ) |>

    col_vals_between(latitude, -90, 90) |>
    col_vals_between(longitude, -180, 180) |>

    col_vals_numeric(
      length_mm, weight, d13c, d15n, c_n
    ) |>

    interrogate()
}
