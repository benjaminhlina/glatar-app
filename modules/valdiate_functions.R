
# ----- validate tbl_samples ------
validate_tbl_samples <- function(df) {

  required_cols <- c(
    "pi_name", "source_id", "user_sample_id", "date", "month",
    "season", "sample_year", "common_name", "scientific_name", "genus",
    "family", "order_sci", "class_sci", "sex", "lifestage", "wild_lab",
    "trt_description", "age", "length_mm", "length_type", "weight",
    "weight_units", "composite", "composite_n", "tissue_type", "sample_procedure",
    "location", "waterbody", "area", "site", "site_depth", "latitude",
    "longitude", "calorimetry_method", "calorimeter_conversion_factor",
    "sample_weight", "sample_weight_type", "energy_measurement",
    "energy_units", "slope", "intercept", "x_units", "y_units", "percent_water",
    "percent_ash", "percent_lipid", "percent_protein", "percent_carbon",
    "percent_nitrogen", "d13c", "d15n", "d34s", "c_n"
  )

  rules <- validator(

    # ---- structure ----
    all(required_cols %in% names(.)),

    # ---- not null ----
    !is.na(pi_name),
    !is.na(scientific_name),
    !is.na(wild_lab),
    !is.na(tissue_type),
    !is.na(sample_procedure),

    # ---- date ----
    !is.na(as.Date(date, origin = "1899-12-30")),

    # ---- ranges ----
    month >= 1 & month <= 12,

    # ---- sets ----
    season %in% c("spring", "summer", "fall", "winter"),
    sex %in% c("male", "female", "unknown"),

    # ---- numeric ----
    is.numeric(length_mm),
    is.numeric(weight)
  )

  out <- confront(df, rules)


  return(out)
}


# ----- pretty pointblank -----
pretty_validate_report <- function(confrontation) {

  df <- as.data.frame(confrontation, add_columns = TRUE)
  # if valdiate doens't return anything then  return nulll
  if (nrow(df) == 0) return(NULL)

  # ----- grab only bad columns -----
  bad <- df[df$value == FALSE, , drop = FALSE]
  # ----- if tehre are non-return NULL -----
  if (nrow(bad) == 0) return(NULL)



  # long format
  long <- df |>
    pivot_longer(
      cols = starts_with("."),
      names_to = "rule",
      values_to = "passed"
    ) |>
    filter(!passed)

  if (nrow(long) == 0) {
    return(NULL)
  }

  rule_info <- sm |>
    select(rule, expression)

  out <- long |>
    left_join(rule_info, by = "rule") |>
    mutate(
      Row = row_number(),
      Column = stringr::str_extract(expression, "(?<=\\().+?(?=[,\\)])"),
      Issue = case_when(
        grepl("required_cols", expression) ~ "Missing required columns",
        grepl("is.na", expression) ~ "Required field - cannot be empty",
        grepl("month", expression) ~ "Month must be between 1 and 12",
        grepl("season", expression) ~ "Invalid season",
        grepl("sex", expression) ~ "Invalid sex",
        grepl("inherits", expression) ~ "Must be a valid date",
        grepl("is.numeric", expression) ~ "Must be numeric",
        TRUE ~ expression
      )
    ) |>
    select(Row, Column, Issue)

  out <- out |>
    group_by(Column, Issue) |>
    summarise(`Row Index` = paste(sort(unique(Row)), collapse = ", ")) |>
    ungroup()

  return(out)
}


