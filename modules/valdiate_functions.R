# ----- add valid txaomnmy ------

add_valid_taxonomy <- function(df, species_list) {

  # ---- get tax from db ----
  valid_taxonomy <- valid_taxonomy(species_list)


  valid_common_sentence <- str_to_sentence(valid_taxonomy$valid_common)
  valid_sci_sentence <- str_to_sentence(valid_taxonomy$valid_scientific)

  df <- df |>
    mutate(
      common_name = stringr::str_to_sentence(common_name),
      scientific_name = str_to_sentence(scientific_name)
    )

  # Store validation results in df attributes for later use
  common_check <- check_taxonomy_match(df$common_name,
                                       valid_common_sentence)
  sci_check <- check_taxonomy_match(df$scientific_name,
                                    valid_sci_sentence)

  cli::cli_alert_info("comon_check {.val {common_check}}")
  cli::cli_alert_info("sci_check {.val {sci_check}}")

  attr(df, "common_name_suggestions") <- common_check$suggestions
  attr(df, "scientific_name_suggestions") <- sci_check$suggestions

  df <- df |>
    mutate(
      .valid_common_name = common_check$valid,
      .valid_scientific_name = sci_check$valid
    )

  return(df)
}

# ----- check tax  -----
check_taxonomy_match <- function(input_values, db_values) {
  # Normalize input value
  input_norm <- stringr::str_to_sentence(input_values)

  # Check exact matches
  matches <- input_norm %in% db_values

  # For non-matches, find closest suggestions
  suggestions <- sapply(which(!matches), function(i) {
    if (is.na(input_values[i])) return(NA)

    # Calculate string distances using Jaro-Winkler distance
    distances <- stringdist::stringdist(
      input_norm[i],
      db_values,
      method = "jw"
    )

    # Get the closest match
    closest_idx <- which.min(distances)
    closest_dist <- distances[closest_idx]

    # Only suggest if reasonably close (distance < 0.3)
    if (closest_dist < 0.3) {
      return(db_values[closest_idx])
    } else {
      return(paste0("No close match found for '", input_values[i], "'"))
    }
  })

  # Create result with suggestions
  result <- list(
    valid = matches,
    suggestions = rep(NA, length(input_values))
  )
  result$suggestions[!matches] <- suggestions

  return(result)
}

# ----- pretty pointblank -----
pretty_validate_report <- function(confrontation) {

  df <- as.data.frame(confrontation, add_columns = TRUE)

  original_data <- confrontation$._keys$keyset

  common_name_suggestions <- attr(original_data,
                                  "common_name_suggestions")
  scientific_name_suggestions <- attr(original_data,
                                      "scientific_name_suggestions")

  cli::cli_alert_info("Common suggestions found:
                      {!is.null(common_name_suggestions)}")
  cli::cli_alert_info("Scientific suggestions found:
                      {!is.null(scientific_name_suggestions)}")

  # if valdiate doens't return anything then  return nulll
  if (nrow(df) == 0) return(NULL)

  # transfer forw number
  df <- df |>
    group_by(name) |>
    mutate(
      data_row = row_number()  # This cycles 1, 2, 3, 4... within each rule
    ) |>
    ungroup() |>
    mutate(
      col_name = case_when(
        # Handle %vin% expressions: get the word before %vin%
        grepl("%vin%", expression) ~ sub("^\\s*(\\w+)\\s+%vin%.*", "\\1",
                                         expression),

        # Handle comparison expressions (month - 1 >= ..., etc.)
        grepl("[-+].*[><=]", expression) ~ sub("^\\s*(\\w+)\\s+[-+].*", "\\1",
                                               expression),

        # Handle function calls with commas - get first word before
        # comma in innermost parens
        grepl("\\([^()]*,", expression) ~ {
          temp <- sub(".*\\(([^()]+)\\).*", "\\1", expression)
          sub("^\\s*([^,]+).*", "\\1", temp)
        },

        # Handle function calls without commas - get content of innermost parens
        grepl("\\(", expression) ~ sub(".*\\(([^()]+)\\).*", "\\1", expression),

        grepl("^\\.valid_", expression) ~ sub("^\\.valid_([^ =]+).*", "\\1",
                                              expression),

        # Default: return the expression as-is
        .default = expression
      ),
      # Clean up any remaining quotes or whitespace
      col_name = trimws(gsub('"', '', col_name))
      # col_name = sub(".*\\(([^,\\)]+).*", "\\1", expression)
    )

  # ----- grab only bad columns -----
  bad <- df |>
    filter(value %in% FALSE)

  # ----- if tehre are non-return NULL -----
  if (nrow(bad) == 0) return(NULL)


  # ----- create pretty names -----
  out <- bad |>
    mutate(
      Issue = case_when(
        expression %in% "required_cols" ~ "Missing required columns - you have
        altered the data entry template - please reupload an unaltered file",
        grepl("is.na\\(as.Date", expression) ~"Date format does not follow the
        required format of yyyy-mm-dd or is an invalid date",
        grepl("is.na", expression) ~ "Required field - cannot be empty",
        expression %in% "month" ~ "Month must be between 1 and 12",
        expression %in% "season" ~ "Invalid season - must be spring,
        summer, fall, winter",
        expression %in% "sex" ~ "Invalid sex - must be female, male,
        unknown, or mixed",
        grepl("is.numeric", expression) ~ "Must be numeric value",
        grepl("common_name", expression)  ~ "Common name not found in database",
        grepl("scientific_name", expression) ~ "Scientific name not found in
        database",
        .default = expression
      )
    ) |>
    select(Row = data_row,
           Column = col_name,
           Issue)


  out <- out |>
    mutate(
      Suggestion = case_when(
        Column %in% "common_name" & !is.null(common_name_suggestions) &
          Row <= length(common_name_suggestions) &
          !is.na(common_name_suggestions[Row]) ~
          paste0("Did you mean: ", common_name_suggestions[Row], "?"),
        Column == "scientific_name" & !is.null(scientific_name_suggestions) &
          Row <= length(scientific_name_suggestions) &
          !is.na(scientific_name_suggestions[Row]) ~
          paste0("Did you mean: ", scientific_name_suggestions[Row], "?"),
        .default = NA
      )
    )

  cli::cli_alert_info("Rows with suggestions: {sum(!is.na(out$Suggestion))}")
  cli::cli_alert_success("Suggestions are the following:
                         {.val {paste(unique(na.omit(out$Suggestion)),
                         collapse = ';')}}")
  # ---- clean this up -----
  out <- out |>
    group_by(Column, Issue) |>
    summarise(
      `Row Index` = paste(sort(unique(Row)), collapse = ", "),
      Suggestion = paste(unique(na.omit(Suggestion)), collapse = "; ")
    ) |>
    ungroup() |>
    mutate(
      Suggestion = if_else(nzchar(Suggestion), Suggestion, "-")
    )


  return(out)
}


# ----- run all validators -----

clean_all_validations <- function(...) {

    dots <- rlang::enquos(...)

    reports <- purrr::imap(dots, function(x, nm) {
      res <- pretty_validate_report(rlang::eval_tidy(x))
      if (!is.null(res)) {
        res$Table <- nm
      }
      res
    }) |>
      purrr::compact()

    if (length(reports) == 0) return(NULL)

  reports_combo <- dplyr::bind_rows(reports) |>
    select(Table, Column, Issue, `Row Index`, Suggestion) |>
    dplyr::arrange(Table, Column, Issue)

  return(reports_combo)
}

valid_taxonomy <- function(x) {

  valid_common <- x |>
    pull(common_name) |>
    unique() |>
    na.omit()

  valid_scientific <- x |>
    pull(scientific_name) |>
    unique() |>
    na.omit()

  valid_taxonomy <- list(
    valid_common = valid_common,
    valid_scientific = valid_scientific
  )
  return(valid_taxonomy)
}


# ----- validate tbl_samples ------
validate_tbl_samples <- function(df) {

  required_cols <- c(
    "pi_name", "source_id", "user_sample_id", "date", "month",
    "season", "sample_year", "common_name", "scientific_name", "genus",
    "family", "sex", "lifestage", "wild_lab",
    "trt_description", "age", "length_mm", "length_type", "weight_g",
    "composite", "composite_n", "tissue_type", "sample_procedure",
    "location", "waterbody", "area", "site", "site_depth", "latitude",
    "longitude", "calorimetry_method", "calorimeter_conversion_factor",
    "sample_weight", "sample_weight_type", "energy_measurement",
    "energy_units", "slope", "intercept", "x_units", "y_units", "percent_water",
    "percent_ash", "percent_lipid", "percent_protein", "percent_carbon",
    "percent_nitrogen", "d13c", "d15n", "d34s", "c_n"
  )



  rules <- validator(

    # ---- structure ----
    contains(required_cols),


    # ---- not null ----
    !is.na(pi_name),
    !is.na(source_id),
    !is.na(scientific_name),
    !is.na(wild_lab),
    !is.na(tissue_type),
    !is.na(sample_procedure),
    !is.na(waterbody),

    # ---- date ----
    !is.na(as.Date(date, origin = "1899-12-30")),

    # ---- ranges ----
    month >= 1 & month <= 12,

    # ---- sets ----
    season %in% c("spring", "summer", "fall", "winter"),
    lifestage %in% c("fry", "larvae", "juvenile", "adult"),
    sex %in% c("male", "female", "unknown"),
    length_type %in% c("Total", "Fork", "Standard", "Carapace"),
    composite %in% c("individual", "composite", "mean", "equation"),
    tissue_type %in% c("muscle", "liver", "stomach", "scales",
                       "otolith", "spines", "cleithra", "whole body"),
    sample_procedure %in% c("wet", "dried"),
    calorimetry_method %in% c("Parr oxygen bomb", "Parr semi-micro oxygen bomb",
                              "Phillipson microbomb", "Gentry-Weigert bomb",
                              "Unknown bomb", "Proximate composition",
                              "Organic analysis", "Wet digestion", "Unknown"),
    sample_weight_type %in% c("wet", "dry"),

    # ---- numeric ----
    is.numeric(length_mm),
    is.numeric(weight),
    is.numeric(age),
    is.numeric(composite_n),
    is.numeric(latitude),
    is.numeric(longitude),
    is.numeric(calorimeter_conversion_factor),
    is.numeric(sample_weight),
    is.numeric(energy_measurement),
    is.numeric(percent_water),
    is.numeric(percent_ash),
    is.numeric(percent_lipid),
    is.numeric(percent_protein),
    is.numeric(percent_carbon),
    is.numeric(percent_nitrogen),
    is.numeric(d13c),
    is.numeric(d15n),
    is.numeric(d34s),
    is.numeric(c_n),
    # ---- see if these are true -----
    .valid_common_name == TRUE,
    .valid_scientific_name == TRUE

  )

  out <- confront(df, rules)


  return(out)
}
