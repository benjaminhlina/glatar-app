# ----- pretty validator -----
pretty_validate_report <- function(confrontation, table_name = NULL) {
  df <- validate::as.data.frame(confrontation)

  original_data <- confrontation$._keys$keyset

  common_name_suggestions <- attr(original_data, "common_name_suggestions")
  scientific_name_suggestions <- attr(
    original_data,
    "scientific_name_suggestions"
  )

  cli::cli_alert_info(
    "Common suggestions found:
                      {!is.null(common_name_suggestions)}"
  )
  cli::cli_alert_info(
    "Scientific suggestions found:
                      {!is.null(scientific_name_suggestions)}"
  )

  # if valdiate doens't return anything then  return nulll
  if (nrow(df) == 0) {
    return(NULL)
  }

  # transfer forw number
  df <- df |>
    dplyr::group_by(name) |>
    dplyr::mutate(
      data_row = dplyr::row_number() # This cycles 1, 2, 3, 4... within each rule
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      col_name = dplyr::case_when(
        grepl("%vin% colnames", expression) ~ gsub(
          '.*"([^"]+)".*',
          '\\1',
          expression
        ),
        # Handle %vin% expressions: get the word before %vin%
        grepl("%vin%", expression) ~ sub(
          "^\\s*(\\w+)\\s+%vin%.*",
          "\\1",
          expression
        ),

        # Handle comparison expressions (month - 1 >= ..., etc.)
        grepl("[-+].*[><=]", expression) ~ sub(
          "^\\s*(\\w+)\\s+[-+].*",
          "\\1",
          expression
        ),

        # Handle function calls with commas - get first word before
        # comma in innermost parens
        grepl("\\([^()]*,", expression) ~ {
          temp <- sub(".*\\(([^()]+)\\).*", "\\1", expression)
          sub("^\\s*([^,]+).*", "\\1", temp)
        },

        # Handle function calls without commas - get content of innermost parens
        grepl("\\(", expression) ~ sub(".*\\(([^()]+)\\).*", "\\1", expression),

        grepl("^\\.valid_", expression) ~ sub(
          "^\\.valid_([^ =]+).*",
          "\\1",
          expression
        ),

        # Default: return the expression as-is
        .default = expression
      ),
      # Clean up any remaining quotes or whitespace
      col_name = trimws(gsub('"', '', col_name))
      # col_name = sub(".*\\(([^,\\)]+).*", "\\1", expression)
    )

  # ----- grab only bad columns -----
  bad <- df |>
    dplyr::filter(value %in% FALSE)

  # ----- if tehre are non-return NULL -----
  if (nrow(bad) == 0) {
    return(NULL)
  }

  rule_col <- rule_match(bad$expression, "col")
  rule_issue <- rule_match(bad$expression, "issue")

  cli::cli_alert_success(
    "What is Descirption {.field {unique(bad$name)}}"
  )

  # ----- create pretty names -----
  out <- bad |>
    dplyr::mutate(
      col_name = dplyr::coalesce(rule_col, col_name),
      Issue = dplyr::case_when(
        grepl(
          "nrow\\(\\.\\) == 1",
          expression
        ) ~ "Sheet is empty - please enter in
        data and reupload",

        expression %in% "publication_type" ~ "Invalid publication type - must
        be Journal Article, Book, Book Section, Report, or Unpublished",

        grepl('%vin% colnames', expression) ~ paste0(
          "Missing required column(s):",
          gsub('.*"([^"]+)".*', '\\1', expression),
          " - you have altered the data entry template -
          please reupload an unaltered file"
        ),

        grepl("is.na\\(as.Date", expression) ~ "Date format does not follow the
        required format of yyyy-mm-dd or is an invalid date",

        grepl("is.na", expression) ~ "Required field - cannot be empty",
        grepl("numeric__", name) ~ "Field needs to be numeric",
        grepl("common_name", expression) ~ "Common name not found in database",
        grepl("scientific_name", expression) ~ "Scientific name not found in
        database",

        # !!!validation_rules,
        .default = expression
      ),
    ) |>
    dplyr::select(Row = data_row, Column = col_name, Issue)

  if (!is.character(common_name_suggestions)) {
    common_name_suggestions <- character(0)
  }

  if (!is.character(scientific_name_suggestions)) {
    scientific_name_suggestions <- character(0)
  }

  row_offset <- if (table_name %in% c("tbl_sources", "tbl_submission")) {
    4
  } else if (table_name %in% c("tbl_samples")) {
    5
  } else {
    0
  }

  out <- out |>
    dplyr::mutate(
      Suggestion = dplyr::case_when(
        Column %in%
          "common_name" &
          !is.null(common_name_suggestions) &
          Row <= length(common_name_suggestions) &
          !is.na(common_name_suggestions[Row]) ~
          paste0("Did you mean: ", common_name_suggestions[Row], "?"),
        Column == "scientific_name" &
          !is.null(scientific_name_suggestions) &
          Row <= length(scientific_name_suggestions) &
          !is.na(scientific_name_suggestions[Row]) ~
          paste0("Did you mean: ", scientific_name_suggestions[Row], "?"),
        .default = NA
      ),
      Row = Row + row_offset
    )

  cli::cli_alert_info("Rows with suggestions: {sum(!is.na(out$Suggestion))}")
  cli::cli_alert_success(
    "Suggestions are the following:
                         {.val {paste(unique(na.omit(out$Suggestion)),
                         collapse = ';')}}"
  )
  # ---- clean this up -----
  out <- out |>
    dplyr::group_by(Column, Issue) |>
    dplyr::summarise(
      `Row Index` = compress_row_index(paste(
        sort(unique(Row)),
        collapse = ", "
      )),
      Suggestion = paste(unique(na.omit(Suggestion)), collapse = "; ")
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Suggestion = dplyr::if_else(nzchar(Suggestion), Suggestion, "-")
    )

  return(out)
}
# compress_row_index(paste(sort(unique(Row)), collapse = ", ")),

# ----- run all validators -----

# ---- get valid taxonomy ------
valid_taxonomy <- function(x) {
  valid_common <- x |>
    dplyr::pull(common_name) |>
    unique() |>
    na.omit()

  valid_scientific <- x |>
    dplyr::pull(scientific_name) |>
    unique() |>
    na.omit()

  valid_taxonomy <- list(
    valid_common = valid_common,
    valid_scientific = valid_scientific
  )
  return(valid_taxonomy)
}


# -----the_golden_lance is validate_tbl_samples ------
# galtar and the golden lance
the_golden_lance <- function(df) {
  required_fields <- c(
    "pi_name",
    "source_id",
    "user_sample_id",
    "date",
    "month",
    "season",
    "sample_year",
    "common_name",
    "scientific_name",
    "genus",
    "family",
    "sex",
    "lifestage",
    "wild_lab",
    "trt_description",
    "age_year",
    "length_mm",
    "length_type",
    "weight_g",
    "data_type",
    "data_type_n",
    "tissue_type",
    "sample_procedure",
    "location",
    "waterbody",
    "area",
    "site",
    "site_depth",
    "latitude",
    "longitude",
    "calorimetry_method",
    "calorimeter_conversion_factor",
    "sample_weight",
    "sample_weight_type",
    "energy_measurement",
    "energy_units",
    "slope",
    "intercept",
    "x_units",
    "y_units",
    "percent_water",
    "percent_ash",
    "percent_lipid",
    "percent_protein",
    "percent_carbon",
    "percent_nitrogen",
    "d13c",
    "d15n",
    "d34s",
    "d18o",
    "d2h",
    "c_n"
  )

  # optional_fields <- c(
  #   "percent_lipid_composition",
  #   "lipid_percent_type",
  #   "lipid_type", "fatty_acid_measurement",
  #   "fatty_acid_unit",
  #   "fatty_acid_type",
  #   "amino_acid_measurement",
  #   "amino_acid_unit",
  #   "amino_acid_type",
  #   "thiamine_nmol_g",
  #   "mercury_ppm",
  #   "mercury_type",
  #   "total_pcb_ng_g",
  #   "pcb_congener_ng_g",
  #   "pcb_congener_type"
  # )

  # Which optional columns were actually submitted
  # present_optional <- optional_fields[optional_fields %in% colnames(df)]

  if (!all(required_fields %in% colnames(df))) {
    rules <- do.call(
      validate::validator,
      c(rule_column_names(required_fields))
    )
    # } else if {
    #   # Build rules for required fields + any optional fields that are present
    #   all_fields_to_validate <- c(required_fields, present_optional)

    #   rules <- do.call(
    #     validate::validator,
    #     c(rule_column_names(all_fields_to_validate))
    #   )
  } else if (nrow(df) == 0) {
    rules <- validate::validator(
      nrow(.) == 1
    )
  } else {
    not_null_fields <- c(
      "pi_name",
      "source_id",
      "scientific_name",
      "wild_lab",
      "tissue_type",
      "sample_procedure",
      "waterbody"
    )

    numeric_fields <- c(
      "sample_year",
      "length_mm",
      "weight_g",
      "age_year",
      "data_type_n",
      "latitude",
      "longitude",
      "calorimeter_conversion_factor",
      "sample_weight",
      "energy_measurement",
      "slope",
      "intercept",
      "percent_water",
      "percent_ash",
      "percent_lipid",
      "percent_protein",
      "percent_carbon",
      "percent_nitrogen",
      "d13c",
      "d15n",
      "d34s",
      "d18o",
      "d2h",
      "c_n",
      "percent_lipid_composition",
      "fatty_acid_measurement",
      "amino_acid_measurement",
      "thiamine_nmol_g",
      "thiaminase_activity",
      "mercury_ppm",
      "total_pcb_ng_g",
      "pcb_congener_ng_g"
    )

    flag_fields <- c(
      ".date",
      ".month",
      ".season",
      ".lifestage",
      ".sex",
      ".length_type",
      ".data_type",
      ".tissue_type",
      ".sample_procedure",
      ".calorimetry_method",
      ".sample_weight_type",
      ".valid_common_name",
      ".valid_scientific_name",
      ".ed",
      ".isotope_lipid_correction",
      ".lipid_percent_type",
      ".lipid_type",
      ".mecury_type",
      ".fatty_acid_unit",
      ".fatty_acid_type",
      ".amino_acid_unit",
      ".amino_acid_type",
      ".thiamine_type"
      # ".pcb_congener_type"
    )

    rule_strings <- c(
      paste0("!is.na(", not_null_fields, ")"),
      paste0(flag_fields, " == TRUE"),
      paste0(
        "grepl('^-?[0-9]*\\\\.?[0-9]+$', as.character(",
        numeric_fields,
        ")) | is.na(",
        numeric_fields,
        ")"
      )
    )

    rules_to_check <- data.frame(
      name = c(
        paste0("not_null__", not_null_fields),
        paste0("flag__", flag_fields),
        paste0("numeric__", numeric_fields)
      ),
      rule = rule_strings
    )
    rules <- validate::validator(
      .data = rules_to_check
    )
  }

  out <- validate::confront(df, rules)

  return(out)
}

# ----- validate source ------
validate_tbl_sources <- function(df) {
  required_fields <- c(
    "source_id",
    "publication_type",
    "author_names",
    "affiliation",
    "email",
    "title",
    "publication_year",
    "journal_name",
    "volume",
    "issue",
    "pages",
    "publisher",
    "editor",
    "ibsn",
    "doi"
  )

  if (!all(required_fields %in% colnames(df))) {
    rules <- do.call(
      validate::validator,
      c(rule_column_names(required_fields))
    )
  } else if (nrow(df) == 0) {
    rules <- validate::validator(
      nrow(.) == 1
    )
  } else {
    rules <- validate::validator(
      # ---- required fields
      !is.na(source_id),
      !is.na(publication_type),

      !is.na(author_names),
      !is.na(publication_year),
      !is.na(email),
      # !is.na(title),

      publication_type %in%
        c("Journal Article", "Book", "Book Section", "Report", "Unpublished")
    )
  }

  out <- validate::confront(df, rules)

  return(out)
}
# ----- validate source ------
validate_tbl_submission <- function(df) {
  required_fields <- c(
    "submitted_by",
    "submission_email",
    "submission_affiliation"
  )

  if (!all(required_fields %in% colnames(df))) {
    rules <- do.call(
      validate::validator,
      c(rule_column_names(required_fields))
    )
  } else if (nrow(df) == 0) {
    rules <- validate::validator(
      nrow(.) == 1
    )
  } else {
    rules <- do.call(
      validate::validator,
      c(
        rule_len(required_fields),
        rule_na(required_fields),
        rule_blank(required_fields),
        rule_email
      )
    )
  }
  out <- validate::confront(df, rules)

  return(out)
}
