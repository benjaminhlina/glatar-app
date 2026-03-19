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

  # ----- create pretty names -----
  out <- bad |>
    dplyr::mutate(
      col_name = dplyr::coalesce(rule_col, col_name),
      Issue = dplyr::case_when(
        grepl("nrow(.) == 1", expression) ~ "Sheet is empty - please enter in
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

        grepl("is.numeric", expression) ~ "Must be numeric value",

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

  row_offset <- if (table_name %in% c("tbl_source", "tbl_submission")) {
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


# ----- validate tbl_samples ------
validate_tbl_samples <- function(df) {
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
  #   "lipid_type",
  #   "fatty_acid_measurement",
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
    rules <- validate::validator(
      # ---- structure ----

      # ---- not null ----
      !is.na(pi_name),
      !is.na(source_id),
      !is.na(scientific_name),
      !is.na(wild_lab),
      !is.na(tissue_type),
      !is.na(sample_procedure),
      !is.na(waterbody),

      # ---- date ----
      # need to make this as a true false that then gets checked
      .date == TRUE,

      # ---- ranges ----
      .month == TRUE,
      .season == TRUE,
      .lifestage == TRUE,
      .sex == TRUE,
      .length_type == TRUE,
      .data_type == TRUE,
      .tissue_type == TRUE,
      .sample_procedure == TRUE,
      .calorimetry_method == TRUE,
      .sample_weight_type == TRUE,

      # ---- numeric ----
      is.numeric(length_mm),
      is.numeric(weight_g),
      is.numeric(age_year),
      is.numeric(data_type_n),
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
      is.numeric(thiamine_nmol_g),
      is.numeric(thiamine_nmol_g_sd),
      is.numeric(thiaminase_activity),
      is.numeric(thiaminase_activity_sd),
      is.numeric(mercury_ppm),
      is.numeric(total_pcb_ng_g),
      is.numeric(pcb_congener_ng_g),
      is.numeric(pcb_congener_type),

      # ---- see if these are true -----
      .valid_common_name == TRUE,
      .valid_scientific_name == TRUE,
      .ed == TRUE,
      .lipid_percent_type == TRUE,
      .lipid_type == TRUE,
      .mecury_type == TRUE,
      .fatty_acid_unit == TRUE,
      .fatty_acid_type == TRUE,
      .amino_acid_unit == TRUE,
      .amino_acid_type == TRUE,
      .thiamine_type == TRUE
    )
  }

  out <- validate::confront(df, rules)

  return(out)
}

# ----- validate source ------
validate_tbl_source <- function(df) {
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
      !is.na(title),

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
