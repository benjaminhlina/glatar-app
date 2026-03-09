add_valid_cols <- function(df) {
  df <- df |>
    dplyr::across(
      where(is.character) & !any_of(c("fatty_acid_type", "thiamine_type")),
      tolower
    ) |>
    dplyr::mutate(
      .date = is.na(date) | grepl("^\\d{4}-\\d{2}-\\d{2}$", date),
      .ed = case_when(
        is.na(sample_weight_type) ~ NA,
        sample_weight_type == "wet" ~ energy_measurement >= 0 &
          energy_measurement <= 13000,
        sample_weight_type == "dry" ~ energy_measurement >= 1000 &
          energy_measurement <= 43000,
        .default = NA
      ),
      .month = is.na(month) | (month >= 1 & month <= 12),
      .season = is.na(season) |
        season %in% c("spring", "summer", "fall", "winter"),
      .lifestage = is.na(lifestage) |
        lifestage %in% c("fry", "larva", "nymph", "pupa", "juvenile", "adult"),
      .sex = is.na(sex) | sex %in% c("male", "female", "unknown", "both"),
      .length_type = is.na(length_type) |
        length_type %in% c("total", "fork", "standard", "carapace"),
      .composite = is.na(composite) |
        composite %in%
          c(
            "individual",
            "composite",
            "mean",
            "equation"
          ),
      .tissue_type = is.na(tissue_type) |
        tissue_type %in%
          c(
            "belly flap",
            "blood",
            "carcass",
            "cleithra",
            "eye lens",
            "egg",
            "fin",
            "gonad",
            "heart",
            "liver",
            "muscle",
            "otolith",
            "scale",
            "spine",
            "stomach",
            "viscera",
            "whole body",
            "whole body (gonads removed)",
            "whole body (shell removed)",
            "whole body (stomach removed)"
          ),
      .sample_procedure = is.na(sample_procedure) |
        sample_procedure %in%
          c(
            "wet",
            "dried"
          ),
      .calorimetry_method = is.na(calorimetry_method) |
        calorimetry_method %in%
          c(
            "adiabatic bomb calorimeter",
            "gentry-weigert bomb",
            "isoperibol bomb calorimeter",
            "organic analysis",
            "parr oxygen bomb",
            "parr semi-micro oxygen bomb",
            "parr 1261 bomb calorimeter",
            "phillipson microbomb",
            "proximate composition",
            "unknown",
            "unknown bomb",
            "wet digestion"
          ),
      .sample_weight_type = is.na(sample_weight_type) |
        sample_weight_type %in% c("wet", "dry"),
    )

  if (
    any(
      colnames(df) %in%
        c(
          "lipid_percent_type",
          "lipid_type",
          "fatty_acid_unit",
          "fatty_acid_type",
          "amino_acid_unit",
          "amino_acid_type"
        )
    )
  ) {
    df <- df |>
      dplyr::mutate(
        .lipid_percent_type = is.na(lipid_percent_type) |
          lipid_percent_type %in% c("% sample weight", "% total lipids"),
        .lipid_type = is.na(lipid_type) |
          lipid_type %in%
            c(
              "fatty acids",
              "phospholipids",
              "sterols",
              "triacylglycerides"
            ),
        .fatty_acid_unit = is.na(fatty_acid_unit) |
          fatty_acid_unit %in% c("ug/mg sample weight", "% total fatty acid"),
        .fatty_acid_type = is.na(fatty_acid_type) |
          fatty_acid_type %in%
            c(
              "∑SFA (saturated)",
              "∑UFA (unsaturated)",
              "∑MUFA (monounsaturated)",
              "∑PUFA (polyunsaturated)",
              "∑n-3",
              "∑n-6",
              "14:0",
              "16:0",
              "18:0",
              "16:1n-7 (POA)",
              "18:1n-9 (OA)",
              "18:3n-3 (ALA)",
              "18:4n-3 (SDA)",
              "18:2n-6 (LIN)",
              "20:4n-6 (ARA)",
              "20:5n-3 (EPA)",
              "22:5n-6 (DPA)",
              "22:6n-3 (DHA)"
            ),
        .amino_acid_unit = is.na(amino_acid_unit) |
          amino_acid_unit %in% c("ug/mg sample weight", "% total protein"),
        .amino_acid_type = is.na(amino_acid_type) |
          amino_acid_type %in%
            c(
              "alanine",
              "arginine",
              "aspartic acid",
              "cysteine",
              "cystine",
              "glutamic acid",
              "glycine",
              "histidine",
              "isoleucine",
              "leucine",
              "lysine",
              "methionine",
              "phenylalanine",
              "proline",
              "serine",
              "threonine",
              "tyrosine",
              "valine"
            ),
        .mercury_type = is.na(mercury_type) |
          mercury_type %in% c("total mercury", "methyl mercury"),
        .thiamine_type = is.na(thiamine_type) |
          thiamine_type %in%
            c(
              "Th (free thiamine)",
              "TMP (thiamine monophosphate)",
              "TPP (thiamine pyrophosphate)",
              "TTh (total thiamine)"
            ),
      )
  }

  return(df)
}

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
  common_check <- check_taxonomy_match(df$common_name, valid_common_sentence)
  sci_check <- check_taxonomy_match(df$scientific_name, valid_sci_sentence)

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
    if (is.na(input_values[i])) {
      return(NA)
    }

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
pretty_validate_report <- function(confrontation, table_name = NULL) {
  df <- as.data.frame(confrontation, add_columns = TRUE)

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
    group_by(name) |>
    mutate(
      data_row = row_number() # This cycles 1, 2, 3, 4... within each rule
    ) |>
    ungroup() |>
    mutate(
      col_name = case_when(
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
    filter(value %in% FALSE)

  # ----- if tehre are non-return NULL -----
  if (nrow(bad) == 0) {
    return(NULL)
  }

  # validation_rules <- list(
  #   make_validation_message("month", "Month must be between 1 and 12"),
  #   make_validation_message(
  #     "season",
  #     "Invalid season - must be spring, summer, fall, or winter"
  #   ),
  #   make_validation_message(
  #     "sex",
  #     "Invalid sex - must be female, male, unknown, or both"
  #   ),
  #   make_validation_message(
  #     "lifestage",
  #     "Invalid lifestage - must be fry, larva, nymph, pupa, juvenile, or adult"
  #   ),
  #   make_validation_message(
  #     "length_type",
  #     "Invalid length type - must be total, fork, standard, or carapace"
  #   ),
  #   make_validation_message(
  #     "composite",
  #     "Invalid composite - must be individual, composite, mean, or equation"
  #   ),
  #   make_validation_message(
  #     "tissue_type",
  #     "Invalid tissue type - must be a recognised tissue type see data dictionary if unfamiliar"
  #   ),
  #   make_validation_message(
  #     "sample_procedure",
  #     "Invalid sample procedure - must be wet or dried"
  #   ),
  #   make_validation_message(
  #     "calorimetry_method",
  #     "Invalid calorimetry method - must be a recognised calorimetry method"
  #   ),
  #   make_validation_message(
  #     "sample_weight_type",
  #     "Invalid sample weight type - must be wet or dry"
  #   ),
  #   make_validation_message(
  #     "lipid_percent_type",
  #     "Invalid lipid percent type - must be % sample weight or % total lipids"
  #   ),
  #   make_validation_message(
  #     "lipid_type",
  #     "Invalid lipid type - must be fatty acids, phospholipids, sterols, or triacylglycerides"
  #   ),
  #   make_validation_message(
  #     "fatty_acid_unit",
  #     "Invalid fatty acid unit - must be ug/mg sample weight or % total fatty acid"
  #   ),
  #   make_validation_message(
  #     "fatty_acid_type",
  #     "Invalid fatty acid type - must be a recognized fatty acid (e.g. 20:5n-3 (EPA), 22:6n-3 (DHA), ∑PUFA, etc.)"
  #   ),
  #   make_validation_message(
  #     "amino_acid_unit",
  #     "Invalid amino acid unit - must be ug/mg sample weight or % total protein"
  #   ),
  #   make_validation_message(
  #     "amino_acid_type",
  #     "Invalid amino acid type - must be a recognized amino acid (e.g. Alanine, Lysine, Glycine, etc.)"
  #   ),
  #   make_validation_message(
  #     "ed",
  #     "Invalid energy measurement - must be within appropriate ranges for Joules/g wet or dry weight"
  #   )
  # )
  # ----- create pretty names -----
  out <- bad |>
    mutate(
      Issue = case_when(
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
        grepl("\\.month", expression) ~ "Month must be between 1 and 12",
        grepl(
          "\\.season",
          expression
        ) ~ "Invalid season - must be spring, summer, fall, or winter",
        grepl(
          "\\.sex",
          expression
        ) ~ "Invalid sex - must be female, male, unknown, or both",
        grepl(
          "\\.lifestage",
          expression
        ) ~ "Invalid lifestage - must be fry, larva, nymph, pupa, juvenile, or adult",
        grepl(
          "\\.length_type",
          expression
        ) ~ "Invalid length type - must be total, fork, standard, or carapace",
        grepl(
          "\\.composite",
          expression
        ) ~ "Invalid composite - must be individual, composite, mean, or equation",
        grepl(
          "\\.tissue_type",
          expression
        ) ~ "Invalid tissue type - must be a recognised tissue type see data dictionary if unfamiliar",
        grepl(
          "\\.sample_procedure",
          expression
        ) ~ "Invalid sample procedure - must be wet or dried",
        grepl(
          "\\.calorimetry_method",
          expression
        ) ~ "Invalid calorimetry method - must be a recognised calorimetry method",
        grepl(
          "\\.sample_weight_type",
          expression
        ) ~ "Invalid sample weight type - must be wet or dry",
        grepl(
          "\\.lipid_percent_type",
          expression
        ) ~ "Invalid lipid percent type - must be % sample weight or % total lipids",
        grepl(
          "\\.lipid_type",
          expression
        ) ~ "Invalid lipid type - must be fatty acids, phospholipids, sterols, or triacylglycerides",
        grepl(
          "\\.fatty_acid_unit",
          expression
        ) ~ "Invalid fatty acid unit - must be ug/mg sample weight or % total fatty acid",
        grepl(
          "\\.fatty_acid_type",
          expression
        ) ~ "Invalid fatty acid type - must be a recognized fatty acid (e.g. 20:5n-3 (EPA), 22:6n-3 (DHA), ∑PUFA, etc.)",
        grepl(
          "\\.amino_acid_unit",
          expression
        ) ~ "Invalid amino acid unit - must be ug/mg sample weight or % total protein",
        grepl(
          "\\.amino_acid_type",
          expression
        ) ~ "Invalid amino acid type - must be a recognized amino acid (e.g. Alanine, Lysine, Glycine, etc.)",
        grepl(
          "\\.mercury_type",
          expression
        ) ~ "Invalid mercury type type - must be total mercury or methyl mercury",
        grepl(
          "\\.thiamine_type ",
          expression
        ) ~ "Invalid thiamine type type - must be the correct Vitamer",
        grepl(
          "\\.ed",
          expression
        ) ~ "Invalid energy measurement - must be within appropriate ranges for Joules/g wet or dry weight",

        # !!!validation_rules,
        .default = expression
      ),
      col_name = case_when(
        grepl("\\.ed", expression) ~ "energy_measurment",
        grepl("\\.month", expression) ~ "month",
        grepl("\\.date", expression) ~ "date",
        grepl("\\.season", expression) ~ "season",
        grepl("\\.sex", expression) ~ "sex",
        grepl("\\.lifestage", expression) ~ "lifestage",
        grepl("\\.length_type", expression) ~ "length_type",
        grepl("\\.composite", expression) ~ "composite",
        grepl("\\.tissue_type", expression) ~ "tissue_type",
        grepl("\\.sample_procedure", expression) ~ "sample_procedure",
        grepl("\\.calorimetry_method", expression) ~ "calorimetry_method",
        grepl("\\.sample_weight_type", expression) ~ "sample_weight_type",
        grepl("\\.amino_acid_type", expression) ~ "amino_acid_type",
        grepl("\\.amino_acid_unit", expression) ~ "amino_acid_unit",
        grepl("\\.lipid_type", expression) ~ "lipid_type",
        grepl("\\.mercury_type", expression) ~ "mercury_type",
        grepl("\\.thiamine_type ", expression) ~ "thiamine_type",
        .default = col_name
      )
    ) |>
    select(Row = data_row, Column = col_name, Issue)

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
    mutate(
      Suggestion = case_when(
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
    group_by(Column, Issue) |>
    summarise(
      `Row Index` = compress_row_index(paste(
        sort(unique(Row)),
        collapse = ", "
      )),
      Suggestion = paste(unique(na.omit(Suggestion)), collapse = "; ")
    ) |>
    ungroup() |>
    mutate(
      Suggestion = if_else(nzchar(Suggestion), Suggestion, "-")
    )

  return(out)
}
# compress_row_index(paste(sort(unique(Row)), collapse = ", ")),

# ----- run all validators -----

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
    select(Table, Column, Issue, `Row Index`, Suggestion) |>
    dplyr::arrange(Table, Column, Issue)

  return(reports_combo)
}

# ---- get valid taxonomy ------
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
    "composite",
    "composite_n",
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

  if (!all(required_fields %in% colnames(df))) {
    rules <- do.call(
      validate::validator,
      c(rule_column_names(required_fields))
    )
  } else if (nrow(df) == 0) {
    rules <- validator(
      nrow(.) == 1
    )
  } else {
    rules <- validator(
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
      .composite == TRUE,
      .tissue_type == TRUE,
      .sample_procedure == TRUE,
      .calorimetry_method == TRUE,
      .sample_weight_type == TRUE,

      # ---- numeric ----
      is.numeric(length_mm),
      is.numeric(weight_g),
      is.numeric(age_year),
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

  out <- confront(df, rules)

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
    rules <- validator(
      nrow(.) == 1
    )
  } else {
    rules <- validator(
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

  out <- confront(df, rules)

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
    rules <- validator(
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
  out <- confront(df, rules)

  return(out)
}
