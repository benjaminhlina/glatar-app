# ---- add_valid_col
add_valid_cols <- function(df) {
  df <- df |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.character) &
        !any_of(c(
          "fatty_acid_type",
          "thiamine_type",
          "energy_measurment_units"
        )),
      tolower
    )) |>
    dplyr::mutate(
      .date = is.na(date) | grepl("^\\d{4}-\\d{2}-\\d{2}$", date),
      .ed = dplyr::case_when(
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
      .data_type = is.na(data_type) |
        data_type %in%
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
