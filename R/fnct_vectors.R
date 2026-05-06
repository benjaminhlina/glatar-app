# ---- Data tabels -----
#' Vector functions
#'
#' These functions produce a vector with information related to
#' the database. These may eventually be moved to being data
#' objects that are loaded when the package is loaded.
#'
#' @name vector_functions
#'
#' @export
data_tables <- function() {
  data_tables <- c(
    "tbl_amino_acid" = "Amino Acid",
    "tbl_calorimetry" = "Calorimetry",
    "tbl_contaminants" = "Contaminants",
    "tbl_fatty_acid" = "Fatty Acid",
    "tbl_isotope" = "Isotope",
    "tbl_lipid_composition" = "Lipid Composition",
    "tbl_proxcomp" = "Proximate Composition",
    "tbl_thiamine" = "Thiamine"
  )

  return(data_tables)
}

# ----- data types -----
#' @name vector_functions
#'
#' @export

data_types <- function() {
  data_types <- c(
    "Individual",
    "Composite",
    "Mean",
    "SD",
    "Equation"
  )
  return(data_types)
}

# ---- flag fields -----

flag_fields <- function() {
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
    ".energy_measurement",
    ".isotope_lipid_correction",
    ".lipid_percent_type",
    ".lipid_type",
    ".mercury_type",
    ".fatty_acid_unit",
    ".fatty_acid_type",
    ".amino_acid_unit",
    ".amino_acid_type",
    ".thiamine_type"
    # ".pcb_congener_type"
  )
  return(flag_fields)
}


# ----- good groups ------
#' @name vector_functions
#'
#' @export
good_groups <- function() {
  good_groups <- c(
    "pi_name",
    "month",
    "sample_year",
    "common_name",
    "scientific_name",
    "genus",
    "tribe",
    "subfamily",
    "family",
    "superfamily",
    "suborder",
    "order",
    "superorder",
    "class",
    "superclass",
    "phylum",
    "kingdom",
    "organism_type",
    "tsn",
    "sex",
    "life_stage",
    "wild_lab",
    "age",
    "composite",
    "tissue_type",
    "sample_procedure",
    "trt_description",
    "waterbody",
    "area",
    "site",
    "site_depth"
  )
  return(good_groups)
}

# ------ not null felisds ----
#' @name vector_functions
#'
#' @export
not_null_fields <- function() {
  not_null_fields <- c(
    "pi_name",
    "source_id",
    "scientific_name",
    "wild_lab",
    "tissue_type",
    "sample_procedure",
    "waterbody"
  )
  return(not_null_fields)
}
# ----- nuemric fields ------
#' @name vector_functions
#'
#' @export
numeric_fields <- function() {
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
  return(numeric_fields)
}

# ----- optional feilds ------
sample_optional_fields <- function() {
  optional_fields <- c(
    "percent_lipid_composition",
    "lipid_percent_type",
    "lipid_type",
    "fatty_acid_measurement",
    "fatty_acid_unit",
    "fatty_acid_type",
    "amino_acid_measurement",
    "amino_acid_unit",
    "amino_acid_type",
    "thiamine_nmol_g",
    "mercury_ppm",
    "mercury_type",
    "total_pcb_ng_g",
    "pcb_congener_ng_g",
    "pcb_congener_type"
  )
  return(optional_fields)
}

# ---- protected tabs ----
#' @name vector_functions
#'
#' @export
protected_tabs <- function() {
  pt <- c("view_data", "insert_data")
  return(pt)
}

# ----- required fields -----
#' @name vector_functions
#'
#' @export

sample_required_fields <- function() {
  sample_required_fields <- c(
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
  return(sample_required_fields)
}


# ----- sources ------
#' @name vector_functions
#'
#' @export

sources_required_fields <- function() {
  sources_required_fields <- c(
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
}

# ----- tbl submission -----
#' @name vector_functions
#'
#' @export
submission_required_fields <- function() {
  submission_required_fields <- c(
    "submitted_by",
    "submission_email",
    "submission_affiliation"
  )
  return(submission_required_fields)
}

# ----- themes -----
#' @name vector_functions
#'
#' @export
themes <- function() {
  themes <- c(
    "Energy Density",
    "Body Composition",
    "Stable Isotopes",
    "Amino Acids",
    "Fatty Acids",
    "Contaminants",
    "Thiamine"
  )
  return(themes)
}
