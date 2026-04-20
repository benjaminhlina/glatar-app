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
    "Contaminates",
    "Thiamine"
  )
  return(themes)
}
