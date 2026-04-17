# ----- all the data types we want -----
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
