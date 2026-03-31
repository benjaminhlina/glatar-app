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

# ------ clean data types -----

clean_data_tables <- function(
  df,
  flag_cols,
  type,
  group_cols,
  filter_coords = TRUE
) {
  group_vars <- names(tidyselect::eval_select(rlang::enquo(group_cols), df))

  if (isTRUE(filter_coords)) {
    df <- df |>
      dplyr::filter(!is.na(longitude))
  }

  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(flag_cols), ~ dplyr::coalesce(., 0L))
    ) |>
    # Use group_by + summarise (not distinct) so flags are OR'd across samples
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      n_samples = dplyr::n(),
      dplyr::across(dplyr::all_of(flag_cols), ~ max(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      data_tables = purrr::pmap_chr(
        dplyr::pick(dplyr::all_of(flag_cols)),
        \(...) {
          flags <- c(...)
          labels <- unname(type)[as.logical(flags)]
          if (length(labels) == 0) {
            "None"
          } else {
            paste(labels, collapse = ", ")
          }
        }
      )
    )

  return(df)
}
