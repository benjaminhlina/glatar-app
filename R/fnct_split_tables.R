#' Split Tables
#'
#' Split tables takes validated and cleaned data submissions and splits
#' them by columns into the table name from the data dictionary.
#'
#' @param df the `data.frame` containing the submitted data.
#' @param tables_to_split the information from data dictionary
#' determing which columns belong to which tables.

#' @return returns a `list`containing the tables named
#' apporipately and split into their proper tables for
#' database submission.
#'
#' @export

split_tables <- function(df, tables_to_split) {
  lapply(names(tables_to_split), function(tbl_name) {
    map <- tables_to_split[[tbl_name]]
    cols <- map$field_name

    id_cols <- if (tbl_name == "tbl_samples") {
      c("sample_id", "submission_id")
    } else {
      "sample_id"
    }
    # --- select only payload columns first
    out <- df |>
      dplyr::select(dplyr::any_of(c(id_cols, cols))) |>
      dplyr::filter(dplyr::if_any(dplyr::any_of(cols), ~ !is.na(.x)))

    if (nrow(out) > 0) {
      cli::cli_alert_success("Using data for table: {tbl_name}")
      return(out)
    } else {
      cli::cli_alert_info("Skipping empty table: {tbl_name}")
      NULL
    }
  }) |>
    purrr::set_names(names(tables_to_split)) |>
    purrr::compact()
}
