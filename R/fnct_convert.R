# ----- convert nice names -----

#' Convert functions
#'
#' These functions convert columns whether that is column names or
#' the values in the columns themeselves.
#'
#' @param cols column to be converted
#' @param lookup a `vector` that has the name of the column
#' and the nice looking name of the column e.g., `sample_id`
#' becomes `Sample ID`.
#'
#' @name convert_functions
#' @export
convert_nice_name <- function(cols, lookup = nice_name_lookup) {
  converted_name <- unname(sapply(cols, function(col) {
    if (col %in% names(lookup)) {
      lookup[[col]]
    } else {
      col
    }
  }))
  return(converted_name)
}
