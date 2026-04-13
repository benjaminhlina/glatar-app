# ----- get nice names -----
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
