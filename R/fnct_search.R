# ---- search table ------
#' Search functions
#'
#' Search functions allow for a dynamic search bar to be displayed to search
#' taxa and source material
#'
#' @param df the data object being supplied can either be a `tbl_lazy` or `data.frame`.
#' @param search_term a dynamic string that contains search terms
#' @param search_cols a dynamic `vector` containing the names of the search
#' columns

#' @return returns a filtered data object that can then be displayed.
#'
#' @name search_functions
#' @export

search_table <- function(df, search_term, search_cols) {
  terms <- stringr::str_split(search_term, "\\s+")[[1]]
  terms <- terms[nzchar(terms)]

  per_term_conditions <- purrr::map(terms, function(term) {
    if (inherits(df, "tbl_lazy")) {
      pattern <- paste0("%", term, "%")
      conditions <- purrr::reduce(
        purrr::map(
          search_cols,
          ~ rlang::expr(as.character(!!rlang::sym(.x)) %ilike% !!pattern)
        ),
        ~ rlang::expr(!!.x | !!.y)
      )
    } else {
      purrr::reduce(
        purrr::map(
          search_cols,
          ~ rlang::expr(
            grepl(!!term, as.character(!!rlang::sym(.x)), ignore.case = TRUE)
          )
        ),
        ~ rlang::expr(!!.x | !!.y)
      )
    }
  })

  conditions <- purrr::reduce(per_term_conditions, ~ rlang::expr(!!.x & !!.y))

  df <- df |>
    dplyr::filter(!!conditions)

  return(df)
}
