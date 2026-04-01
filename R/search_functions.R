# ---- search table ------
search_table <- function(df, search_term, search_cols) {
  if (inherits(df, "tbl_lazy")) {
    pattern <- paste0("%", search_term, "%")
    conditions <- purrr::reduce(
      purrr::map(
        search_cols,
        ~ rlang::expr(as.character(!!rlang::sym(.x)) %ilike% !!pattern)
      ),
      ~ rlang::expr(!!.x | !!.y)
    )
  } else {
    pattern <- search_term
    conditions <- purrr::reduce(
      purrr::map(
        search_cols,
        ~ rlang::expr(
          grepl(!!pattern, as.character(!!rlang::sym(.x)), ignore.case = TRUE)
        )
      ),
      ~ rlang::expr(!!.x | !!.y)
    )
  }

  df <- df |>
    dplyr::filter(!!conditions)

  return(df)
}
