# ---- clear field -----

#' Clear functions
#'
#' These functions allow fields to be cleared within a server session.
#'
#' @param session session to be cleared
#' @param cols column names
#'
#' @name clear_functions
#' @export

clear_fields <- function(session, cols) {
  text_cols <- cols[cols != "tsn"]
  for (col in text_cols) {
    shiny::updateTextInput(session, col, value = "")
  }
  shiny::updateNumericInput(session, "tsn", value = NA)
}

# ----- clear table -----
#' @param cols column names
#'
#' @name clear_functions
#' @export
clear_table <- function(cols) {
  df <- data.frame(
    matrix(ncol = length(cols), nrow = 0, dimnames = list(NULL, cols)),
    stringsAsFactors = FALSE
  )
  return(df)
}
