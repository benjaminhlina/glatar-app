# ----- filter observserer ----

#' Exclude all observer
#'
#' This function uses an `observerEvent()` to adjust dropdown
#' menus so that when other selections beside `"All"` is selected
#' it will not allow `"All"` to be selected until the entire box
#' is cleared.
#'
#'
#' @param input a server input value.
#' @param session a shiny session.
#' @param id the dropdown object to apply the observer
#'
#' @return an Event that excludes `"All"``
#' @export

exclusive_all_observer <- function(input, session, id) {
  shiny::observeEvent(
    input[[id]],
    {
      sel <- input[[id]]
      if ("All" %in% sel && length(sel) > 1) {
        shiny::updateSelectInput(
          session,
          id,
          selected = setdiff(sel, "All")
        )
      }
    },
    ignoreInit = TRUE
  )
}
