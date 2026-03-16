# ----- filter observserer ----
exclusive_all_observer <- function(input, session, id) {
  shiny::observeEvent(
    input[[id]],
    {
      sel <- input[[id]]
      if ("All" %in% sel && length(sel) > 1) {
        shiny::updateSelectInput(
          session,
          id,
          selected = dplyr::setdiff(sel, "All")
        )
      }
    },
    ignoreInit = TRUE
  )
}
