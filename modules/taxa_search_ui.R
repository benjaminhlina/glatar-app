taxa_search_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = id,
    shiny::h2("Search Taxa in the Database"),
    shiny::p(
      "Use the search bar to look up the taxa that are in the database. This tab servers two purposes 1)
      to let the user know what species exist in the database and 2) when uploading new data, the user can 
      check validation errors to match what species are in the database."
    ),
    shiny::textInput(
      ns("search_bar"),
      label = "Search Taxa",
      placeholder = "Type to filter taxa..."
    ),
    DT::DTOutput(ns("taxa_table"))
  )
}
taxa_search_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    filtered_taxa <- shiny::reactive({
      search_cols <- dplyr::tbl(con, "tbl_taxonomy") |>
        colnames()

      df <- dplyr::tbl(con, "tbl_taxonomy") |>
        dplyr::arrange(common_name)

      search_term <- input$search_bar

      # Only filter if the user has typed something
      if (!is.null(search_term) && nzchar(trimws(search_term))) {
        pattern <- paste0("%", search_term, "%")
        conditions <- purrr::reduce(
          purrr::map(
            search_cols,
            ~ rlang::expr(as.character(!!rlang::sym(.x)) %ilike% !!pattern)
          ),
          ~ rlang::expr(!!.x | !!.y)
        )

        df <- df |>
          dplyr::filter(!!conditions)
      }

      df <- df |>
        dplyr::collect()
      current_names <- names(df)

      names(df) <- dplyr::coalesce(
        nice_name_lookup[current_names],
        current_names
      )

      return(df)
    })

    output$taxa_table <- DT::renderDT({
      DT::datatable(
        filtered_taxa(),
        options = list(pageLength = 25, scrollX = TRUE, searching = FALSE),
        rownames = FALSE
      )
    })
  })
}
