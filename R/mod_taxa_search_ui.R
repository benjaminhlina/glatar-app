taxa_search_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(
    tabName = id,
    shiny::h2("Search Taxa in the Database"),
    shiny::p(
      "Use the search bar to look up the taxa that are in the database.
      This tab servers two purposes 1)
      to let the user know what species exist in the database and 2) when
      uploading new data, the user can
      check validation errors to match what species are in the database."
    ),
    shiny::textInput(
      ns("search_bar"),
      label = "Search Taxa",
      placeholder = "Type to filter taxa..."
    ),
    DT::DTOutput(ns("taxa_table")),
    shiny::h2("Suggest Taxa to the Database"),
    add_taxa_ui(ns = ns, con = con),
    DT::DTOutput(ns("add_taxa"))
  )
}

# ----- server taxa search -----
taxa_search_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {
    filtered_taxa <- create_searching_data(
      con = con,
      tbl_name = "tbl_taxonomy",
      input = input,
      collect = TRUE
    )
    output$taxa_table <- DT::renderDT({
      DT::datatable(
        filtered_taxa(),
        options = list(pageLength = 25, scrollX = TRUE, searching = FALSE),
        rownames = FALSE
      )
    })
  })
}
