view_data_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = id,
                 shiny::h2("Viewing Selected Table"),
                 shiny::selectInput(ns("table_select"),
                                    "Select a Table",
                                    choices = NULL),
                 DT::DTOutput(ns("selected_table"))
  )
}


# ---- server ----
view_data_server <- function(id, con) {
  moduleServer(id, function(input, output, session) {

    # Get all table names and update selectInput dynamically
    observe({
      table_names <- DBI::dbListTables(con)
      
      table_names <- table_names[-9]
       
      updateSelectInput(session, "table_select", choices = table_names)
    })

    # Render the selected table
    output$selected_table <- DT::renderDT({
     
      allowed_tables <- c(
      "tbl_calorimetry", 
      "tbl_data_dictionary", 
      "tbl_isotope", 
      "tbl_length", 
      "tbl_location", 
      "tbl_proxcomp", 
      "tbl_samples", 
      "tbl_source", 
      "tbl_taxonomy")
      
      req(input$table_select %in% allowed_tables)

      safe_name <- DBI::dbQuoteIdentifier(con, input$table_select)
      
      DBI::dbGetQuery(con, paste0("SELECT * FROM ", safe_name))
    },
    options = list(
      pageLength = 15,
      scrollX = TRUE
    ))
  })
}
