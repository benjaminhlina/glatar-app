logout_server <- function(id, con, main_input) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$tabs, {
      if (input$tabs == "logout") {
        shinydashboard::updateTabItems(session, "tabs", "home")
        shiny::showModal(shiny::modalDialog(
          title = "Confirm Logout",
          "Are you sure you want to log out?",
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              "confirm_logout",
              "Logout",
              class = "btn btn-danger"
            )
          )
        ))
      }
      shiny::observeEvent(input$confirm_logout, {
        shiny::removeModal()
        session$reload()
      })
    })
  })
}
