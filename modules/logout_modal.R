logout_server <- function(id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(parent_session$input$tabs, {
      if (parent_session$input$tabs == "logout") {
        shinydashboard::updateTabItems(session, "tabs", "home")
        shiny::showModal(shiny::modalDialog(
          title = "Confirm Logout",
          "Are you sure you want to log out?",
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              session$ns("confirm_logout"),
              "Logout",
              class = "btn btn-danger"
            )
          )
        ))
      }
    })

    shiny::observeEvent(input$confirm_logout, {
      shiny::removeModal()
      session$reload()
    })
  })
}
