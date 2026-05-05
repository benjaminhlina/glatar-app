# ------- logout -----
#' Logout Server
#'
#'  Provides the logout modal to ask the user if they would like to
#' logout.
#'
#' @param id the shiny namespace id name (i.e., `"logout"`).
#' @param parent_session a shiny server session.
#'
#' @return a modal that asks the user if they really want to logout.
#' This prevents accidental logouts.
#'
#' @name login_modal_server
#' @export

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
