# ----- tab server ------

tab_auth_server <- function(
  input,
  output,
  session,
  credentials,
  sidebar_id = "tabs"
) {
  auth_state <- shiny::reactiveVal(FALSE)
  pending_tab <- shiny::reactiveVal(NULL)
  login_failed <- shiny::reactiveVal(FALSE)

  # ------ Intercept tab navigation ------
  shiny::observeEvent(
    input[[sidebar_id]], # ← dynamic input ID
    {
      tab <- input[[sidebar_id]]

      if (tab %in% protected_tabs() && !auth_state()) {
        pending_tab(tab)
        login_failed(FALSE)
        shiny::showModal(tab_login_modal())
        shinydashboard::updateTabItems(session, sidebar_id, "home") # ← same ID
      }
    },
    ignoreInit = TRUE
  )

  # ------ Handle login submission ------
  shiny::observeEvent(input$tab_login_submit, {
    valid <- check_tab_credentials(
      input$tab_login_user,
      input$tab_login_pass,
      credentials
    )

    if (valid) {
      auth_state(TRUE)
      login_failed(FALSE)
      shiny::removeModal()

      if (!is.null(pending_tab())) {
        shinydashboard::updateTabItems(session, sidebar_id, pending_tab()) # ← same ID
        pending_tab(NULL)
      }
    } else {
      login_failed(TRUE)
      shiny::showModal(tab_login_modal(failed = TRUE))
      shiny::updateTextInput(session, "tab_login_pass", value = "")
    }
  })

  # ------ Handle cancel ------
  shiny::observeEvent(input$tab_login_cancel, {
    shiny::removeModal()
    shinydashboard::updateTabItems(session, sidebar_id, "home") # ← same ID
  })

  # ------ Logout helper ------
  logout <- function() {
    auth_state(FALSE)
    shinydashboard::updateTabItems(session, sidebar_id, "home") # ← same ID
    shiny::showNotification(
      "You have been logged out.",
      type = "message",
      duration = 3
    )
  }

  list(
    auth_state = auth_state,
    logout = logout
  )
}
