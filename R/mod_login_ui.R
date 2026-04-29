# ----- tab server ------

tab_auth_server <- function(
  input,
  output,
  session,
  credentials,
  sidebar_id = "tabs",
  off = FALSE
) {
  # ---- this alllows me to turn things on and off
  if (isTRUE(off)) {
    return(list(
      auth_state = shiny::reactiveVal(TRUE), # always authenticated
      logout = function() invisible(NULL) # no-op
    ))
  }
  auth_state <- shiny::reactiveVal(FALSE)
  pending_tab <- shiny::reactiveVal(NULL)
  login_failed <- shiny::reactiveVal(FALSE)

  # ------ Intercept tab navigation ------
  shiny::observeEvent(
    input[[sidebar_id]],
    {
      tab <- input[[sidebar_id]]

      if (tab %in% protected_tabs() && !auth_state()) {
        pending_tab(tab)
        login_failed(FALSE)
        shiny::showModal(tab_login_modal())
        shinydashboard::updateTabItems(session, sidebar_id, "home")
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
      dest <- pending_tab()
      pending_tab(NULL)

      shiny::removeModal()

      shinyjs::delay(150, {
        if (!is.null(dest)) {
          shinydashboard::updateTabItems(session, sidebar_id, dest)
        }
      })
    } else {
      login_failed(TRUE)
      shiny::showModal(tab_login_modal(failed = TRUE))
      shiny::updateTextInput(session, "tab_login_pass", value = "")
    }
  })

  # ----- register modal -----
  register_observer(input, output, session, modal = TRUE)

  # ------ Handle cancel ------
  shiny::observeEvent(input$tab_login_cancel, {
    shiny::removeModal()
    shinydashboard::updateTabItems(session, sidebar_id, "home")
  })

  # ------ Logout helper ------
  logout <- function() {
    auth_state(FALSE)
    shinydashboard::updateTabItems(session, sidebar_id, "home")
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
